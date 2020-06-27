# LIBRARIES ------------------------------------
library(tidyverse)
theme_set(theme_classic())
library(maps)
library(scales)

# IMPORT DATA ---------------------------------
USA_education_data_final <- read.csv("USA_education_data_final.csv")
USA_education_data_total <- subset(USA_education_data_final, 
                                   USA_education_data_final$State %in% "USA")
state_scores <- read.csv("state_scores.csv",fileEncoding="UTF-8-BOM")
names(state_scores)[names(state_scores) == "state"] <- "abb"

# SET UP --------------------------------------
windowsFonts("Arial" = windowsFont("Arial"))
plotTheme <- theme(plot.title = element_text(family = "Arial", face = "bold", size = (15), hjust=0.5), 
                   legend.title = element_text(family="Arial", size=(12)), 
                   legend.text = element_text(family="Arial", size=(10)), 
                   axis.title = element_text(family = "Arial", size = (14)),
                   axis.text = element_text(family = "Arial", size = (10))
)
device             <- c("device_always", "device_usually", "device_sometimes", "device_rarely", "device_never", "device_nr")
internet           <- c("internet_always", "internet_usually", "internet_sometimes", "internet_rarely", "internet_never", "internet_nr")
characteristic_all <- c("computer_provider", "internet_provider", "unemployed", "resp_employment", "food", "income")
type               <- c("device", "internet")
availability_all   <- c("always", "usually", "sometimes", "rarely", "never")
computer_provider  <- c("comp_nr", "comp_school", "comp_home", "comp_other" )
internet_provider  <- c("int_nr", "int_school", "int_home", "int_other")
unemployed         <- c("unemployed_nr", "yes_unemployed", "no_unemployed")
resp_employment    <- c("resp_employed_nr", "resp_employed_yes", "resp_employed_no")
food               <- c("food_nr",
                        "food_enough_want", "food_enough_notwant", "food_sometimes", "food_notenough")
income             <- c("income_nr","25000>", "25000-34999", "35000-49999", "50000-74999", "75000-99999", "100000-149999", 
                        "150000-199999", "200000<")
labels_each <- c("No response",
                 "School", "Home", "Other",
                 "Yes", "No",
                 "Yes", "No",
                 "Enough of desired food type", "Enough food, but not always\nof desired type", "Sometimes enough food", "Often not enough food",
                 "Less than $25,000", "$25,000-$34,999", "$35,000-$49,999", "$50,000-$74,999", "$75,000-$99,999", "$100,000-$149,999",
                 "$150,000-$199,999", "Over $200,000")
state_geographies <- map_data("state") %>%
  as_tibble()

# FUNCTION DEFINITIONS ------------------------
capatalize <- function(str) {
  substr(str, 1, 1) <- toupper(substr(str, 1, 1))
  return(str)
}

char_subset <- function(characteristic){
  ##char_subset() is a function to subset the entire data frame by characteristic
  ##input : characteristic - a variable
  ##output : data frame of subset data frame only on the characteristic
  df <- subset(USA_education_data_final, 
               USA_education_data_final[[2]] %in% characteristic)
  return(df)
}

map_subset <- function(type, weeknum){
  ##map_subset() is a function to subset data state_scores by device/internet availability
  ##input : type - string of either "device" or "internet"
  ##        week - number between 1 and 4
  ##output : data frame of subset data of average score device/internet availability in chosen week
  if(type == "device"){
    df_type <- state_scores[c(1, 2, 3)]
  }
  else if(type == "internet"){
    df_type <- state_scores[c(1,2,4)]
  }
  
  df <- as.data.frame(subset(df_type, df_type$week == weeknum))
  return(df)
}

map_availability <- function(type, weeknum){
  ##map_availability() is a function that produces a us map of internet/device availability average score 
  ##input : type - string of either "device" or "internet"
  ##        week - number between 1 and 4
  ##output : us map
  state_values <- map_subset(type = type, weeknum = weeknum)
  state_abbreviations <- tibble(
    state = state.name,
    abb = state.abb
  )
  # join your data frame with lookup table
  state_values <- state_values %>%
    left_join(state_abbreviations, by = "abb") %>%
    # make state names lowercase to match state_geographies
    mutate(state = tolower(state))
  # join with geography info
  states <- left_join(state_geographies, state_values, by = c("region" = "state"))
  # map!
  if(type == "device"){fact <- states$device_average_score}
  else if(type == "internet") {fact <- states$internet_average_score}
  legendName <- capatalize(type) #format legend title
  plt <- ggplot(states, aes(x = long, y = lat, group = group, fill = fact)) + 
    geom_polygon() +
    scale_fill_viridis_c(option = "magma") +
    theme_void() +
    labs(fill = paste(legendName, "availability score", sep = " "))
  return(plt)
}

stacked_bar <- function(characteristic, type, availability, display){
  ##stacked_bar() is a function to return a stacked bar graph of chosen availability of internet/device by 
  ##chosen characteristic
  ##input : characteristic - a string of the following: "computer_provider", "internet_provider", 
  ##                         "unemployed",  "resp_employment", "food", "income" 
  ##        type - string of either "device" or "internet"
  ##        availability - string of either "always" "usually", "sometimes", "rarely", "never"
  ##        display - string of either "raw" or "percent"
  ##output : ggplot of stacked bar graph
  if (characteristic == "computer_provider")
  {
    use <- computer_provider
    labs <- labels_each[1:4]
    legend_t <- "Device provider" #legend title
  }
  else if (characteristic == "internet_provider")
  {
    use <- internet_provider
    labs <- labels_each[1:4]
    legend_t <- "Internet provider"
  }
  else if (characteristic == "unemployed")
  {
    use <- unemployed
    labs <- labels_each[c(1,5,6)]
    legend_t <- "Respondent or household\nmember experienced loss\nof employment income"
  }
  else if (characteristic == "resp_employment")
  {
    use <- resp_employment
    labs <- labels_each[c(1,7,8)]
    legend_t <- "Respondent \ncurrently employed"
  }
  else if (characteristic == "food")
  {
    use <- food
    labs <- labels_each[c(1, 9:12)]
    legend_t <- "Food sufficiency for households \nprior to March 13, 2020"
  }
  else if (characteristic == "income")
  {
    use <- income
    labs <- labels_each[c(1, 13:20)]
    legend_t <- "Household income"
  }
  df <- char_subset(use)
  df[2] <- factor(df[[2]], levels = use) 
  if(display == "percent"){
    pos <- "fill"
    tit <- paste("Percentage of respondents with", type, 
                        availability, "\navailable for educational purposes", sep = " ")
    axis <- percent
  }
  else if (display == "raw"){
    pos <- "stack"
    tit <- paste("Number of respondents with", type,
                 availability, "\navailable for educational purposes", sep = " ")
    axis <- scales::comma
  }
  if(type == "device"){
    index = match(availability, availability_all)
    if (index == 1){fact <- df$device_always}
    else if (index == 2){fact <- df$device_usually}
    else if (index ==3){fact <- df$device_sometimes}
    else if (index ==4){fact <- df$device_rarely}
    else if (index == 5){fact <- df$device_never}
  }
  else if(type == "internet" ){
    index = match(availability, availability_all)
    if (index == 1){fact <- df$internet_always}
    else if (index == 2){fact <- df$internet_usually}
    else if (index ==3){fact <- df$internet_sometimes}
    else if (index ==4){fact <- df$internet_rarely}
    else if (index == 5){fact <- df$internet_never}
  }
  plt <- ggplot(df, aes(fill=df[[2]], 
                        y=fact, 
                        x=df$Week)) + 
    geom_bar(position=pos, stat="identity")+
    xlab("Week") + 
    ylab(tit)+
    scale_y_continuous(labels = axis)+
    guides(color = guide_legend(override.aes = list(size=5)))+
    plotTheme+
    scale_fill_manual(values = c("#1b6ca8ff", "#5fdde5ff", "#f0e36aff",
                                 "#f37121ff","#d92027ff","#a8df65ff",
                                 "#67ae74ff","#17706eff","#509492ff"),
                      name= legend_t,
                      labels = labs)
  return(plt)
}

# SHINY APP -----------------------------------

# Define UI
ui <- fluidPage(
  navbarPage( title="The Data Quails",
    #Welcome tab
    tabPanel("Welcome", fluid=TRUE,
             
      fluidRow(column(8, offset=2, h1("Welcome!", align="center"),
                      img(src="cover_photo_thin.png", width="100%")),
        column(8, offset = 2, br(),
      p(style="font-size: 16px; text-align: justify;","This app allows you to investigate the societal impacts of the COVID-19 
        pandemic on education in the United States. The data presented are taken 
        from surveys conducted by the US Census Bureau, showing the availability 
        of devices and internet in households with children in public or private 
        schools in the US over a period of four weeks (23 April - 26 May 2020)."),
      p(style="font-size: 16px; text-align: justify;","This app was created as an entry for the 2020", a(href="https://datafest-edi.github.io/web/","DataFest"), 
      "competition at the University of Edinburgh. 
      Find out more about the designers by clicking the 'About us' tab."),
      p(style="font-size: 16px; text-align: justify;", "The 'Map' tab shows the survey data across the US by state, giving you an 
        overview of how available internet and devices for educational purposes 
        are in the average household for each state. The 'Bar graph' tab shows how 
        household characteristics interact with the availability of devices and 
        internet for school-going children. You are encouraged to interact with 
        the data to find trends and investigate how people have responded to the 
        pandemic over time."),
      hr(),
      h3("Why this matters", align="center"),
      p(style="font-size: 16px; text-align: justify;", "The COVID-19 pandemic resulted in the US government enforcing lockdown 
        measures and social distancing, forcing many schools to close. Schools 
        have thus shifted to teaching students remotely, making student’s access 
        to the internet vital for their education. Hence, we chose to investigate 
        student’s access to the internet and devices for learning purposes as 
        education continues to shift online. An analysis of student’s access to 
        these two resources can provide valuable insight into understanding how 
        our traditional models of education have transformed considering the 
        COVID-19 pandemic. It remains true that these resources are not universally 
        accessible and that socio-economic factors such as income or food sufficiency 
        can correlate with whether students have 
        access to these resources."),
      hr(),
      h3("Survey responses", align="center"),
      p(style="font-size: 16px; text-align: justify;", "The survey asks relevant households to report student’s access to two 
        primary resources: access to computers and access to the internet. 
        Respondents are asked to state whether students always, usually, 
        sometimes, rarely, or never have access to computers and the internet. The total number of respondents varied
        over the four weeks."),
      hr(),
      h3("Our data", align="center"),
      p(style="font-size: 16px; text-align: justify;", "Our dataset is sourced from the US Census Bureau’s", 
      a(href="https://www.census.gov/householdpulsedata","Household Pulse 
      Survey"), "whose aim is to collect data concerning social and economic 
      impacts on US households due to the COVID-19 pandemic. This survey is designed to quickly 
      and efficiently collect and present information."),
      p(style="font-size: 16px; text-align: center;", "Week 1: 23 April - 05 May",br(),
      "Week 2: 07 May - 12 May",br(),
      "Week 3: 14 May - 19 May",br(),
      "Week 4: 21 May - 26 May"),
      hr(),
      h3("Terminology", align="center"),
      p(style="font-size: 16px; text-align: justify;", "Device is defined as the following: Any computer, tablet, phone, or 
        other electronic appliance which has the capability to connect to the 
        internet for educational purposes."),
      hr(),
      h3("References", align="center"),
      p(style="font-size: 16px; text-align: justify;", "U.S. Census Bureau (2020). Measuring Household Experiences 
      during the Coronavirus (COVID-19) Pandemic. Retrieved from 
      [https://www.census.gov/householdpulsedata]."),
      br(),
      p(align="center", "Cover image by", 
        a(href="https://pixabay.com/users/GDJ-1086657/?utm_source=link-attribution&amp;utm_medium=referral&amp;utm_campaign=image&amp;utm_content=3846597", "Gordon Johnson"), 
        "from", a(href="https://pixabay.com/?utm_source=link-attribution&amp;utm_medium=referral&amp;utm_campaign=image&amp;utm_content=3846597", "Pixabay")
      )
        ))),
              
    #Map tab
    tabPanel("Map", fluid=TRUE, sidebarLayout(
      sidebarPanel(width=3,
        radioButtons(inputId = "mapType", 
          label="Availability type:",
          choices = c("Device"="device", "Internet"="internet"),
          selected = "device"
        ),
        sliderInput(inputId = "mapWeek",
          label="Week:",
          min = 1,
          max = 4,
          value = 1,
          step = 1,
        ),
        h4("Note on availability score:"),
        helpText("The availability score was calculated by 
                 assigning numerical values between 1-5, where 
                 1 represents the participant never had access 
                 and 5 represents the participant always has access. 
                 We take the average response of each state’s 
                 participants and calculate the average state score 
                 which is shown in the map.")
      ),
      mainPanel(titlePanel("Average Availability Scores Across US States"),
        plotOutput("map")
      )
    )
  ),
              
    #Barplot tab
    tabPanel("Bar graph", fluid=TRUE, sidebarLayout(
      sidebarPanel(width=3,
        radioButtons(inputId="barplotType",
          label="Availability type:",
          choices=c("Device"="device","Internet"="internet"),
          selected="device"
         ),
        radioButtons(inputId="barplotAvail",
          label="Select availability:",
          choices=c("Always"="always","Usually"="usually", "Sometimes"="sometimes","Rarely"="rarely","Never"="never"),
          selected="always"
        ),
        selectInput(inputId="barplotData",
          label="Select data to be plotted:",
            choices=c("Device provided by"="computer_provider",
            "Internet provided by"="internet_provider",
            "Experienced loss of income"="unemployed",
            "Currently employed"="resp_employment",
            "Food sufficiency"="food",
            "Household income"="income"),
            selected="computer_provider"
        ),
        radioButtons(inputId="barplotDisplay",
          label="Select display types:",
          choices=c("Percentage"="percent","Number of respondents"="raw"),
          selected="percent")
      ),#close sidebarPanel
      mainPanel(
        titlePanel("Device and Internet Availability by Characteristic"),
        plotOutput("barplot", width="100%"),
        br(),br(),
        h4("Notes on characteristic variables:"),
        h5(style="font-weight: bold;","Device provided by"),
        p("Respondents were asked who provided the student’s learning device: school, household, or some other source."),
        h5(style="font-weight: bold;","Internet provided by"),
        p("Respondents were asked about the source of the student’s main internet access: school, household, or some other source."),
        h5(style="font-weight: bold;","Experienced loss of income"),
        p("Respondents were asked whether they had experienced a loss of employment income, or if they anticipated a loss of income in the upcoming four weeks from taking the survey."),
        h5(style="font-weight: bold;","Current employment status"),
        p("Respondents were asked whether they were currently employed: yes or no."),
        h5(style="font-weight: bold;","Food sufficiency"),
        p("Respondents were asked about their food sufficiency prior to 13 March 2020: enough of the types of food wanted; enough food, but not always the types wanted; sometimes not enough to eat; often not enough to eat."),
        h5(style="font-weight: bold;","Household income"),
        p("Respondents were asked about their annual household income which was grouped into eight categories.")
      )
    )
    ),
  #About us tab
  tabPanel("About us", fluid=TRUE, fluidRow(
    h1("The Data Quails", align="center"),
    br(),br(),
    column(4,
      img(src='claire.png', height="50%", width="50%", align="center"),
      p(br(), strong("Name:"), "Claire Squires", br(),
        strong("Degree:"), "BSc (Hons) Cognitive Science", br(),
        strong("Hometown:"), "Cape Town, South Africa", br(),
        strong("Favourite university building:"), "Library Bar, Teviot"
      )
    ),
    column(4,
      img(src='jaden.png', height="50%", width="50%", align="center"),
      p(br(),strong("Name:"), "Jaden Kimura", br(),
        strong("Degree:"), "MA (Hons) Philosophy and Mathematics", br(),
        strong("Hometown:"), "Los Angeles, California, USA", br(),
        strong("Favourite university building:"), "Teviot"
      )  
    ),
    column(4,
      img(src='kaori.png', height="50%", width="50%", align="center"),
      p(br(),strong("Name:"), "Kaori Shimizu", br(),
      strong("Degree:"), "BSc (Hons) Mathematics and Statistics", br(),
      strong("Hometown:"), "Earth", br(),
      strong("Favourite university building:"), "James Clerk Maxwell Building"
      )  
    ),
    column(2, offset=5,
           br(), br(),
    img(src="three_quails.jpg", width="100%")
    ),
    column(12, 
           br(), br(),
    p(align="center", "Avatars from", a(href="https://getavataaars.com/", "Avataaars")),
    p(align="center", "Qauils from", a(href="https://www.vecteezy.com/free-vector/bird", "Vecteezy"))
    )
  )
  )
  )
  )

# Define server
server <- function(input, output) {
  #Map tab
  output$map <- renderPlot({
    map_availability(input$mapType, input$mapWeek)
  })
  
  #Barplot tab
  output$barplot <- renderPlot({
    stacked_bar(input$barplotData, input$barplotType, input$barplotAvail, input$barplotDisplay)
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)