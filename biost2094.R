library(shiny)
library(dplyr)
library(survival)
library(ggplot2)

direct_adjusted_mortality_rate <- function(data, breaks, labels, standard_population_rate) {
  
  #Create age group based on breaks and labels, right=FALSE allows infinite
  data$age_group <- cut(data$age, breaks = breaks, labels = labels, 
                        include.lowest = TRUE, right = FALSE)
  
  #Calculate total deaths and total population in each subgroup
  subgroup <- data %>%
    group_by(age_group) %>%
    summarise(
      total_deaths = sum(death),    #total deaths 
      total_population = n(),       #total population  
      mortality_rate = total_deaths / total_population #mortality rate
    )
  
  #add age-adjusted mortality rate
  adjusted_rates <- subgroup %>%
    mutate(
      adjusted_mortality_rate = mortality_rate * standard_population_rate[as.character(age_group)]
    )

  return(adjusted_rates)

}


indirect_adjusted_mortality_rate <- function(data, breaks, labels, standard_population_rate) {

  #Create age group based on breaks and labels, right=FALSE allows infinite
  data$age_group <- cut(data$age, breaks = breaks, labels = labels, 
                        include.lowest = TRUE, right = FALSE)
  
  #Calculate total deaths and total population in each subgroup
  subgroup <- data %>%
    group_by(age_group) %>%
    summarise(
      total_deaths = sum(death),    #total deaths 
      total_population = n(),       #total population  
    )
  
  subgroup$expected_deaths <- subgroup$total_population * standard_population_rate[as.character(subgroup$age_group)]
  
  subgroup$SMR <- subgroup$total_deaths / subgroup$expected_deaths
  
  return(subgroup)

}

life_table <- function(data, initial_population = 100000) {
  
  # calculate total death/population and live people for each age
  summary_data <- data %>%
    group_by(age) %>%
    summarise(
      deaths = sum(death),          # total death
      population = n(),             # total population
      alive = population - deaths   # number of alive
    ) %>%
    arrange(age)                    #order age for the plots
  
  # calculate lx, dx, qx
  summary_data <- summary_data %>%
    mutate(
      lx = initial_population * cumprod(alive / population),  #number of survival for each age
      dx = lx * (deaths / population),     #number of deaths for each age                   
      qx = deaths / population          #death rate for each age               
    )
  
  # calculate Lx, Tx, ex
  summary_data <- summary_data %>%
    mutate(
      #the number of person-years lived for alive people in this year
      Lx = lx - lead(lx, default = 0),        
      #the total number of person-years lived from a given age until the end of the life table.
      Tx = cumsum(Lx),                        
      #the expected remaining lifetime for alive individuals at the beginning of this year
      ex = Tx / lx                             
    )
  
  return(summary_data)
}


ui <- fluidPage(
  titlePanel("Age-Adjustment Mortality Rate Calculation and Life Table Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("analysis_type", "Select Analysis Type:",
                  choices = c("Choose", "Age-Adjustment Mortality Rate", "Life Table")),
      conditionalPanel(
        condition = "input.analysis_type === 'Age-Adjustment Mortality Rate'",
        selectInput("adjustment_type", "Select Age-Adjustment Type:",
                    choices = c("Choose", "Direct", "Indirect")),
        textInput("breaks", "Enter Age Breaks:", value = "0,20,40,60,Inf"),
        textInput("labels", "Enter Age Labels:", value = "0-19,20-39,40-59,60+"),
        textInput("pop_rates", "Enter Standard Population Rates:", value = "0.25,0.25,0.25,0.25")
      ),
      conditionalPanel(
        condition = "input.analysis_type === 'Life Table'",
        numericInput("initial_population", "Initial Population:", value = 100000, min = 1)
      ),
      fileInput("data", "Upload your data:", accept = c(".csv", ".txt")),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", tableOutput("results")),
        tabPanel("Survivor (lx)", plotOutput("plot_lx")),
        tabPanel("Deaths (dx)", plotOutput("plot_dx")),
        tabPanel("Life Expectancy (ex)", plotOutput("plot_ex"))
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$submit, {
    req(input$analysis_type, input$data)
    
    data <- read.csv(input$data$datapath)
    
    if (input$analysis_type == "Age-Adjustment Mortality Rate") {
      req(input$adjustment_type, input$breaks, input$labels, input$pop_rates)
      
      breaks <- as.numeric(unlist(strsplit(input$breaks, ",")))
      labels <- unlist(strsplit(input$labels, ","))
      pop_rates <- as.numeric(unlist(strsplit(input$pop_rates, ",")))
      standard_population_rate <- setNames(pop_rates, labels)
      
      if (input$adjustment_type == "Direct") {
        results <- direct_adjusted_mortality_rate(data, breaks, labels, standard_population_rate)
      } else {
        results <- indirect_adjusted_mortality_rate(data, breaks, labels, standard_population_rate)
      }
      output$results <- renderTable({results})
      
    } else if (input$analysis_type == "Life Table") {
      req(input$initial_population)
      
      # get initial_population
      initial_pop <- as.numeric(input$initial_population)
      life_table_data <- life_table(data, initial_pop)
      output$results <- renderTable({life_table_data})
      
      output$plot_lx <- renderPlot({
        ggplot(life_table_data, aes(x = age, y = lx)) +
          geom_line(color = "blue") +
          labs(title = "lx: Number of survivors by age", x = "Age", y = "lx")
      })
      output$plot_dx <- renderPlot({
        ggplot(life_table_data, aes(x = age, y = dx)) +
          geom_bar(stat = "identity", fill = "red") +
          labs(title = "dx: Number of deaths by age", x = "Age", y = "dx")
      })
      output$plot_ex <- renderPlot({
        ggplot(life_table_data, aes(x = age, y = ex)) +
          geom_line(color = "green") +
          labs(title = "ex: Life expectancy from age", x = "Age", y = "ex")
      })
    }
  })
}

shinyApp(ui, server)

#generate a dataset for testing
#set.seed(123)  
#ages <- sample(0:100, 1000, replace = TRUE)
#deaths <- rbinom(1000, 1, prob = ages/200)
#random_dataset <- data.frame(
#  age = ages,
#  death = deaths
#)

#write.csv(random_dataset,paste0("D:/GSR/EDEN/sample.csv"), row.names = FALSE)
