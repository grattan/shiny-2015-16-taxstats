#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(data.table)
library(magrittr)
library(SampleFile1516)
library(hutils)
library(grattan)
library(plotly)

s1516 <- 
  as.data.table(sample_file_1516) %>%
  apply_super_caps_and_div293 %>%
  mutate_ntile(col = "Taxable_Income", n = 10L) %>%
  .[, tax := income_tax(Taxable_Income, 
                        fy.year = "2015-16",
                        .dots.ATO = sample_file_1516)] %>%
  .[]


isnt_01 <- function(x) {
  if (max(x) == 1L &&
      min(x) == 0L) {
    return(FALSE)
  }
  TRUE
}

minima_by_decile <-
  s1516 %>% 
  select_which(is.numeric) %>%
  drop_col("Ind") %>%
  select_which(isnt_01) %>%
  .[, lapply(.SD, min), keyby = "Taxable_IncomeDecile"]

avg_by_decile <-
  s1516 %>% 
  select_which(is.numeric) %>%
  drop_col("Ind") %>%
  select_which(isnt_01) %>%
  .[, lapply(.SD, as.double)] %>%
  .[, lapply(.SD, mean), keyby = "Taxable_IncomeDecile"] %>%
  .[, lapply(.SD, round), keyby = "Taxable_IncomeDecile"]




library(shiny)
library(DT)



ui <- fluidPage(
  
  # Application title
  titlePanel("2015-16 taxstats"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("the_vars",
                         "Variables to include:",
                         choices = c("Taxable income" = "Taxable_Income",
                                     "Salary/wages" = "Sw_amt",
                                     "Super contributions" = "SuperContr",
                                     "Tax" = "tax"),
                         selected = c("Taxable_Income", "Tax"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      DT::DTOutput("Table1")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Table1 <- DT::renderDataTable({
    the_variables <-
      c(if ("SuperContr" %in% input[["the_vars"]]) {
        c("concessional_contributions",
          "non_concessional_contributions")
      },
      input[["the_vars"]]) %>%
      unique %>%
      .[. != "SuperContr"]
    
    avg_by_decile %>%
      selector(cols = the_variables)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
