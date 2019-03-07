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

library(hutils)
library(grattan)
library(ggplot2)
library(plotly)

# From grattanCharts without requiring the package
grattan_dollar <- function (x, digits = 0) {
  nsmall <- digits
  commaz <- format(abs(x), nsmall = nsmall, trim = TRUE, big.mark = ",", 
                   scientific = FALSE, digits = 1L)
  
  if_else(x < 0, 
          paste0("\U2212","$", commaz),
          paste0("$", commaz))
}

sample_file_1516 <- fread(file = "data-raw/2016_sample_file.csv",
                          logical01 = FALSE, 
                          integer64 = "double")

s1516 <- 
  sample_file_1516 %>%
  apply_super_caps_and_div293 %>%
  mutate_ntile(col = "Taxable_Income", n = 10L) %>%
  mutate_ntile(col = "Taxable_Income", n = 100L) %>%
  .[, tax := income_tax(Taxable_Income, 
                        fy.year = "2015-16",
                        .dots.ATO = sample_file_1516)] %>%
  .[, rentalLosses := -pminC(Net_rent_amt, 0)] %>%
  .[, taxNoNG := income_tax(Taxable_Income + rentalLosses, 
                            fy.year = "2015-16",
                            .dots.ATO = .SD)] %>%
  .[]

for (j in seq_along(s1516)) {
  if (is.integer(.subset2(s1516, j))) {
    set(s1516, j = j, value = as.double(.subset2(s1516, j)))
  }
}


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

minima_by_percentile <-
  s1516 %>% 
  select_which(is.numeric) %>%
  drop_col("Ind") %>%
  select_which(isnt_01) %>%
  .[, lapply(.SD, min), keyby = "Taxable_IncomePercentile"]

median_by_decile <-
  s1516 %>% 
  select_which(is.numeric) %>%
  drop_col("Ind") %>%
  select_which(isnt_01) %>%
  .[, lapply(.SD, median), keyby = "Taxable_IncomeDecile"] %>%
  .[, lapply(.SD, round), keyby = "Taxable_IncomeDecile"]

median_by_percentile <-
  s1516 %>% 
  select_which(is.numeric) %>%
  drop_col("Ind") %>%
  select_which(isnt_01) %>%
  .[, lapply(.SD, median), keyby = "Taxable_IncomePercentile"] %>%
  .[, lapply(.SD, round), keyby = "Taxable_IncomePercentile"]

maxima_by_decile <-
  s1516 %>% 
  select_which(is.numeric) %>%
  drop_col("Ind") %>%
  select_which(isnt_01) %>%
  .[, lapply(.SD, max), keyby = "Taxable_IncomeDecile"]

maxima_by_percentile <-
  s1516 %>% 
  select_which(is.numeric) %>%
  drop_col("Ind") %>%
  select_which(isnt_01) %>%
  .[, lapply(.SD, max), keyby = "Taxable_IncomePercentile"]

avg_by_percentile <-
  s1516 %>% 
  select_which(is.numeric) %>%
  drop_col("Ind") %>%
  select_which(isnt_01) %>%
  .[, lapply(.SD, as.double)] %>%
  .[, lapply(.SD, mean), keyby = "Taxable_IncomePercentile"] %>%
  .[, lapply(.SD, round), keyby = "Taxable_IncomePercentile"]

avg_by_decile <-
  s1516 %>% 
  select_which(is.numeric) %>%
  drop_col("Ind") %>%
  select_which(isnt_01) %>%
  .[, lapply(.SD, as.double)] %>%
  .[, lapply(.SD, mean), keyby = "Taxable_IncomeDecile"] %>%
  .[, lapply(.SD, round), keyby = "Taxable_IncomeDecile"]

Percentiles <- 
  s1516 %>% 
  select_which(is.numeric) %>%
  drop_col("Ind") %>%
  select_which(isnt_01) %>%
  .[, lapply(.SD, quantile, prob = (1:99)/100)] %>%
  .[]



setnames <- function(x, old, new) {
  data.table::setnames(x, old = old, new = new, skip_absent = TRUE)
}

style_names <- function(x) {
  setnames(x, "Taxable_Income", "Taxable Income")
  setnames(x, "Taxable_IncomeDecile", "Taxable Income Decile")
  setnames(x, "Sw_amt", "Salary/Wages")
  setnames(x, "taxNoNG", "Tax if no negative gearing")
  setnames(x, "rentalLosses", "Rental losses")
  setnames(x, 
           old = names(x), 
           new = gsub("_([a-z])",
                      " \\U\\1\\E", 
                      gsub("^([a-z])", 
                           "\\U\\1\\E", 
                           names(x),
                           perl = TRUE),
                      perl = TRUE))
  
  x
}

standardize_var_input <- function(the_input, v = "the_vars") {
  x <- the_input[[v]]
  c(if ("SuperContr" %in% x) {
    c("concessional_contributions",
      "non_concessional_contributions")
  },
  x) %>%
    unique %>%
    .[. != "SuperContr"]
}


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
                                     "Rental losses" = "rentalLosses",
                                     "Tax if no negative gearing" = "taxNoNG",
                                     "Net capital gains" = "Net_CG_amt",
                                     "Tax" = "tax"),
                         selected = c("Taxable_Income", "Tax")),
      radioButtons("plot_y", 
                   label = "y-axis plot", 
                   choices = c("Taxable income" = "Taxable_Income",
                               "Salary/wages" = "Sw_amt",
                               "Super contributions" = "SuperContr",
                               "Rental losses" = "rentalLosses",
                               "Tax if no negative gearing" = "taxNoNG",
                               "Net capital gains" = "Net_CG_amt",
                               "Tax" = "tax"),
                   selected = c("Taxable_Income")),
      radioButtons("the_stat", 
                   "Summary statistic", 
                   choices = c("min", "mean", "median", "max"),
                   selected = "mean")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      DT::dataTableOutput("Table1"),
      plotlyOutput("Plot1")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Table1 <- DT::renderDataTable({
    the_variables <- standardize_var_input(input)
    
    TheTable <- 
      switch(input[["the_stat"]],
             "min" = minima_by_decile,
             "median" = median_by_decile,
             "mean" = avg_by_decile,
             "max" = maxima_by_decile) %>%
      selector(cols = the_variables) %>%
      style_names
    
    the_caption <- 
      paste(switch(input[["the_stat"]],
                   "min" = "Minimum",
                   "median" = "Median",
                   "mean" = "Average",
                   "max" = "Maximum"),
            "values by taxable income decile")
    
    DT::datatable(TheTable,
                  rownames = FALSE,
                  options = list(dom = "t"),
                  selection = "none",
                  caption = the_caption) %>%
      DT::formatCurrency(columns = names(TheTable), 
                         digits = 0)
  })
  
  output$Plot1 <- renderPlotly({
    p <- 
      Percentiles %>%
      selector(cols = standardize_var_input(input, "plot_y")) %>%
      .[, Percentile := .I] %>%
      melt.data.table(id.vars = "Percentile",
                      variable.factor = FALSE) %>%
      .[, text := paste0("Percentile: ", Percentile, "\n",
                         "Variable: ", variable, "\n",
                         "Value: $", prettyNum(value, big.mark = ","))] %>%
      .[, variable := gsub("_", " ", variable)] %>%
      .[, variable := sub("^(.)", "\\U\\1\\E", variable, perl = TRUE)] %>%
      .[, value := round(value)] %>%
      ggplot(aes(x = Percentile,
                 y = value, 
                 text = text)) +
      geom_point() + 
      theme(legend.position = c(0, 1),
            legend.justification = c(0, 1)) +
      scale_y_continuous(labels = grattan_dollar)
    p <- 
      if (uniqueN(standardize_var_input(input, "plot_y")) > 1L) {
        p + geom_line(aes(group = variable, 
                      color = variable))
      } else {
        p + geom_line(aes(group = variable)) 
      }
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(x = 0, 
                           y = 1,
                           xanchor = 0,
                           yanchor = 1))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
