#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(shinythemes)
library(tidyverse)
library(plotly)
library(DT)
library(patchwork)
library(ggpubr)

# x = readr::read_tsv("data2x02-survey/data/clean.tsv")
x = readr::read_tsv("data/clean.tsv")

x = x %>% select(-c("height", "gender", "social_media", "spain_budget"))

continuous_vars = c("height_clean", "spain_budget_clean", "feel_overseas", 
                    "feel_anxious", "study_hrs", "random_number", "exercise_hrs", 
                    "employment_hrs", "weekly_saving", "weeks_behind", "team_role", 
                    "data2x02_hrs", "social_media_hrs", "wam", "shoe_size")
categorical_vars = c("gender_clean", "covid_positive", "living_arrangements", 
                     "uni_travel_method", "uni_travel_listen", "read_news", "study_load", 
                     "work", "lab_zoom", "steak_preference", "dominant_hand", 
                     "normal_advanced", "city", "hourly_plan", "assignment_on_time", 
                     "used_r_before", "uni_year", "sport", "decade_selection", "social_media_clean")

plot_types = c("Boxplot", "Comparative Boxplot", "Barplot", "Scatter Plot")

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "sandstone"),
  
  navbarPage("DATA2X02 Survey",
             tabPanel("Home",
                      htmlOutput("home")
             ),
             
             tabPanel("Data Table",
                      h1("Data table"),
                      DT::dataTableOutput("data2x02_table")
             ),
             
             tabPanel("Visualisations",
                sidebarLayout(
                  sidebarPanel(
                    selectInput(
                      inputId = "plotType",
                      label = "Choose a Type of Plot:",
                      choices = plot_types),
                    conditionalPanel(
                      condition = "input.plotType == 'Boxplot'",
                      selectInput(inputId = "numericVar1",
                                  label = "Choose Numeric Variable:",
                                  choices = continuous_vars)
                    ),
                    conditionalPanel(
                      condition = "input.plotType == 'Comparative Boxplot'",
                      selectInput(inputId = "numericVar2",
                                  label = "Choose Numeric Variable:",
                                  choices = continuous_vars),
                      selectInput(inputId = "comparativeVar",
                                  label = "Choose Comparative Variable:",
                                  choices = categorical_vars)
                    ),
                    conditionalPanel(
                      condition = "input.plotType == 'Barplot'",
                      selectInput(inputId = "categoricalVar",
                                  label = "Choose Categorical Variable:",
                                  choices = categorical_vars)
                    ),
                    conditionalPanel(
                      condition = "input.plotType == 'Scatter Plot'",
                      selectInput(inputId = "x",
                                  label = "X Axis Variable:",
                                  choices = continuous_vars),
                      selectInput(inputId = "y",
                                  label = "Y Axis Variable:",
                                  choices = continuous_vars),
                      selectInput(inputId = "color",
                                  label = "Coloring Variable",
                                  choices = categorical_vars),
                      selectInput(inputId = "facet",
                                  label = "Faceting variable",
                                  choices = categorical_vars),
                    ),
                    
                  ),
                  mainPanel(
                    textOutput("vizTitle"),
                    plotly::plotlyOutput("plot")
                  )
                )
             ),
             tabPanel("Goodness of Fit Tests",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("nullDist", "Select H0 distribution:",
                               selected = "uniform",
                               choices = c("uniform")
                   ),
                   selectInput("catVar1", "Select Catagorical Variable:",
                               selected = "gender_clean",
                               choices = categorical_vars
                   ),
                   numericInput("sigLvl", "Significant Level:",
                                value = 0.05, min = 0, max = 1, step = 0.01
                   ),
                 ),
                 
                 mainPanel(
                   h2("Hypothesis"),
                   htmlOutput("GoFTHypo"),
                   h2("Frequency table"),
                   tableOutput("observedCounts"),
                   h2("Assumptions"),
                   p("We assume all observations are independent."),
                   htmlOutput("GoFTAssumption"),
                   
                 )
               )
             ),
             tabPanel("T-tests",
               sidebarLayout(
                 sidebarPanel(width = 3,
                   selectInput("tTestType", "Choose T-test Type:",
                               selected = "One Sample",
                               choices = c("one-sample", "paired", "two-sample")
                               ),
                   selectInput("numVar1", "Numeric Variable 1:",
                               selected = "height_clean",
                               choices = continuous_vars),
                   conditionalPanel(
                     condition = "input.tTestType=='one-sample'",
                     numericInput("nullMean", "Enter H0 Mean:", value = 0)
                   ),
                   conditionalPanel(
                     condition = "input.tTestType=='paired'",
                     selectInput("numVar2", "Numeric Variable 2:",
                                 selected = "height_clean",
                                 choices = continuous_vars)
                   ),
                   conditionalPanel(
                     condition = "input.tTestType=='two-sample'",
                     selectInput("splitVar", "Catagorical Variable to Split the Data:",
                                 selected = "gender_clean",
                                 choices = categorical_vars),
                     uiOutput("twoSample"),
                     checkboxInput("varEq", "Variances are Equal:",
                                   value = TRUE)
                   ),
                   
                   selectInput("altrHypo", "Choose Alternative Hypothesis:",
                               selected = "two.sided",
                               choices = c("two.sided", "greater", "less")
                   ),
                   numericInput("sigLvl2", "Significant Level:",
                                value = 0.05, min = 0, max = 1, step = 0.01
                   ),
                   
                   
                 ),
                 mainPanel(width = 9,
                   h2("Hypothesis"),
                   htmlOutput("tTestHypo"),
                   h2("Assumptions"),
                   p("We assume all observations are independent."),
                   p("Check the normality of the data using boxplot and Q-Q plot:"),
                   plotlyOutput("box"),
                   plotlyOutput("qq"),
                   htmlOutput("tTestOutcome"),
                   
                 )
               )
             ),
  ),
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #home page
  output$home <- renderUI({
    title = h1("Welcome")
    p1 = p("This is a Shiny web app for exploring 2022 semester 2 DATA2X02 survey data.", style = "font-size: 20px")
    p2 = p("There are 5 pages withing this app: home, data table, visualisations, goodness of fit tests, and t-tests. You can access each page by clicking the link in the navigation bar. Let's explain what you can do in each page!", style = "font-size: 20px")
    list = tags$ul(style = "font-size: 20px",
      tags$li("Data Table: View the survey data"),
      tags$li("Visualisation: barplot, comparative barplot, boxplot, and scatter plot"),
      tags$li("Goodness of Fit Tests: chi-square goodness of fit test for uniform distribution"),
      tags$li("T-test: one-sample, paired, and two-sample"),
    )
    HTML(paste(title,p1,p2,list))
  })
  
  # data table
  output$data2x02_table <- DT::renderDataTable({
    x
  })
  
  # Visualisation
  output$vizTitle <- renderText({
      input$plotType
  })
  
  output$plot <- plotly::renderPlotly({
    if (input$plotType == "Boxplot") {
      x %>% ggplot(aes(y = .data[[input$numericVar1]])) + 
        geom_boxplot(color = "skyblue3", outlier.color = "black", fill = "skyblue", outlier.fill = "black")
        
    } else if (input$plotType == "Comparative Boxplot") {
      x %>% ggplot(aes(y = .data[[input$numericVar2]], 
                       x = .data[[input$comparativeVar]],
                       color = .data[[input$comparativeVar]])) +
        geom_boxplot() + 
        theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
    } else if (input$plotType == "Barplot") {
      x %>% ggplot(aes(x = .data[[input$categoricalVar]],
                       fill = .data[[input$categoricalVar]])) +
        geom_bar() + 
        theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
    } else if (input$plotType == "Scatter Plot") {
      x %>% ggplot(aes(x = .data[[input$x]], 
                       y = .data[[input$y]],
                       color = .data[[input$color]])) +
        geom_point() +
        facet_grid (cols = vars(.data[[input$facet]]))
    }
  })
  
  # Goodness of Fit Test
  output$GoFTHypo <- renderUI({
    null <- paste("H0:", strong(input$catVar1), "follows a", strong(input$nullDist), "a distribution.")
    altr <- paste("H1:", strong(input$catVar1), "does not follow a", strong(input$nullDist), "a distribution.")
    HTML(paste(null, altr, sep = '<br/>'))
  })
  output$observedCounts <- renderTable({
    x %>% select(input$catVar1) %>% table()
  })
  output$GoFTAssumption <- renderUI({
    test = reactive({
      t = x %>% pull(input$catVar1) %>%
        table() %>% chisq.test()
      bool = F %in% t$expected >= 5
      
      return (t)
    })
    t = test()
    bool = !(F %in% t$expected >= 5)
    if (bool) {
      assumption <- "Also, all expected cell counts are greater than or equal to 5. Thus, the assumptions has been satisfied."
    } else {
      assumption <- "But, not all expected cell counts are greater than or equal to 5. Thus the assumptions has not been satisfied."
    }
    # conclusion
    if (t$p.value < input$sigLvl) {
      conclusion <- paste("Since p-value <", input$sigLvl, ", we reject H0. That is, the data does not follow", input$nullDist, "distribution.")
    } else {
      conclusion <- paste("Since p-value >=", input$sigLvl, ", we do not reject H0. That is, the data follows", input$nullDist, "distribution.")
    }
    HTML(paste(assumption, "<h2>Observed test statistic</h2>", 
               "t0 =", round(t$statistic, 3),
               "<h2>p-value</h2>",
               "p-value = ", signif(t$p.value, 3),
               "<h2>Conclusion</h2>",
               conclusion
               )
         )
  })
  
  # T-test
  output$twoSample <- renderUI({
    if (input$tTestType == "two-sample") {
      tagList(
        selectInput("cat1", "Sample 1:",
                    choices = x %>% select(input$splitVar) %>% unique()),
        selectInput("cat2", "Sample 2:",
                    choices = x %>% select(input$splitVar) %>% unique())
      )
    }
  })
  output$tTestHypo <- renderUI({
    switch (input$tTestType,
      "one-sample" = {
        s <- paste("Let the mean of", input$numVar1, "be μ.")
        null <- paste("H0: μ =", input$nullMean)
        switch (input$altrHypo,
          "two.sided" = {
            altr <- paste("H1: μ ≠ ", input$nullMean)
          },
          "greater" = {
            altr <- paste("H1: μ > ", input$nullMean)
          },
          "less" = {
            altr <- paste("H1: μ < ", input$nullMean)
          }
        )
        HTML(paste(s, null, altr, sep = '<br/>'))
      },
      "paired" = {
        s <- paste("Let the mean of", input$numVar1, "and", input$numVar2, "be μ1 and μ2.")
        null <- paste("H0: μ1 = μ2")
        switch (input$altrHypo,
                "two.sided" = {
                  altr <- paste("H1: μ1 ≠ μ2")
                },
                "greater" = {
                  altr <- paste("H1: μ1 > μ2")
                },
                "less" = {
                  altr <- paste("H1: μ1 < μ2")
                }
        )
        HTML(paste(s, null, altr, sep = '<br/>'))
      },
      "two-sample" = {
        s1 <- paste("Let sample 1 and sample 2 be the observations where", input$splitVar, "is", input$cat1, "and", input$cat2)
        s2 <- paste("Let the mean of", input$numVar1, "of sample 1 and sample 2 be μ1 and μ2.")
        null <- paste("H0: μ1 = μ2")
        switch (input$altrHypo,
                "two.sided" = {
                  altr <- paste("H1: μ1 ≠ μ2")
                },
                "greater" = {
                  altr <- paste("H1: μ1 > μ2")
                },
                "less" = {
                  altr <- paste("H1: μ1 < μ2")
                }
        )
        HTML(paste(s1, s2, null, altr, sep = '<br/>'))
      }
    )
  })
  output$box <- renderPlotly({
    if (input$tTestType == "one-sample") {
      x %>% ggplot(aes(y = .data[[input$numVar1]])) + 
        geom_boxplot(color = "skyblue3", outlier.color = "black", fill = "skyblue", outlier.fill = "black") +
        labs(title = "Boxplot")
    } else if (input$tTestType == "paired") {
      d = data.frame(c(x %>% pull(input$numVar1), x %>% pull(input$numVar2))) %>% 
        mutate(
          sample = rep(c(input$numVar1, input$numVar2), each = nrow(x))
        ) %>% drop_na()
      colnames(d)[1] = "value"
      d %>% ggplot(aes(y = value, fill = sample)) + 
        geom_boxplot() +
        labs(title = "Boxplots")
      
    } else if (input$tTestType == "two-sample") {
      #d1 = x %>% filter(input$splitVar == input$cat1)
      x %>% ggplot(aes(y = .data[[input$numVar1]], 
                        x = .data[[input$splitVar]],
                        fill = .data[[input$splitVar]])) + 
        geom_boxplot() + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
        labs(title = "Comparative Boxplot")
      
    }
  })
  output$qq <- renderPlotly({
    if (input$tTestType == "one-sample") { 
      ggqqplot(x, x = input$numVar1, title = "Q-Q Plot")
    } else if (input$tTestType == "paired") { 
      d = data.frame(c(x %>% pull(input$numVar1), x %>% pull(input$numVar2))) %>% 
        mutate(
          sample = rep(c(input$numVar1, input$numVar2), each = nrow(x))
        ) %>% drop_na()
      colnames(d)[1] = "value"
      ggqqplot(d, x = "value", facet.by = "sample", title = "Q-Q Plots")
      
    } else if (input$tTestType == "two-sample") { 
      #d1 = x %>% filter(input$splitVar == input$cat1 ||
      #                  input$splitVar ==  input$cat2)
      ggqqplot(x, x = input$numVar1, facet.by = input$splitVar, title = "Q-Q Plot")
    }  
  })
  output$tTestOutcome <- renderUI({
    switch (input$tTestType,
      "one-sample" = {
        tTest <- reactive({
          tt =t.test(x %>% pull(input$numVar1), mu = input$nullMean,
                     alternative = input$altrHypo, conf.level = 1 - input$sigLvl2)
          return(tt)
        })
        tt = tTest()
        
        # conclusion
        if (tt$p.value < input$sigLvl2) {
          if (input$altrHypo == "two.sided") {
            conclusion <- paste("Since p-value <", input$sigLvl2, ", we reject H0. That is, the mean of", input$numVar1,  "is not equal to", input$nullMean, ".")
          } else if (input$altrHypo == "greater") {
            conclusion <- paste("Since p-value <", input$sigLvl2, ", we reject H0. That is, the mean of", input$numVar1,  "is greater than", input$nullMean, ".")
          } else if (input$altrHypo == "less") {
            conclusion <- paste("Since p-value <", input$sigLvl2, ", we reject H0. That is, the mean of", input$numVar1,  "is less than", input$nullMean, ".")
          }
          
        } else {
          conclusion <- paste("Since p-value >=", input$sigLvl2, ", we do not reject H0. That is, the mean of", input$numVar1,  "is equal to", input$nullMean, ".")
        }
        
        HTML(paste("<h2>Observed test statistic</h2>", 
                   "t0 =", round(tt$statistic, 3),
                   "<h2>p-value</h2>",
                   "p-value = ", signif(tt$p.value, 3),
                   "<h2>Conclusion</h2>",
                   conclusion
                  )
        )
      },
      "paired" = {
        pairedTTest <- reactive({
          sub = x %>% select(input$numVar1, input$numVar2) %>%
            drop_na()
          tt2 = t.test(sub %>% pull(input$numVar1), sub %>% pull(input$numVar2), paired = T,
                     alternative = input$altrHypo, conf.level = 1 - input$sigLvl2)
          return(tt2)
        })
        t2 = pairedTTest()
        
        # conclusion
        if (t2$p.value < input$sigLvl2) {
          if (input$altrHypo == "two.sided") {
            conclusion <- paste("Since p-value <", input$sigLvl2, ", we reject H0. That is, the mean of", input$numVar1, "and", input$numVar2, "are different.")
          } else if (input$altrHypo == "greater") {
            conclusion <- paste("Since p-value <", input$sigLvl2, ", we reject H0. That is, the mean of", input$numVar1,  "is greater than", input$numVar2, ".")
          } else if (input$altrHypo == "less") {
            conclusion <- paste("Since p-value <", input$sigLvl2, ", we reject H0. That is, the mean of", input$numVar1,  "is less than", input$numVar2, ".")
          }
          
        } else {
          conclusion <- paste("Since p-value >=", input$sigLvl2, ", we do not reject H0. That is, the mean of", input$numVar1, "and", input$numVar2, "are the same.")
        }
        
        HTML(paste("<h2>Observed test statistic</h2>", 
                   "t0 =", round(t2$statistic, 3),
                   "<h2>p-value</h2>",
                   "p-value = ", signif(t2$p.value, 3),
                   "<h2>Conclusion</h2>",
                   conclusion
        )
        )
      },
      "two-sample" = {
        twoSpTTest <- reactive({
          sub = x %>% select(input$numVar1, input$splitVar) %>%
            drop_na()
          sample1 = filter(sub, sub[,2] == input$cat1) %>%
            pull(input$numVar1)
          sample2 = filter(sub, sub[,2] == input$cat2) %>%
            pull(input$numVar1)
          
          tt = t.test(sample1, sample2, var.equal = input$varEq,
                     alternative = input$altrHypo, conf.level = 1 - input$sigLvl2)
          return(tt)
        })
        tt = twoSpTTest()
        
        # conclusion
        if (tt$p.value < input$sigLvl2) {
          if (input$altrHypo == "two.sided") {
            conclusion <- paste("Since p-value <", input$sigLvl2, ", we reject H0. That is, the mean of", input$numVar1, "which", input$splitVar, "is", input$cat1, "and", input$cat2, "are different.")
          } else if (input$altrHypo == "greater") {
            conclusion <- paste("Since p-value <", input$sigLvl2, ", we reject H0. That is, the mean of", input$numVar1, "which", input$splitVar, "is", input$cat1 ,"is greater than", input$cat2, ".")
          } else if (input$altrHypo == "less") {
            conclusion <- paste("Since p-value <", input$sigLvl2, ", we reject H0. That is, the mean of", input$numVar1, "which", input$splitVar, "is", input$cat1 ,"is less than", input$cat2, ".")
          }
          
        } else {
          conclusion <- paste("Since p-value >=", input$sigLvl2, ", we do not reject H0. That is, the mean of", input$numVar1, "which", input$splitVar, "is", input$cat1, "and", input$cat2, "are the same.")
        }
        
        HTML(paste("<h2>Observed test statistic</h2>", 
                   "t0 =", round(tt$statistic, 3),
                   "<h2>p-value</h2>",
                   "p-value = ", signif(tt$p.value, 3),
                   "<h2>Conclusion</h2>",
                   conclusion
        )
        )
      }
    )
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
