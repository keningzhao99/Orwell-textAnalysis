library(shiny)
library(shinydashboard)
library(tidytext) # text mining using tidy tools
library(textdata) # get sentiment lexicons
library(tidyverse)
library(scales)
library(igraph)
library(ggraph)

c_lexi = c("Bing", "NRC", "Afinn")

ui <- dashboardPage(

  dashboardHeader(
    title = "Sentiment Analysis with George Orwell's Text",
    titleWidth = 450,
    tags$li(class = "dropdown", tags$a(href = "https://www.linkedin.com/in/kening-zhao/", icon("linkedin"), "My Profile", target = "_blank")),
    tags$li(class = "dropdown", tags$a(href = "https://github.com/keningzhao99/Orwell-textAnalysis.git", icon("github"), "Source Code", target = "_blank"))
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      
      # first menu item
      menuItem("Sentiment Analysis", tabName = "senti"),
      
      conditionalPanel("input.sidebar == 'senti'", selectInput(inputId = "books", label = "Select a Work", choices = sum_df %>% select(Title) %>% distinct() %>% arrange(Title), 
                  selected = "A Clergyman's Daughter")),
      conditionalPanel("input.sidebar == 'senti'", sliderInput(inputId = "top_n", label = "Top N Words", 1, 30, value = 5)),
      conditionalPanel("input.sidebar == 'senti'", radioButtons(inputId = "n_gram", label = "Select Nth-Gram", choices = c(2,3))),
      
      # second menu item
      menuItem("Word Frequencies Correlation", tabName = "freq"),
      conditionalPanel("input.sidebar == 'freq'", selectInput(inputId = "to_compare1", label = "Select a Work for Correlation Analysis", choices = c("A Clergyman's Daughter", "Animal Farm", "Nineteen Eighty-Four", 
                                                                                "Coming up for Air", "Homage to Catalonia"),
                  selected = "A Clergyman's Daughter"))
    )
  ),
  
  dashboardBody(
    tabItems(

      # first tab item
      tabItem(tabName = "senti",

              # tab box
              tabBox(id = "t1", width = 10,
                     tabPanel(title = "About",
                              "This Web Page is created out of personal interest. I have always been a fan of Dystopian and Satire Novels,
                                          especially those of George Orwell's. I made this web page using R that analyzes five works of his via
                                          the method of Text Mining in hopes to find anything interesting about the text as well as the correlation 
                                          across his different books.",
    
    
                                          "Read more about each book in this analysis by clicking on the icon below"
                                ,
                              tags$li(class="dropdown",tags$a(href="https://en.wikipedia.org/wiki/Animal_Farm", icon("book"), "Animal Farm",target="_blank")),
                              tags$li(class="dropdown",tags$a(href="https://en.wikipedia.org/wiki/Nineteen_Eighty-Four", icon("book"), "Nineteen Eighty-Four",target="_blank")),
                              tags$li(class="dropdown",tags$a(href="https://en.wikipedia.org/wiki/Coming_Up_for_Air", icon("book"), "Coming up for Air",target="_blank")),
                              tags$li(class="dropdown",tags$a(href="https://en.wikipedia.org/wiki/Homage_to_Catalonia", icon("book"), "Homage to Catalonia",target="_blank")),
                              tags$li(class="dropdown",tags$a(href="https://en.wikipedia.org/wiki/A_Clergyman%27s_Daughter", icon("book"), "A Clergyman's daughter",target="_blank"))
                             ),
                     tabPanel(title = "Lexicon Analysis",
                              selectInput(inputId = "lexi", label = "Select Lexicon", choices = c_lexi, selected = "Bing"),
                              plotOutput("lexi_plots")
                     ), 
                     tabPanel(title = "N-Grams Analysis",
                              plotOutput("ngram_plot"))
              )
      ),


      # second tab item
      tabItem(tabName = "freq",

              #tab box
              tabBox(id = "t2", width = 10,
                     tabPanel(title = "Correlation Graph",
                              fluidRow(box(plotOutput("corr_plot")),box(plotOutput("corr_plot2"))),
                              fluidRow(box(plotOutput("corr_plot3")),box(plotOutput("corr_plot4")))),
                     tabPanel(title = "Pearson's product-moment correlation",
                              fluidPage(
                                verbatimTextOutput("cor_sum")
                              )
                              )
              )
      )
    )
  ),
  
  skin = "black"

)
