library(shiny)
library(shinythemes)

shinyUI(
    navbarPage("Coursera Datascience Capstone Project",
               theme = shinytheme("simplex"),
               tabPanel("App",
                        span(textOutput("wait")),
                        fluidRow(
                            column(3),
                            column(6,
                                   tags$div(
                                       span(textAreaInput("text", 
                                                          label = h3("Enter text here:"),
                                                          height = '300',
                                                          width="200%",
                                                          resize = "vertical",
                                                          value = )),
                                       tags$style(type="text/css", "#text { height: 50px; width: 100%; font-size: 30px; display: block;}"),
                                       conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                        tags$div("Loading...",id="loadmessage")),
                                       tags$style(type="text/css", "#loadmessage { height: 50px; width: 100%; font-size: 30px; display: block;text-align:center;}"),
                                       h4("The Predicted Words:"),
                                       tags$div(fluidRow(
                                           column(4,
                                                  span(style="text-align:center",tags$strong(tags$h3(textOutput("predictedWordLeft"))))),
                                           column(4,
                                                  span(style="text-align:center",tags$strong(tags$h3(textOutput("predictedWordMid"))))),
                                           column(4,
                                                  span(style="text-align:center",tags$strong(tags$h3(textOutput("predictedWordRight")))))
                                       ),
                                       tags$head(tags$style("#predictedWordMid{color: #26a69a;
                                                            }"
                                       )
                                       )
                                       ),
                                       align="left")
                            ),
                            column(3)
                        )),
               tabPanel("Help",
                        fluidRow(
                            column(3),
                            column(6,
                                   h2("Data Science Specialization Capstone Project"),
                                   h4("The goal of this app is to predict the next word, given a partial English sentence."),
                                   h4("This Shiny app was developed for the Capstone Project of the Johns Hopkins Coursera Data Science Specialization. This Capstone Project is designed in partnership with Swiftkey."),
                                   hr(),  
                                   h2("How to use the application"),
                                   h4(tags$ol(
                                       tags$li("'Loading' message is displayed whenever shiny app is processing"),
                                       tags$li("The user needs to enter a partial sentence in the text area field."),
                                       tags$li("The 3 predicted words appears in bottom of the text area"),
                                       tags$li("The middle word has the highest probability followed by left one and right one")
                                   )),
                                   hr(),
                                   h2("Algorithm Used"),
                                   h4("Model computs the probability of occuring the next word based on last few words of the sentence."),
                                   h4("ngrams are computed and frequency dictionary of ngrams created"),
                                   h4("Stupid backoff algorithm is used for prediticting the words.")
                            ),
                            column(3)))
    )
)
