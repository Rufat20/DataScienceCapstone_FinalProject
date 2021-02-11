library(shiny)
library(shinythemes)

# Define UI for dataset viewer application
shinyUI(fluidPage(theme = shinytheme("superhero"),
                  
                  # Application title
                  titlePanel("Predicting Next Words with Word-Cloud"),
                  
                  sidebarLayout(position = "left",
                                sidebarPanel(
                                    h3("Description for Application"),
                                    p("This application predicts the most likely next words based on what you typed. Also, it generates the wordcloud containing next word candidates sorted by the probability of the word."),
                                    p("The model is developed in R and it is the final project for the Data Science Specialization by Johns Hopkins University on Coursera."),
                                    
                                    h3("How to use it ?"),
                                    p("Just type words. The predicted words are updated after each space."),
                                    
                                    h3("Additional information"),
                                    div(
                                        a("Github",
                                          href="https://github.com/Rufat20/DataScienceCapstone_FinalProject")
                                    )
                                ),
                                
                                mainPanel(
                                    textInput("userTxt", "Type words : (Don't forget put a space in the last)", ""),
                                    h4("Most likely next words :"),
                                    textOutput("wordPrediction01", container = div),
                                    textOutput("wordPrediction02", container = div),
                                    textOutput("wordPrediction03", container = div),
                                    textOutput("wordPrediction04", container = div),
                                    textOutput("wordPrediction05", container = div),
                                    imageOutput("wordcloud")
                                )
                  ),
                  div(class = "footer",
                      #style="text-align: center;",
                      p("Rufat - February 11, 2021")
                  )
))