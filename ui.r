library(shiny)
library(ggplot2)
load("max-data.rdata")

shinyUI(fluidPage(
  titlePanel("The Largest Donations from Each Year of the Promise Lives Campaign"),
  sidebarLayout(
    sidebarPanel(
      selectInput("donors", "View a Donor's Total Contributions:",
                  choices = c("All", "Anonymous", "Cleveland Clinic Health System", "Dr. Violet L. Patton", "Estate of Beth K. Stocker", "Estate of Dolores Russ", "Ms. Lynn Johnson", "Osteopathic Heritage Foundations", "Scripps Howard Foundation")),
      # add changing text here (description of the donors)
      hr()
    ), # end sidebarPanel
    mainPanel(
      plotOutput("main_plot")
      )
      
  ) # end sidebarLayout
  
))