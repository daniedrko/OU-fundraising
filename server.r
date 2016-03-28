library(shiny)
library(ggplot2)
library(plotly)
load("max-data.rdata")



shinyServer(function(input, output) {
  output$main_plot <- renderPlot(function() {
    pal <- c('#1f3d7a', '#9daf72', '#566047', '#562f32', '#462d44', '#859731', '#640e27', '#33001a')
    if(input$donors == "All") {
      ggplot(maxx, aes(year, AMT, fill = factor(DONORNAME))) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = pal) +
        ggtitle("The largest gifts to Ohio University, 2005-2015") +
        xlab("Year of Donation") +
        ylab("Amount Given")
    }
    else {
      if(input$donors == "Anonymous") {
        ggplot(an, aes(gift_date, AMT)) +
          geom_point(color = '#1f3d7a', size = 3) +
          ggtitle("Anonymous Donations") +
          xlab("Gift Date") +
          ylab("Amount of Individual Donation")
      }
      else {
        if(input$donors == "Cleveland Clinic Health System") {
          ggplot(cle, aes(gift_date, AMT)) +
            geom_point(colour = '#9daf72', size = 3) +
            ggtitle("Donations from the Cleveland Clinic Health System") +
            xlab("Gift Date") +
            ylab("Amount of Individual Donation")
        }
        else {
          if(input$donors == "Dr. Violet L. Patton") {
            ggplot(vlp, aes(gift_date, AMT)) +
              geom_point(colour = '#566047', size = 3) +
              ggtitle("Donations from Dr. Violet L. Patton") +
              xlab("Gift Date") +
              ylab("Amount of Individual Donation")
          }
          else {
            if(input$donors == "Estate of Beth K. Stocker") {
              ggplot(bks, aes(gift_date, AMT)) +
                geom_point(colour = '#562f32', size = 3) +
                ggtitle("Donations from the Estate of Beth K. Stocker") +
                xlab("Gift Date") +
                ylab("Amount of Individual Donation")
            }
            else {
              if(input$donors == "Estate of Dolores Russ") {
                ggplot(dr, aes(gift_date, AMT)) +
                  geom_point(colour = '#462d44', size = 3) +
                  ggtitle("Donations from the Estate of Dolores Russ") +
                  xlab("Gift Date") +
                  ylab("Amount of Individual Donation")
              }
              else {
                if(input$donors == "Ms. Lynn Johnson") {
                  ggplot(lj, aes(gift_date, AMT)) +
                    geom_point(colour = '#859731', size = 3) +
                    ggtitle("Donations from Lynn Johnson") +
                    xlab("Gift Date") +
                    ylab("Amount of Individual Donation")
                }
                else {
                  if(input$donors == "Osteopathic Heritage Foundations") {
                    ggplot(ohf, aes(gift_date, AMT)) +
                      geom_point(colour = '#640e27', size = 3) + 
                      ggtitle("Donations from Osteopathic Heritage Foundation") +
                      xlab("Gift Date") + 
                      ylab("Amount of Individual Donation")
                  }
                  else {
                    ggplot(shf, aes(gift_date, AMT)) +
                      geom_point(colour = '#33001a', size = 3) +
                      ggtitle("Donations from the Scripps Howard Foundation") +
                      xlab("Gift Date") +
                      ylab("Amount of Individual Donation")
                  }
                }
              }
            }
          }
        }
      }
      
    } 
 
  }) ## end of main_plot
  
  output$words <- renderText(function() {
    if(input$donors == "All") {
      "The top single donations to Ohio University per each year of the Promise Lives campaign. So larger donations, such as the Osteopathic Heritage Foundation's $105 million gift in 2011, appear to be smaller in the university's data because they were split up by their intended purposes."
    }
    else {
      if()
    }
  }) ## end of words
  
  output$tab2 <- renderPlot(function() {
    if(input$donors == "All") {
      ggplot(tops, aes(year, total, group = DONORNAME)) +
        geom_bar(stat = "identity", position = "dodge", aes(fill = factor(DONORNAME))) +
        scale_fill_manual(values = pal) +
        scale_x_discrete(limits = levels(tops$year))
    }
    else {
      ggplot(subset(tops, DONORNAME == input$donors), aes(year, total, group = DONORNAME)) +
        geom_bar(stat = "identity", position = "dodge", aes(fill = factor(DONORNAME))) +
        scale_fill_manual(values = pal) +
        scale_x_discrete(limits = levels(tops$year))
    }
    
  })
  
})