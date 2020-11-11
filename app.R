library(shiny)
library(readr)
library(readxl)
library(ggplot2)
library(ggridges)
library(maps)
library(ellipse) 
library(corrplot)
library(dplyr)
library(reshape2)
library(viridis)
library(scales)
library(ggmosaic)
library(plotly)
library(GGally)
library(ggthemes)
library(lubridate)


studentPerformance <- read_csv("studentsPerformance.csv")
studentPerformance$gender <- as.factor(studentPerformance$gender)
studentPerformance$race.ethnicity <- as.factor(studentPerformance$race.ethnicity)
studentPerformance$parental.level.of.education <- as.factor(studentPerformance$parental.level.of.education)
studentPerformance$lunch <- as.factor(studentPerformance$lunch)
studentPerformance$test.preparation.course  <- as.factor(studentPerformance$test.preparation.course )

getGrade <- function(score) {
    if(score >= 90 & score <= 100) {
        return("A")
    } else {
        if(score >= 85 & score <= 89) {
            return("A-")
        } else {
            if(score >= 80 & score <= 84) {
                return("B+")
            } else {
                if(score >= 75 & score <= 79) {
                    return("B")
                } else {
                    if(score >= 70 & score <= 74) {
                        return("B-")
                    } else {
                        if(score >= 65 & score <= 69) {
                            return ("C")
                        } else {
                            if(score >= 50 & score <= 64) {
                                return ("D")
                            } else {
                                if(score > 0 & score <= 49) {
                                    return ("E")
                                } else {
                                    return ("F")
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

getWeight<-function(grade){
    weight <- switch(grade, 
                     "A"=4.00, 
                     "A-"=3.67, 
                     "B+"=3.33, 
                     "B"=3.00, 
                     "B-"=2.50, 
                     "C"=2.0, 
                     "D"=1.0, 
                     "E"=0.00, 
                     "F"=0.00)
    return(weight)
}

studentPerformance$math.grade <- sapply(studentPerformance$math.score, FUN=getGrade)
studentPerformance$reading.grade <- sapply(studentPerformance$reading.score, FUN=getGrade)
studentPerformance$writing.grade <- sapply(studentPerformance$writing.score, FUN=getGrade)

studentPerformance$math.weight <- sapply(studentPerformance$math.grade, FUN=getWeight)
studentPerformance$reading.weight <- sapply(studentPerformance$reading.grade, FUN=getWeight)
studentPerformance$writing.weight <- sapply(studentPerformance$writing.grade, FUN=getWeight)

studentPerformance$math.grade <- with(studentPerformance, 
                                      ifelse(math.score >= 90 & math.score <= 100, "A",
                                             ifelse(math.score >= 85 & math.score <= 89, "A-",
                                                    ifelse(math.score >= 80 & math.score <= 84, "B+",    
                                                           ifelse(math.score >= 75 & math.score <= 79, "B", 
                                                                  ifelse(math.score >= 70 & math.score <= 74, "B-",
                                                                         ifelse(math.score >= 65 & math.score <= 69, "C",
                                                                                ifelse(math.score >= 50 & math.score <= 64, "D",
                                                                                       ifelse(math.score >= 0 & math.score <= 49, "D", "F"
                                                                                       )))))))))

studentPerformance$GPA <- apply(cbind(studentPerformance$math.weight, studentPerformance$reading.weight, studentPerformance$writing.weight),1, FUN=mean)

studentPerformance$eligible <- with(studentPerformance, ifelse(GPA>=2,T, F))


studentPerformance$math.grade <- as.factor(studentPerformance$math.grade)
studentPerformance$reading <- as.factor(studentPerformance$reading.grade)
studentPerformance$writing.grade <- as.factor(studentPerformance$writing.grade)

#View(studentPerformance)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Student Performance"),
    
    selectInput("select", h3("Select Color"), 
                choices = list("Green" = "green", "Red" = "red",
                               "Blue" = "blue"), selected = 1),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- studentPerformance$math.score
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = input$select, border = 'black', xlab = "Math Test Score",
             ylab = "Frequency", main = "Math Scores")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
