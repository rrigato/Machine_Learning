install.packages("shiny")

library(shiny)

data(iris)

ui <- 
fluidPage(
		#creates a slider object
		sliderInput(inputId = "num", label = "Choose a number",   
			   value = 12, min = 1, max = 25),
		#need to put a comma between objects
		plotOutput(outputId="main_plot")
	   )

server <- function (input, output)
{
	output$main_plot <- renderPlot({

    				hist(		iris$Sepal.Length,
     						 xlab = "Duration (minutes)",
    				  			main = "Geyser eruption duration"
					)
				})
}


shinyApp(ui=ui, server=server)


