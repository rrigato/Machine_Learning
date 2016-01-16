install.packages("shiny")

library(shiny)

ui <- 
fluidPage(
		#creates a slider object
		sliderInput(inputId = "num", label = "Choose a number",   
			   value = 12, min = 1, max = 25),
		#need to put a comma between objects
		plotOutput(outputId="hist")
	   )

server <- function (input, output)
{
}


shinyApp(ui=ui, server=server)


