library(shiny)
source("Grind.R")
ui <- fluidPage(
  sidebarLayout(
          sidebarPanel(
                  numericInput(inputId = "a",label = "a",value = 500),
                  numericInput(inputId = "b",label = "b",value = 0.01),
                  numericInput(inputId = "c",label = "c",value = 0),
                  numericInput(inputId = "d",label = "d",value = 0.001),
                  numericInput(inputId = "e",label = "e",value = 0.1),
                  numericInput(inputId = "v",label = "v",value = 10),
                  numericInput(inputId = "w",label = "w",value = 1),
                  numericInput(inputId = "t",label = "t",value = 60)
          ),
          mainPanel(
                  plotOutput("plot"),
                  plotOutput("plane")
          )
  )
)

server <- function(input, output, session) {
        
        # Set model
        model <- function(time,state,parms){
                with(as.list(c(state,parms)),{
                        
                        dw <- a-b*w*v-c*w;
                        dv <- d*w*v-e*v
                        
                        return(list(c(dw,dv)))
                })
        }
        
        # Set parameters
        p <- reactive({
                c(a=input$a,b=input$b,c=input$c,d=input$d,e=input$e)
        })
        
        # Set initial condition
        s <- reactive({
                c(w=input$w, v=input$v)
        })
        
        # Solve the model
        out <- reactive({
                run(tmax = input$t,
                    tstep = 0.1,
                    state = s(),
                    parms = p(),
                    odes = model,
                    timeplot = FALSE,
                    table = TRUE)
        })
        
        # Show time plot
        output$plot <- renderPlot({
                matplot(x = out()[,1],
                        y = out()[,2:3],
                        type = "l",
                        lty = c(1,2),
                        lwd = 3,
                        col = c(1,2),
                        xlab = "time",
                        ylab = "Density")
                
                legend("topright", 
                       legend = c("w","v"),
                       lty = c(1,2),
                       lwd = 3,
                       bty = "n",
                       col = c(1,2))
        })
        
        # Show plane plot
        output$plane <- renderPlot({
                plane(state = s(),parms = p(),xmin = 0,xmax = 600,ymin = 0,ymax = 2000,vector = TRUE)
                
                w <- input$e/input$d
                v <- (input$a*input$d-input$c*input$e)/(input$b*input$e)
                
                newton(state = c(w = w, v = v),parms = p(),plot = TRUE)
        })
 
}

shinyApp(ui, server)