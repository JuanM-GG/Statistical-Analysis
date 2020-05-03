####################################################################################################################
# Título:       Monod con crecimiento de recurso.
# Nombre:       Juan Manuel Gutiérrez García.
# Fecha:        2 de mayo de 2020.
# Notas:        El valor de delta = 0.4545 ocasiona un cambio en la estabilidades del sistema
#####################################################################################################################

# Load the libraries
library(shiny)

ui <- fluidPage(
  sidebarLayout(
          
          sidebarPanel(
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  numericInput(inputId = "delta",
                               label = "delta",
                               value = 0.46, 
                               step = 0.01)
          ),
          
          mainPanel(
                  plotOutput("plotPlane", height = "300px"),
                  plotOutput("plotTime", height = "300px")
          )
  )
)

server <- function(input, output, session) {
        
        # Set the model
        model <- function(t, state, parms) {
                with(as.list(c(state,parms)), {
                        
                        dR <- r*R*(1 - R/K) - a*R*N/(h+R);
                        dN <- c*a*R*N/(h+R) - delta*N;
                        
                        return(list(c(dR, dN)));  
                })} 
        
        
        s <- c(R=0.5,N=0.4);
        
        # Set parameters
        p <- reactive({
                c(r=1,K=1,h=0.1,a=0.5,c=1,delta=input$delta)
        })
         
        output$plotPlane <- renderPlot({
          
                 # Draw plane
                 plane(xmax=1.5,eps=-0.01, parms = p());
                 run(tmax = 200, traject = T, parms = p());
        })
        
        output$plotTime <- renderPlot({
                
                # Time plot
                run(tmax = 200, parms = p());
        })
  
}

shinyApp(ui, server)
