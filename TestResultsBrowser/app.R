library(shiny)
#source("parameters.R")

dimentionalities = c(2,5,20,2,5,10,5,5)
function_indices = 1:28
start_sizes = c(10,10,10,20,20,20,50,100)
points_types = c("random", "hypergrid", "poisson_disk")

generateFileName <- function(f,D,nStart, points_type, atempt, epsilon=1e-4, stepSize=1, maxIter=5000){
  #generate name
  name = sprintf("../Results/f%d_D%d_nStart%d_stepSize%f_epsilon%f_maxIter%d_%s_%d.csv", f,D,nStart,stepSize,epsilon,maxIter,points_type, atempt)
  #print(name)
  name
}

charts <- function(input, atempt){
  params <- parseInput(input)
  filename <- generateFileName(params$r, params$k, params$step, params$it, params$tmin, params$tmax, params$points_type, atempt)
  gamedata = read.csv(filename, sep=";", header=FALSE, fill= TRUE,col.names = 0:75)
  gamelengths = table(factor(gamedata$X0, 
                             #levels = min(gamedata$X0):max(gamedata$X0)
                             levels = 3:35
                             ))
  barplot(gamelengths, 
          main = sprintf("Game lenghts for r=%d k=%d \n vs %s points_type",params$r,params$k, params$points_type), 
          sub = sprintf("step=%g \n %d iterations per step (%d in total) \n Tmax=%d, Tmin=%d", 
                        params$step, params$it, params$it/params$step, params$tmax, params$tmin))
}

aver <- function(input, atempt){
  params <- parseInput(input)
  filename <- generateFileName(params$r, params$k, params$step, params$it, params$tmin, params$tmax, params$points_type, atempt)
  gamedata = read.csv(filename, sep=";", header=FALSE, fill= TRUE,col.names = 0:75)
  mean(gamedata$X0)
}

parseInput <- function(input){
  params <- list("D"=dimentionalities[as.numeric(input$Dn)],
                 "cecfun"=as.numeric(input$cecfun),
                 "start_size"= start_sizes[as.numeric(input$Dn)],
                 "points_type" = points_types[as.numeric(input$points_type)]
                 )
  return(params)
}

#server.R
server <- function(input, output) {
  #params = parseInput(input)
  # output$value = renderPrint({
  #   params=parseInput(input) 
  #   n <- generateFileName(params$r, params$k, params$step, params$it, params$tmin, params$tmax, params$points_type, 1)
  # })
  output$text = renderText({
    params <- parseInput(input)
    n <- generateFileName(params$cecfun, params$D, params$start_size, params$points_type, 1)
    nam <- paste("You have chosen", params$cecfun, params$D, params$start_size, params$points_type, 1)
    nam
  })
  
  # output$plot1 = renderPlot({ charts(input, 1) })
  # output$plot2 = renderPlot({ charts(input, 2) })
  # output$plot3 = renderPlot({ charts(input, 3) })
  # output$plot4 = renderPlot({ charts(input, 4) })
  # output$plot5 = renderPlot({ charts(input, 5) })
  # output$avg1 = renderText({ aver(input, 1) })
  # output$avg2 = renderText({ aver(input, 2) })
  # output$avg3 = renderText({ aver(input, 3) })
  # output$avg4 = renderText({ aver(input, 4) })
  # output$avg5 = renderText({ aver(input, 5) })
}

# ui.R
ui <- shinyUI(fluidPage(
  titlePanel("Multi-start hillclimbing algorithm"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select parameters to see results from test runs for a selected function from CEC2013 benchmark"),
      
      radioButtons("Dn", 
                   label = h3("Dimentionality of the problem"),
                   choices = list( "D = 2,  nStartingPoints = 10" = 1,
                                   "D = 5,  nStartingPoints = 10" = 2, 
                                   "D = 20, nStartingPoints = 10" = 3, 
                                   "D = 2,  nStartingPoints = 20" = 4, 
                                   "D = 5,  nStartingPoints = 20" = 5, 
                                   "D = 10, nStartingPoints = 20" = 6, 
                                   "D = 5,  nStartingPoints = 50" = 7, 
                                   "D = 5,  nStartingPoints = 100" = 8),
                   selected = 1),
      selectInput("cecfun", 
                   label = h3("CEC2013 function:"),
                   choices = (as.list(setNames(function_indices, function_indices))),
                   selected = 1),
      radioButtons("points_type", 
                   label = h3("Starting points selection method:"),
                   choices = list("Random" = 1,
                                  "Hypergrid" = 2,
                                  "Poisson disk" = 3),
                   selected = 1),
      hr(),
      fluidRow(column(12, verbatimTextOutput("value")))
      #       ,
      #       submitButton("Generate plots")
    ),
    
    mainPanel(
      # plotOutput("plot1"), textOutput("avg1"),
      # plotOutput("plot2"), textOutput("avg2"),
      # plotOutput("plot3"), textOutput("avg3"),
      # plotOutput("plot4"), textOutput("avg4"),
      # plotOutput("plot5"), textOutput("avg5"),
      textOutput("text")
    )
  )
))

shinyApp(ui = ui, server = server)