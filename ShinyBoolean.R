library(shiny)
library(ggplot2)
library(ggforce)
library(sf)

# Create sf circle as polygon
circle_sf <- function(x, y, r, id) {
  # Treat as planar geometry, no CRS
  st_buffer(st_sfc(st_point(c(x, y))), dist = r) |>
    st_sf(set = id)
}

# UI
ui <- fluidPage(
  titlePanel("Boolean Search Logic with Venn Diagrams"),
  sidebarLayout(
    sidebarPanel(
      textInput("setA", "Set A:", "apple,banana"),
      textInput("setB", "Set B:", "banana,grape"),
      selectInput("op", "Boolean Operation", choices = c("AND", "OR", "NOT A", "NOT B"))
    ),
    mainPanel(
      plotOutput("vennPlot", height = "500px"),
      verbatimTextOutput("resultText")
    )
  )
)

# Server
server <- function(input, output) {
  
  parseSet <- function(txt) {
    unique(trimws(unlist(strsplit(txt, ","))))
  }
  
  output$vennPlot <- renderPlot({
    # Sets
    A <- parseSet(input$setA)
    B <- parseSet(input$setB)
    
    # Circles as sf polygons
    circleA <- circle_sf(0, 0, 1, "A")
    circleB <- circle_sf(1, 0, 1, "B")
    
    region <- switch(input$op,
                     "AND"   = st_intersection(circleA, circleB),
                     "OR"    = {
                       # Ensure union is properly handled
                       geom <- st_union(st_geometry(circleA), st_geometry(circleB))
                       highlight <- st_sf(geometry = st_sfc(geom))
                       highlight$set <- "A ∪ B"
                       highlight
                     },
                     "NOT A" = st_difference(circleB, circleA),
                     "NOT B" = st_difference(circleA, circleB)
    )
    
    base <- ggplot() +
      geom_sf(data = circleA, fill = "lightblue", alpha = 0.3, color = "black") +
      geom_sf(data = circleB, fill = "lightpink", alpha = 0.3, color = "black") +
      annotate("text", x = -0.7, y = 1.1, label = "A", size = 6) +
      annotate("text", x = 1.7, y = 1.1, label = "B", size = 6) +
      theme_void() +
      coord_sf()
    
    result_color <- switch(input$op,
                           "AND" = "red",
                           "OR" = "orchid",
                           "NOT A" = "hotpink",
                           "NOT B" = "deepskyblue")
    
    # Add logical region
    if (!is.null(region)) {
      base <- base + geom_sf(data = region, fill = result_color, alpha = 0.5, color = NA)
    }
    
    base
  })
  
  output$resultText <- renderPrint({
    A <- parseSet(input$setA)
    B <- parseSet(input$setB)
    result <- switch(input$op,
                     "AND" = intersect(A, B),
                     "OR"  = union(A, B),
                     "NOT A" = setdiff(B, A),
                     "NOT B" = setdiff(A, B))
    cat("Result:\n")
    print(result)
  })
}

shinyApp(ui, server)

