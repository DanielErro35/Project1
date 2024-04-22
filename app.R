
library(shiny)
library(here)
library(tidyverse)

library(ggvis)
library(dplyr)
if (FALSE) {
  library(RSQLite)
  library(dbplyr)
}

# Set up handles to database tables on app start
baseball_22 <- read_csv(here("data", "3-22-24_CalPoly_UCSB.csv"))
baseball_23 <- read_csv(here("data", "3-23-24_CalPoly_UCSB.csv"))
baseball_24 <- read_csv(here("data", "3-24-24_CalPoly_UCSB.csv"))

axis_vars <- c(
  "Relative Speed" = "RelSpeed",
  "Spin Rate" = "SpinRate"
)


server <- function(input, output, session) {
  
  # Filter the movies, returning a data frame
  baseball <- reactive({
    # Due to dplyr issue #318, we need temp variables for input values
    relspeed <- input$relspeed
    spinrate <- input$spinrate
    
    # Apply filters
    b <- baseball_22 %>%
      filter(
        RelSpeed >= relspeed,
        SpinRate >= spinrate
      ) %>%
      arrange(RelSpeed)
    
    # # Optional: filter by genre
    # if (input$genre != "All") {
    #   genre <- paste0("%", input$genre, "%")
    #   m <- m %>% filter(Genre %like% genre)
    # }
    # # Optional: filter by director
    # if (!is.null(input$director) && input$director != "") {
    #   director <- paste0("%", input$director, "%")
    #   m <- m %>% filter(Director %like% director)
    # }
    # # Optional: filter by cast member
    # if (!is.null(input$cast) && input$cast != "") {
    #   cast <- paste0("%", input$cast, "%")
    #   m <- m %>% filter(Cast %like% cast)
    # }
    
    
    b <- as.data.frame(b)
    
    # Add column which says whether the movie won any Oscars
    # Be a little careful in case we have a zero-row data frame
    # m$has_oscar <- character(nrow(m))
    # m$has_oscar[m$Oscars == 0] <- "No"
    # m$has_oscar[m$Oscars >= 1] <- "Yes"
    # m
  })
  
  # Function for generating tooltip text
  # movie_tooltip <- function(x) {
  #   if (is.null(x)) return(NULL)
  #   if (is.null(x$ID)) return(NULL)
  #   
  #   # Pick out the movie with this ID
  #   all_movies <- isolate(movies())
  #   movie <- all_movies[all_movies$ID == x$ID, ]
  #   
  #   paste0("<b>", movie$Title, "</b><br>",
  #          movie$Year, "<br>",
  #          "$", format(movie$BoxOffice, big.mark = ",", scientific = FALSE)
  #   )
  # }
  
  # A reactive expression with the ggvis plot
  vis <- reactive({
    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    
    # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
    # but since the inputs are strings, we need to do a little more work.
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
    baseball %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_points(size := 50, size.hover := 200,
                   fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name) %>%
      set_options(width = 500, height = 500)
  })
  
  vis %>% bind_shiny("plot1")
  
  output$n_baseball <- renderText({ nrow(baseball()) })
}



ui <- fluidPage(
  titlePanel("CalPoly vs UCSB baseball pitches explorer"),
  fluidRow(
    column(3,
           wellPanel(
             h4("Filter"),
             sliderInput("relspeed", "Minimum speed of ball",
                         70, 90, 80, step = 2),
             sliderInput("spinrate", "Minimum spin rate of ball", 1100, 2600, 1800, step = 100),
           ),
           wellPanel(
             selectInput("xvar", "X-axis variable", axis_vars, selected = "RelSpeed"),
             selectInput("yvar", "Y-axis variable", axis_vars, selected = "SpinRate"),
           )
    ),
    column(9,
           ggvisOutput("plot1"),
           wellPanel(
             span("Number of pitches selected:",
                  textOutput("n_baseball")
             )
           )
    )
  )
)


shinyApp(ui = ui, server = server)

