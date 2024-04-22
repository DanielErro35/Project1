
library(shiny)
library(here)
library(tidyverse)
library(stringr)
library(bslib)

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
  "Release Speed" = "RelSpeed",
  "Spin Rate" = "SpinRate",
  "Extension" = "Extension",
  "Vertical Break" = "InducedVertBreak",
  "Horizontal Break" = "HorzBreak"
)


server <- function(input, output, session) {
  
  # Filter the movies, returning a data frame
  baseball <- reactive({
    # Due to dplyr issue #318, we need temp variables for input values
    relspeed <- input$relspeed
    spinrate <- input$spinrate
    mininning <- input$inning[1]
    maxinning <- input$inning[2]
    
    if (input$game == "Game 1") {
      baseball_data <- baseball_22
    }else if (input$game == "Game 2") {
      baseball_data <- baseball_23
    }else if (input$game == "Game 3") {
      baseball_data <- baseball_24
    }
    
    # Apply filters
    b <- baseball_data %>%
      filter(
        RelSpeed >= relspeed,
        SpinRate >= spinrate,
        Inning >= mininning,
        Inning <= maxinning
      ) %>%
      arrange(RelSpeed)
    
    # Optional: filter by Pitch Type
    if (input$pitchtype != "All") {
      pitchtype <- input$pitchtype
      b <- b %>% filter(grepl(pitchtype, TaggedPitchType))
    }
    # # Optional: filter by pitcher
    if (!is.null(input$pitcher) && input$pitcher != "") {
      pitcher = input$pitcher
      b <- b %>% filter(grepl(tolower(pitcher), tolower(Pitcher)))
    }
    # # Optional: filter by batter
    if (!is.null(input$batter) && input$batter != "") {
      batter = input$batter
      b <- b %>% filter(grepl(tolower(batter), tolower(Batter)))
    }
    
    b <- as.data.frame(b)
    
    # Add column which says whether the movie won any Oscars
    # Be a little careful in case we have a zero-row data frame
    b$team <- character(nrow(b))
    b$team[b$PitcherTeam == "CAL_MUS"] <- "Cal Poly"
    b$team[b$PitcherTeam == "SAN_GAU"] <- "UCSB"
    b
  })
  
  # Function for generating tooltip text
  baseball_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$PitchNo)) return(NULL)

    # Pick out the movie with this ID
    baseball_22 <- isolate(baseball())
    bb <- baseball_22[baseball_22$PitchNo == x$PitchNo, ]

    paste0("<b>", bb$Pitcher, "</b><br>",
           bb$TaggedPitchType, "<br>", "Batter: ", bb$Batter
    )
  }
  
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
                   fillOpacity := 0.2, fillOpacity.hover := 0.5,
                   stroke = ~team, key := ~PitchNo) %>%
      add_tooltip(baseball_tooltip, "hover") %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name) %>%
      add_legend("stroke", title = "Pitcher's Team", values = c("Cal Poly Mustangs", "UCSB Gauchos")) %>%
      scale_nominal("stroke", domain = c("Cal Poly", "UCSB"),
                    range = c("forestgreen", "blue")) %>%
      set_options(width = 500, height = 500)
  })
  
  vis %>% bind_shiny("plot1")
  
  output$n_baseball <- renderText({ nrow(baseball()) })
}



ui <- fluidPage(theme = bs_theme(preset = "united"),
  titlePanel("CalPoly vs UCSB baseball pitches explorer"),
  fluidRow(
    column(3,
           wellPanel(
             h4("Filter"),
             selectInput("game", "Game",
                         c("Game 1", "Game 2", "Game 3")
             ),
             sliderInput("inning", "Inning", 1, 12, value = c(1, 9),
                         sep = ""),
             sliderInput("relspeed", "Minimum speed of ball",
                         70, 90, 80, step = 2),
             sliderInput("spinrate", "Minimum spin rate of ball", 1100, 2700, 1800, step = 100),
             selectInput("pitchtype", "Pitch Type",
                         c("All", "ChangeUp", "Curveball", "Cutter", "Fastball", "FourSeamFastBall",
                           "Slider", "TwoSeamFastBall")
             ),
             textInput("pitcher", "Pitcher name contains (e.g., 'matt', 'ager, matt')"),
             textInput("batter", "Batter name contains (e.g. 'joe', 'yorke, joe')")
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

