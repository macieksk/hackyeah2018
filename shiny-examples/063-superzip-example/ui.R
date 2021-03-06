# %load shiny-examples/063-superzip-example/ui.R
library(leaflet)

# Choices for drop-downs

#vars <- c(
#  "Zgony" = "zgonyCSV"
#)
vars<-2:NCOL(powiatyData)
names(vars)<-colnames(powiatyData[,2:NCOL(powiatyData)])

navbarPage("LuGi - HackYeah2018", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Health myth busters"),

        selectInput("dane", "Dane", vars),
        #conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
          # Only prompt for threshold when coloring or sizing by superzip
         # numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        #),
        
        textOutput("coord_lat"),
        textOutput("coord_lng"),
        actionButton("testButton","LoadData"),
        textOutput("testText"),

        plotOutput("histCentile", height = 200),
        plotOutput("scatterCollegeIncome", height = 250)
      ),

      tags$div(id="cite",
        'Data compiled for ', tags$em('HackYeah2018'), ' by M.Sykulski, B.Poszewiecka, J.Grabowski.'
      )
    )
  ),

  tabPanel("Data explorer",
    fluidRow(
      column(3,
        selectInput("states", "Regions", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
        )
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
        )
      )
    ),
    fluidRow(
      column(1,
        numericInput("minScore", "Min score", min=0, max=100, value=0)
      ),
      column(1,
        numericInput("maxScore", "Max score", min=0, max=100, value=100)
      )
    ),
    hr(),
    DT::dataTableOutput("ziptable")
  ),

  conditionalPanel("false", icon("crosshair"))
)