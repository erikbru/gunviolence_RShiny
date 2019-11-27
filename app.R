library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(shinydashboard)

guns <- readRDS("guns.rds")
states <- readRDS("states.rds")

header <- dashboardHeader(title = "Gun voilence in the US 2013-2018", titleWidth=500)

sidebar <- dashboardSidebar(
  sidebarMenu(
  menuItem(h4("Dashboard"), tabName = "dashboard"),
  menuItem(h4("Full analysis"), href = "https://www.kaggle.com/erikbruin/gun-violence-in-the-us-eda-and-rshiny-app"), newtab = FALSE),
          hr(),
          h5("The default dates cover the full date range available.
          If you change the start or end date,
          the graphs will be refreshed for the selected date range", align="center"),
                            dateRangeInput(inputId = "start",
                                           label = "Select period",
                                           start = min(guns$date),
                                           end = max(guns$date),
                                           min = min(guns$date),
                                           max = max(guns$date))
                                                        )

body <- dashboardBody(
  tabItems(
    tabItem(tabName= "dashboard",
            fluidRow(
              tabBox(id = "erik", width=12, height=700,
                     tabPanel("Map",
                              br(),
                              fluidRow(h4("By hovering over the map, a label shows up which shows the name of the state and the number of incidents per 100,000 inhabitants in the selected period.
                              In addition, you can zoom in or out. By zooming out, Alaska and Hawaii also become visible.
                              Please note that the range in the legend also gets updated after a new date selection.", align='left')),
                              br(),
                              fluidRow(leafletOutput("usmap", height=700))),
                      tabPanel("Bar chart",
                              br(),
                              h4("Within the selected period, you can specify how many states you want
                              to display in the bar charts. By hovering over the charts, a label shows up which shows the exact number of incidents in the selected period.", align="left"),
                              sliderInput(inputId = "num",
                                          label = "Choose Top N states with most incidents",
                                          value = 10,
                                          min = 1,
                                          max = (n_distinct(guns$state)-1)),
                              fluidRow(plotlyOutput("plot1")),
                              fluidRow(plotlyOutput("plot2"))),
                      tabPanel("Table",
                               br(),
                               fluidRow(h4("The default sort of this table is by descending number of incidents per 100,000 inhabitants in the selected period (column Per100k).", align='left')),
                               br(),
                               fluidRow(DT::dataTableOutput(outputId="table"))))
    
    )
  )
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {

  gun_select <- reactive({guns %>% filter(date >= input$start[1] & date <= input$start[2] & state !="District of Columbia") %>%
      group_by(state, population) %>% summarise(incidents = n()) %>% ungroup() %>%
      mutate(Per100k = round((incidents/population)*100000))})


  output$plot1<-renderPlotly({
    ggplotly(gun_select() %>% top_n(input$num, wt=incidents) %>%
      ggplot(aes(x=reorder(state, incidents), y=incidents, text=state)) +
      geom_bar(stat='identity', fill='red') + coord_flip() +
      labs(x='', y='Number of incidents', title="Absolute number of incidents"),
      tooltip=c("text", "y"))
    })

  output$plot2<-renderPlotly({
    ggplotly(gun_select() %>% top_n(input$num, wt=Per100k) %>%
      ggplot(aes(x=reorder(state, Per100k), y=Per100k, fill= Per100k, text=state)) +
      geom_bar(stat='identity') + coord_flip() +
      labs(x='', y='Incidents per 100k inhabitants', title="Relative number of incidents") +
      scale_fill_gradient(low="yellow", high="red") +
      theme(legend.position="none"),
      tooltip=c("text", "y"))
  })

  output$table <- DT::renderDataTable(DT::datatable({gun_select() %>% arrange(desc(Per100k))}))

  states1 <- reactive({tigris::geo_join(states, gun_select(), "NAME", "state", how="inner")})

  state_popup <- reactive({paste0("<strong>State: </strong>",
                        states1()$NAME,
                        "<br><strong>Incidents per 100,000 inhabitants </strong>",
                        states1()$Per100k) %>%
                        lapply(htmltools::HTML)})

  pal <- reactive({colorNumeric("Reds", domain=states1()$Per100k)})

  output$usmap <- renderLeaflet({states1() %>%
                                  leaflet() %>%
                                  addProviderTiles("CartoDB.Positron") %>%
                                  setView(-98.483330, 38.712046, zoom = 4) %>%
                                  addPolygons(data = states1(),
                                              fillColor = ~pal()(states1()$Per100k),
                                              weight = 2,
                                              opacity = 1,
                                              color = "white",
                                              dashArray = "3",
                                              fillOpacity = 0.7,
                                              highlight = highlightOptions(
                                                weight = 5,
                                                color = "#666",
                                                dashArray = "",
                                                fillOpacity = 0.7,
                                                bringToFront = TRUE),
                                              label = state_popup(),
                                              labelOptions = labelOptions(
                                                style = list("font-weight" = "normal", padding = "3px 8px"),
                                                textsize = "15px",
                                                direction = "auto")) %>%
                                  addLegend(pal = pal(),
                                              values = states1()$Per100k,
                                              position = "bottomright",
                                              title = "Per100k")})

  }
#}
shinyApp(ui = ui, server = server)