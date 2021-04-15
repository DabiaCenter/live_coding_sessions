shinyUI(fluidPage(
  useShinyjs(),
    navbarPage(title = "Live Coding Session 3",
               id = "inTabset",
               selected = "Importacion de datos",
               br(),
               tabPanel(title = "Importacion de datos",
                            fluidRow(
                                sidebarPanel(
                                    fileInput("file", label = h3("Inserte los datos")),
                                    uiOutput("filtros")
                                ),
                                mainPanel(
                                    DTOutput("preview")
                                )
                            )
                    ),
               tabPanel(title = "Graficos",
                            fluidRow(
                                sidebarPanel(
                                    h4("Filtros para el grafico de dispersion"),
                                    uiOutput("filtro_scatter")
                                ),
                                mainPanel(
                                    plotOutput("scatter")
                                )
                            )
               )
            )
        )
    )
