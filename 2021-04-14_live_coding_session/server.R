# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    datos <- reactiveValues(raw = NULL,
                            filtered = NULL,
                            plot = NULL,
                            aes = NULL)
    
    observeEvent(input$file,{
        
        data <- read_csv(input$file$datapath)
        datos$raw <- data
        print(data)
        
    })

    output$filtros <- renderUI({
        
        req(datos$raw)
        
        fluidRow(
        selectInput("cut1", h4("Filtro por corte:"), 
                    unique(datos$raw$cut), multiple = TRUE,
                    selected = "Ideal"),
        selectInput("color1", h4("Filtro por color:"), 
                    unique(datos$raw$color), multiple = TRUE,
                    selected = "E"),
        selectInput("clarity1", h4("Filtro por claridad:"), 
                    unique(datos$raw$clarity), multiple = TRUE,
                    selected = "IF"),
        numericInput("minprecio", h4("Precio minimo:"),value = min(datos$raw$price),
                     min = min(datos$raw$price), max = max(datos$raw$price)),
        numericInput("maxprecio", h4("Precio maximo:"),value = max(datos$raw$price),
                     min = min(datos$raw$price), max = max(datos$raw$price))
        )
        
    })
    
    observe({
        
        req(datos$raw, input$cut1, input$color1, 
            input$clarity1, input$minprecio, input$maxprecio)
        
        datos$filtered <- datos$raw %>%
                            filter(cut %in% input$cut1,
                                      color %in% input$color1,
                                      clarity %in% input$clarity1,
                                      price >= input$minprecio |
                                          price <= input$maxprecio)
        
    })
    
    output$preview <- renderDT({
        datos$filtered
    })
    
    output$filtro_scatter <- renderUI({
        
        req(datos$raw)
        
        fluidRow(
            selectInput("xaxis", h4("Seleccione el eje x:"), 
                        colnames(datos$raw %>% dplyr::select(where(is.numeric))),
                        selected = "carat"),
            selectInput("yaxis", h4("Seleccion el eje y:"), 
                        colnames(datos$raw %>% dplyr::select(where(is.numeric))),
                        selected = "price"),
            sliderInput("alpha", "Transparencia:",
                        min = 0, max = 1,
                        value = 1, step = 0.1),
            numericInput("shape", h4("Figura:"),
                         value = 1,
                         min = 0, max = 24),
            radioButtons("color_choose", "Tipo de dato para color:",
                         list("Variable" = 2, "Constante" = 1), selected = 1),
            colourpicker::colourInput("color", 
                                      h4("Seleccione el color:"), 
                                      value = "black"),
            selectInput("color2", h4("Seleccione el color:"), 
                        colnames(datos$raw %>% dplyr::select(-where(is.numeric))),
                        selected = "cut"),
            radioButtons("size_choose", "Tipo de dato para tamano:",
                         list("Variable" = 2, "Constante" = 1), selected = 1),
            numericInput("size", h4("Seleccione el tamano:"),
                         value = 1,
                         min = 1, max = 3, step = 0.5),
            selectInput("size2", h4("Seleccione el tamano:"), 
                        colnames(datos$raw %>% dplyr::select(-where(is.numeric))),
                        selected = "color")
        )
        
    })
    
    observe({
        
        req(input$color_choose, input$size_choose)
        
        if(input$color_choose == 1){
            hideElement("color2")
            showElement("color")
        } else{
            hideElement("color")
            showElement("color2")
        }
        
        if(input$size_choose == 1){
            hideElement("size2")
            showElement("size")
        } else{
            hideElement("size")
            showElement("size2")
        }
        
        
    })
    
    observe({

        req(input$xaxis, input$yaxis)

        datos$plot <- ggplot(datos$raw,
                             aes(x = get(input$xaxis), y = get(input$yaxis))) +
            xlab(input$xaxis) + 
            ylab(input$yaxis)

        if(input$color_choose == 1 & input$size_choose == 1){
            datos$aes <- geom_point(alpha = input$alpha,
                                    shape = input$shape,
                                    colour = input$color,
                                    size = input$size)
        } else if(input$color_choose == 1 & input$size_choose == 2){
            datos$aes <- geom_point(aes(size = get(input$size2)),
                                    alpha = input$alpha,
                                    shape = input$shape,
                                    colour = input$color)
        } else if(input$color_choose == 2 & input$size_choose == 1){
            datos$aes <- geom_point(aes(colour = get(input$color2)),
                                    alpha = input$alpha,
                                    shape = input$shape,
                                    size = input$size)
        } else{
            datos$aes <- geom_point(aes(colour = get(input$color2), 
                                        size = get(input$size2)),
                                    alpha = input$alpha,
                                    shape = input$shape)
        }

    })
    
    output$scatter <- renderPlot({
        datos$plot + datos$aes
    },  height = 600, width = 1000)

})
