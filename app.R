
library(leaflet)
library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(hrbrthemes)
library(viridis)

library(shinyjs)
library(crosstalk)
library(shinythemes)

library(fmsb)
library(tidyverse)
library(fmsb)
library(reshape)
library(reshape2)

library(rvest)
library(httr)
library(jsonlite)
library(stringr)
library(rsconnect)



load('terrazas.Rda')

terrazas_select = terrazas %>% 
  select(id_terraza, id_local, desc_distrito_local, desc_barrio_local, Cod_Postal,
         coordenada_x_local, coordenada_y_local, rotulo, desc_periodo_terraza, 
         Superficie_ES, desc_ubicacion_terraza, hora_ini_LJ_es, hora_fin_LJ_es,
         direccion, telefono, price_level, resena_media, total_rating, foto, 
         url, ultimas_resenas, longitud, latitud, mesas_es, sillas_es)


ui <- navbarPage(
    
    'Qué hacer en Madrid', theme = shinytheme('cosmo'),
    
    tabPanel('Ocio Madrid',
             
             sidebarLayout(
                 sidebarPanel(img(src = 'Captura.PNG',height=200, width=500),
                              h3("Terrazas"),
                              selectInput(
                                  inputId = 'barrio',
                                  label = 'Barrio',
                                  choices = sort(unique(terrazas_select$desc_distrito_local)),
                                  multiple = TRUE,
                                  selected = "SALAMANCA"
                                  
                                  
                              ),
                              
                              
                              selectInput(
                                  inputId = 'calefac',
                                  label = 'Acondicionada para el invierno',
                                  choices = c("SI" = "Anual", "No" = "Estacional"),
                                  multiple = TRUE,
                                  selected = "Anual"
                              ),
                              
                              selectInput(
                                  inputId = 'precio',
                                  label = 'Price Rating',
                                  choices = c("Barato" = 1, "Precio Medio" = 2, "Caro" = 3,
                                              "Muy caro" = 4),
                                  multiple = TRUE,
                                  selected = 1
                              ),
                              
                              sliderInput(inputId = "total_reseña",
                                          label = "Número de Reseñas",
                                          min(terrazas_select$total_rating), max(terrazas_select$total_rating),
                                          value = range(terrazas_select$total_rating), step = 25),
                              
                              
                              
                              
                              sliderInput(inputId = "cal_resena",
                                          label = "Calificación Reseña:",
                                          min(terrazas_select$resena_media), max(terrazas_select$resena_media),
                                          value = range(terrazas_select$resena_media), step = 0.25),
                              h3("Actividades Culturales (proximamente)"),
                              selectInput(
                                  inputId = 'gratis',
                                  label = 'Actividad gratuita',
                                  choices = c("Gratis" = 1, "No gratuito" = 0),
                                  multiple = TRUE,
                                  selected = 0
                              )), mainPanel = 
                     
                     
                     mainPanel(
                         # tabsetPanel(type = 'tab',
                         # tabPanel('Gráfico', plotOutput(outputId = 'grafico_tiempo', height = 700,width = 700) ),
                         # tabPanel('Tiempo Ahora', dataTableOutput(outputId = 'table'))
                         
                         leafletOutput(outputId = 'map', height = 695)
                         
                     )), 
             
             fluidRow(
                 tabsetPanel(type = 'tab',
                             tabPanel("Selección", fluidRow(
                                 column(8, align="right",plotOutput(outputId = 'reseñas', height = 1000, width = 1000,
                                                                    click = 'plot_click'))
                                 
                             )
                             )
                             ,
                             tabPanel('Ver todos individualmente', 
                                      
                                      plotOutput(outputId = 'reseñas_mini1', width = 600, height = 600),
                                      plotOutput(outputId = 'reseñas_mini2', width = 600, height = 600),
                                      plotOutput(outputId = 'reseñas_mini3', width = 600, height = 600),
                                      plotOutput(outputId = 'reseñas_mini4', width = 600, height = 600),
                                      plotOutput(outputId = 'reseñas_mini5', width = 600, height = 600),
                                      plotOutput(outputId = 'reseñas_mini6', width = 600, height = 600),
                                      plotOutput(outputId = 'reseñas_mini7', width = 600, height = 600),
                                      plotOutput(outputId = 'reseñas_mini8', width = 600, height = 600),
                                      plotOutput(outputId = 'reseñas_mini9', width = 600, height = 600),
                                      plotOutput(outputId = 'reseñas_mini10', width = 600, height = 600),
                                      plotOutput(outputId = 'reseñas_mini11', width = 600, height = 600),
                                      plotOutput(outputId = 'reseñas_mini12', width = 600, height = 600),
                                      plotOutput(outputId = 'reseñas_mini13', width = 600, height = 600),
                                      plotOutput(outputId = 'reseñas_mini14', width = 600, height = 600),
                                      plotOutput(outputId = 'reseñas_mini15', width = 600, height = 600),
                                      plotOutput(outputId = 'reseñas_mini16', width = 600, height = 600),
                                      plotOutput(outputId = 'reseñas_mini17', width = 600, height = 600),
                                      plotOutput(outputId = 'reseñas_mini18', width = 600, height = 600),
                                      plotOutput(outputId = 'reseñas_mini19', width = 600, height = 600),
                                      plotOutput(outputId = 'reseñas_mini20', width = 600, height = 600),
                                      plotOutput(outputId = 'reseñas_mini21', width = 600, height = 600)))
             )
             
             
             
             
             
             
    ),
    tabPanel('El tiempo',
             mainPanel(
                 plotOutput(outputId = 'grafico_tiempo', width = 2000, height = 600),
                 dataTableOutput(outputId = 'table', width = 1500))
    ),
    
    
    tabPanel('Conclusiones',
             fluidRow(
                 wellPanel(
                     h3('Conclusiones'),
                     htmlOutput('texto_conc'),
                     
                     tabsetPanel(type = 'tab',tabPanel(title = 'Reseña Media',
                                                       plotlyOutput(outputId = 'reseña_media_df', width = 1000, height = 750)),
                                 tabPanel(title = 'Reseña Total',
                                          plotlyOutput(outputId = 'reseña_total_df', width = 1000, height = 750)),
                                 tabPanel(title = 'Mesas',
                                          plotlyOutput(outputId = 'reseña_mesas_df', width = 1000, height = 750)),
                                 
                                 tabPanel(title = 'Sillas',
                                          plotlyOutput(outputId = 'reseña_sillas_df', width = 1000, height = 750)),
                                 
                                 tabPanel(title = "Summary", tableOutput(outputId = 'summary')))
                     
                 )
             )
             
    )
    
)



server <- function(input, output, session) {
    
    # Forecast tiempo
    

    
    
    #-------------------------------------------------------------------------------
    key_1 = "e6ccca7bc87e0fd7db0c9f8fa5e5c515"
    #-------------------------------------------------------------------------------
    
    codigo_postal = c("28028",
                      "28021",
                      "28022",
                      "28051",
                      "28045",
                      "28041",
                      "28016",
                      "28013",
                      "28022",
                      "28040",
                      "28042",
                      "28053",
                      "28052",
                      "28025",
                      "28017",
                      "28009",
                      "28030",
                      "28012",
                      "28039",
                      "28003",
                      "28055")
    
    url_parte_1 = "http://api.openweathermap.org/data/2.5/forecast?zip="
    
    url_parte_2 = ",es&appid=e6ccca7bc87e0fd7db0c9f8fa5e5c515"
    #-------------------------------------------------------------------------------
    
    listofdfs <- list()
    
    
    for (i in seq_along(codigo_postal)){
        
        pars = GET(paste(url_parte_1, codigo_postal[i], url_parte_2, sep = ""))
        
        form_json = fromJSON(rawToChar(pars$content))
        
        tabla = data.frame(
            
            "dia_hora" = form_json[["list"]][["dt_txt"]],
            "temperatura_media" = form_json[["list"]][["main"]][["temp"]] - 273.15,
            "temperatura_max" = form_json[["list"]][["main"]][["temp_max"]] - 273.15,
            "temperatura_min" = form_json[["list"]][["main"]][["temp_min"]] - 273.15,
            "sensacion_termica" = form_json[["list"]][["main"]][["feels_like"]] - 273.15,
            "nubes" = form_json[["list"]][["clouds"]][["all"]],
            "velocidad_viento" = form_json[["list"]][["wind"]][["speed"]] * 3.6
            
        )
        
        listofdfs[[i]] <- tabla # save your dataframes into the list
        
    }
    
    names(listofdfs) = c("SALAMANCA",
                         "VILLAVERDE",
                         "SAN BLAS-CANILLEJAS",
                         "VILLA DE VALLECAS",
                         "ARGANZUELA",
                         "USERA",
                         "CHAMARTIN",
                         "CENTRO" ,
                         "FUENCARRAL-EL PARDO",
                         "MONCLOA-ARAVACA",
                         "BARAJAS",
                         "PUENTE DE VALLECAS",
                         "VICALVARO",
                         "CARABANCHEL",
                         "CIUDAD LINEAL",
                         "RETIRO",
                         "MORATALAZ",
                         "LATINA" ,
                         "TETUAN" ,
                         "CHAMBERI",
                         "HORTALEZA")
    
    save(listofdfs, file = "forecast_tiempo.Rda")
    
    
    
    c("SALAMANCA",
      "VILLAVERDE",
      "SAN BLAS-CANILLEJAS",
      "VILLA DE VALLECAS",
      "ARGANZUELA",
      "USERA",
      "CHAMARTIN",
      "CENTRO" ,
      "FUENCARRAL-EL PARDO",
      "MONCLOA-ARAVACA",
      "BARAJAS",
      "PUENTE DE VALLECAS",
      "VICALVARO",
      "CARABANCHEL",
      "CIUDAD LINEAL",
      "RETIRO",
      "MORATALAZ",
      "LATINA" ,
      "TETUAN" ,
      "CHAMBERI",
      "HORTALEZA")
    
    
    
    #-------------------------------------------------------------------------------
    
    # Tiempo actual. 
    
    distritos = c("SALAMANCA",
                  "VILLAVERDE",
                  "SAN BLAS-CANILLEJAS",
                  "VILLA DE VALLECAS",
                  "ARGANZUELA",
                  "USERA",
                  "CHAMARTIN",
                  "CENTRO" ,
                  "FUENCARRAL-EL PARDO",
                  "MONCLOA-ARAVACA",
                  "BARAJAS",
                  "PUENTE DE VALLECAS",
                  "VICALVARO",
                  "CARABANCHEL",
                  "CIUDAD LINEAL",
                  "RETIRO",
                  "MORATALAZ",
                  "LATINA" ,
                  "TETUAN" ,
                  "CHAMBERI",
                  "HORTALEZA")
    
    actual_parte_1 = 'http://api.openweathermap.org/data/2.5/weather?zip='
    actual_parte_2 = ',es&appid=e6ccca7bc87e0fd7db0c9f8fa5e5c515'
    
    
    actual <- list()
    
    
    for (i in seq_along(codigo_postal)){
        
        pars = GET(paste(actual_parte_1, codigo_postal[i], actual_parte_2, sep = ""))
        
        form_json = fromJSON(rawToChar(pars$content))
        
        tabla = data.frame(
            "descripción" = form_json$weather$description,
            "temperatura_media" = paste(round(form_json$main$temp- 273.15,2) ,"Cº"),
            "temperatura_max" = paste(round(form_json$main$temp_max-273.15,2) ,"Cº"),
            "temperatura_min" = paste(round(form_json$main$temp_min - 273.15, 2), "Cº"),
            "sensacion_termica" = paste(round(form_json$main$feels_like-273.15,2) , "Cº"),
            "nubes" = form_json$clouds$all,
            "velocidad_viento" = paste(form_json$wind$speed *3.6, "Km/h"),
            "barrio" = distritos[i]
            
        )
        
        actual[[i]] <- tabla # save your dataframes into the list
        
    }
    
    rbind(
        as.data.frame(actual[["SALAMANCA"]]),
        as.data.frame(actual[["LATINA"]]))
    
    names(actual) = c("SALAMANCA",
                      "VILLAVERDE",
                      "SAN BLAS-CANILLEJAS",
                      "VILLA DE VALLECAS",
                      "ARGANZUELA",
                      "USERA",
                      "CHAMARTIN",
                      "CENTRO" ,
                      "FUENCARRAL-EL PARDO",
                      "MONCLOA-ARAVACA",
                      "BARAJAS",
                      "PUENTE DE VALLECAS",
                      "VICALVARO",
                      "CARABANCHEL",
                      "CIUDAD LINEAL",
                      "RETIRO",
                      "MORATALAZ",
                      "LATINA" ,
                      "TETUAN" ,
                      "CHAMBERI",
                      "HORTALEZA")
    #-------------------------------------------------------------------------------
    
    
    #Actividades Culturales
    
    url = GET("https://datos.madrid.es/egob/catalogo/206974-0-agenda-eventos-culturales-100.json")
    
    datos = fromJSON(rawToChar(url$content))
    
    prueba = data.frame("titulo_actos" = datos[["@graph"]][["title"]],
                        
                        "long_actos" = datos[["@graph"]][["location"]][["longitude"]],
                        
                        "lat_actos" = datos[["@graph"]][["location"]][["latitude"]],
                        
                        "gratis_actos" = datos[["@graph"]][["free"]],
                        
                        "hora_actos" = datos[["@graph"]][["dtstart"]],
                        
                        "location_actos" = datos[["@graph"]][["event-location"]],
                        
                        "descripcion" = datos[["@graph"]][["description"]],
                        
                        "url_barrio" = datos[["@graph"]][["address"]][["district"]][["@id"]],
                        
                        "direccion_actos" = datos[["@graph"]][["address"]][["area"]][["street-address"]]
                        ,
                        "link" = datos[["@graph"]][["link"]])
    
    
    prueba = prueba[sapply(prueba$long_actos, is.numeric),]
    prueba = prueba[sapply(prueba$lat_actos, is.numeric),]
    prueba = prueba[c(which( !is.na(prueba$url_barrio), arr.ind=TRUE)),]
    
    
    
    barrio_list = c()
    for(i in prueba$url_barrio){
        barrio = toupper(strsplit(i, split = "/")[[1]][11])
        barrio_list = c(barrio_list, barrio)
    }
    
    prueba$barrio = barrio_list
    
    
    
    
    
    
    #-------------------------------------------------------------------------------
    
    dgr_fmt <- function(x, ...) {
        parse(text = paste(x, "*degree", sep = ""))
    }
    
    #-------------------------------------------------------------------------------
    metricas_terrazas_zip = terrazas_select %>% 
        select(sillas_es, mesas_es, resena_media, total_rating, price_level, desc_distrito_local) %>% 
        group_by(desc_distrito_local) %>% 
        summarize(sillas = mean(sillas_es), mesas = mean(mesas_es), 
                  reseña_media = mean(resena_media),
                  reseñas_totales = mean(total_rating),
                  precio = mean(price_level))
    
    
    # Añadimos las metricas maximas para cada columna
    
    metricas_terrazas_zip$sillas = as.numeric(metricas_terrazas_zip$sillas)
    metricas_terrazas_zip$mesas = as.numeric(metricas_terrazas_zip$mesas)
    metricas_terrazas_zip$reseña_media = as.numeric(metricas_terrazas_zip$reseña_media)
    metricas_terrazas_zip$reseñas_totales = as.numeric(metricas_terrazas_zip$reseñas_totales)
    metricas_terrazas_zip$precio = as.numeric(metricas_terrazas_zip$precio)
    
    x <- rep(c("MEDIA" ,
               mean(metricas_terrazas_zip$sillas),
               mean(metricas_terrazas_zip$mesas),
               mean(metricas_terrazas_zip$reseña_media),
               mean(metricas_terrazas_zip$reseñas_totales),
               mean(metricas_terrazas_zip$precio)), ncol(metricas_terrazas_zip))
    
    metricas_terrazas_zip <- rbind(x, metricas_terrazas_zip)
    
    
    metricas_terrazas_zip$sillas = as.numeric(metricas_terrazas_zip$sillas)
    metricas_terrazas_zip$mesas = as.numeric(metricas_terrazas_zip$mesas)
    metricas_terrazas_zip$reseña_media = as.numeric(metricas_terrazas_zip$reseña_media)
    metricas_terrazas_zip$reseñas_totales = as.numeric(metricas_terrazas_zip$reseñas_totales)
    metricas_terrazas_zip$precio = as.numeric(metricas_terrazas_zip$precio)
    x <- rep(c("MINIMO" ,
               min(metricas_terrazas_zip$sillas),
               min(metricas_terrazas_zip$mesas),
               min(metricas_terrazas_zip$reseña_media),
               min(metricas_terrazas_zip$reseñas_totales),
               min(metricas_terrazas_zip$precio)), ncol(metricas_terrazas_zip))
    
    metricas_terrazas_zip <- rbind(x, metricas_terrazas_zip)
    
    
    
    
    
    
    metricas_terrazas_zip$sillas = as.numeric(metricas_terrazas_zip$sillas)
    metricas_terrazas_zip$mesas = as.numeric(metricas_terrazas_zip$mesas)
    metricas_terrazas_zip$reseña_media = as.numeric(metricas_terrazas_zip$reseña_media)
    metricas_terrazas_zip$reseñas_totales = as.numeric(metricas_terrazas_zip$reseñas_totales)
    metricas_terrazas_zip$precio = as.numeric(metricas_terrazas_zip$precio)
    
    
    x <- rep(c("MAXIMO" ,
               max(metricas_terrazas_zip$sillas),
               max(metricas_terrazas_zip$mesas),
               max(metricas_terrazas_zip$reseña_media),
               max(metricas_terrazas_zip$reseñas_totales),
               max(metricas_terrazas_zip$precio)), ncol(metricas_terrazas_zip))
    
    metricas_terrazas_zip <- rbind(x, metricas_terrazas_zip)
    
    
    metricas_terrazas_zip$sillas = as.numeric(metricas_terrazas_zip$sillas)
    metricas_terrazas_zip$mesas = as.numeric(metricas_terrazas_zip$mesas)
    metricas_terrazas_zip$reseña_media = as.numeric(metricas_terrazas_zip$reseña_media)
    metricas_terrazas_zip$reseñas_totales = as.numeric(metricas_terrazas_zip$reseñas_totales)
    metricas_terrazas_zip$precio = as.numeric(metricas_terrazas_zip$precio)
    
    
    
    ###############################################################################
    
    #-------------------------------------------------------------------------------
    #POP UP
    
    terrazas_select = terrazas_select %>% 
        mutate(popup_info = paste( sep = "<br/>",
                                   "<strong>Nombre:</strong>", rotulo,  "<strong>Dirección:</strong>", direccion,
                                   "<strong>Teléfono:</strong>", telefono, "<strong>Hora Apertura:</strong>",
                                   hora_ini_LJ_es, "<strong>Hora Cierre:</strong>", hora_fin_LJ_es))
    
    
    prueba = prueba %>% 
        mutate(popup_info = paste( sep = "<br/>",
                                   "<strong>Nombre:</strong>", titulo_actos,  "<strong>Hora de inicio:</strong>", hora_actos,
                                   "<strong>Localización:</strong>", location_actos, "<strong>Dirección:</strong>",
                                   direccion_actos,
                                   "<strong>Comentarios:</strong>", descripcion))
    
    
    
    
    output$reseñas = renderPlot({
        
        colors_fill = c(scales::alpha("black", 0.2),
                        scales::alpha("gold", 0.2),
                        scales::alpha("tomato", 0.2),
                        scales::alpha("skyblue", 0.2),
                        scales::alpha("deepskyblue3", 0.2),
                        scales::alpha("deeppink", 0.2),
                        scales::alpha("darkcyan", 0.2),
                        scales::alpha("red", 0.2),
                        scales::alpha("floralwhite", 0.2),
                        scales::alpha("grey", 0.2),
                        scales::alpha("plum1", 0.2),
                        scales::alpha("green1", 0.2),
                        scales::alpha("magenta1", 0.2),
                        scales::alpha("azure", 0.2),
                        scales::alpha("chocolate", 0.2),
                        scales::alpha("darkblue", 0.2),
                        scales::alpha("brown2", 0.2),
                        scales::alpha("lightgray", 0.2),
                        scales::alpha("red4", 0.2),
                        scales::alpha("midnightblue", 0.2))
        
        
        
        
        
        colors_line = c(scales::alpha("black", 0.6),
                        scales::alpha("gold", 0.6),
                        scales::alpha("tomato", 0.6),
                        scales::alpha("skyblue", 0.6),
                        scales::alpha("deepskyblue3", 0.6),
                        scales::alpha("deeppink", 0.6),
                        scales::alpha("darkcyan", 0.6),
                        scales::alpha("red", 0.6),
                        scales::alpha("floralwhite", 0.6),
                        scales::alpha("grey", 0.6),
                        scales::alpha("plum1", 0.6),
                        scales::alpha("green1", 0.6),
                        scales::alpha("magenta1", 0.6),
                        scales::alpha("azure", 0.6),
                        scales::alpha("chocolate", 0.6),
                        scales::alpha("darkblue", 0.6),
                        scales::alpha("brown2", 0.6),
                        scales::alpha("lightgray", 0.6),
                        scales::alpha("red4", 0.6),
                        scales::alpha("midnightblue", 0.6))
        
        
        radarchart(metricas_terrazas_zip[ metricas_terrazas_zip$desc_distrito_local %in% c("MAXIMO", "MINIMO",
                                                                                           "MEDIA",
                                                                                           input$barrio),c(2,3,4,5,6)],
                   title = "Métricas por Barrio",plty = 1, 
                   pcol = colors_line, pfcol = colors_fill, plwd = length(input$barrio) + 1, axistype = 1 ,seg = 5, 
                   vlabels=c("Número de Sillas", "Número de Mesas", "Reseña Media", "Reseñas Totales", "Precio"
                   ), 
                   pangle=c(10, 45, 120)
                   
        )
        legend(x=0.6, 
               y=1.35, 
               legend = c("Media",input$barrio), 
               bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
        
    })
    
    colors_fill = c(scales::alpha("black", 0.2),
                    scales::alpha("gold", 0.2),
                    scales::alpha("tomato", 0.2),
                    scales::alpha("skyblue", 0.2),
                    scales::alpha("deepskyblue3", 0.2),
                    scales::alpha("deeppink", 0.2),
                    scales::alpha("darkcyan", 0.2),
                    scales::alpha("red", 0.2),
                    scales::alpha("floralwhite", 0.2),
                    scales::alpha("grey", 0.2),
                    scales::alpha("plum1", 0.2),
                    scales::alpha("green1", 0.2),
                    scales::alpha("magenta1", 0.2),
                    scales::alpha("azure", 0.2),
                    scales::alpha("chocolate", 0.2),
                    scales::alpha("darkblue", 0.2),
                    scales::alpha("brown2", 0.2),
                    scales::alpha("lightgray", 0.2),
                    scales::alpha("red4", 0.2),
                    scales::alpha("midnightblue", 0.2))
    
    
    
    
    
    colors_line = c(scales::alpha("black", 0.6),
                    scales::alpha("gold", 0.6),
                    scales::alpha("tomato", 0.6),
                    scales::alpha("skyblue", 0.6),
                    scales::alpha("deepskyblue3", 0.6),
                    scales::alpha("deeppink", 0.6),
                    scales::alpha("darkcyan", 0.6),
                    scales::alpha("red", 0.6),
                    scales::alpha("floralwhite", 0.6),
                    scales::alpha("grey", 0.6),
                    scales::alpha("plum1", 0.6),
                    scales::alpha("green1", 0.6),
                    scales::alpha("magenta1", 0.6),
                    scales::alpha("azure", 0.6),
                    scales::alpha("chocolate", 0.6),
                    scales::alpha("darkblue", 0.2),
                    scales::alpha("brown2", 0.2),
                    scales::alpha("lightgray", 0.2),
                    scales::alpha("red4", 0.2),
                    scales::alpha("midnightblue", 0.2))
    
    
    output$reseñas_mini1 = renderPlot({
        if("ARGANZUELA" %in% input$barrio){
            
            radarchart(metricas_terrazas_zip[ metricas_terrazas_zip$desc_distrito_local %in% c("MAXIMO", "MINIMO",
                                                                                               "MEDIA", "ARGANZUELA"
            ),c(2,3,4,5,6)],
            title = "ARGANZUELA",
            pcol = colors_line[c(1,5)], pfcol = colors_fill[c(1,5)], plwd = 2 , axistype = 1 ,vlabels=c("Número de Sillas", "Número de Mesas", "Reseña Media", "Reseñas Totales", "Precio")
            )
            legend(x=0.6, 
                   y=1.35, 
                   legend = c("Media"), 
                   bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
        }
    })
    
    output$reseñas_mini2 = renderPlot({
        if("BARAJAS" %in% input$barrio){
            
            radarchart(metricas_terrazas_zip[ metricas_terrazas_zip$desc_distrito_local %in% c("MAXIMO", "MINIMO",
                                                                                               "MEDIA", "BARAJAS"
            ),c(2,3,4,5,6)],
            title = "BARAJAS",
            pcol = colors_line[c(1,2)], pfcol = colors_fill[c(1,2)], plwd = 2 , axistype = 1 ,vlabels=c("Número de Sillas", "Número de Mesas", "Reseña Media", "Reseñas Totales", "Precio")
            )
            legend(x=0.6, 
                   y=1.35, 
                   legend = c("Media"), 
                   bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
        }
    })
    
    output$reseñas_mini3 = renderPlot({
        if("CARABANCHEL" %in% input$barrio){
            
            radarchart(metricas_terrazas_zip[ metricas_terrazas_zip$desc_distrito_local %in% c("MAXIMO", "MINIMO",
                                                                                               "MEDIA", "CARABANCHEL"
            ),c(2,3,4,5,6)],
            title = "CARABANCHEL",
            pcol = colors_line[c(1,3)], pfcol = colors_fill[c(1,3)], plwd = 2 , axistype = 1 ,vlabels=c("Número de Sillas", "Número de Mesas", "Reseña Media", "Reseñas Totales", "Precio")
            )
            legend(x=0.6, 
                   y=1.35, 
                   legend = c("Media"), 
                   bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
        }
    })
    
    output$reseñas_mini4 = renderPlot({
        if("CENTRO" %in% input$barrio){
            
            radarchart(metricas_terrazas_zip[ metricas_terrazas_zip$desc_distrito_local %in% c("MAXIMO", "MINIMO",
                                                                                               "MEDIA", "CENTRO"
            ),c(2,3,4,5,6)],
            title = "CENTRO",
            pcol = colors_line[c(1,4)], pfcol = colors_fill[c(1,4)], plwd = 2 , axistype = 1 ,vlabels=c("Número de Sillas", "Número de Mesas", "Reseña Media", "Reseñas Totales", "Precio")
            )
            legend(x=0.6, 
                   y=1.35, 
                   legend = c("Media"), 
                   bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
        }
    })
    
    output$reseñas_mini5 = renderPlot({
        if("CHAMARTIN" %in% input$barrio){
            
            radarchart(metricas_terrazas_zip[ metricas_terrazas_zip$desc_distrito_local %in% c("MAXIMO", "MINIMO",
                                                                                               "MEDIA", "CHAMARTIN"
            ),c(2,3,4,5,6)],
            title = "CHAMARTIN",
            pcol = colors_line[c(1,5)], pfcol = colors_fill[c(1,5)], plwd = 2 , axistype = 1 ,vlabels=c("Número de Sillas", "Número de Mesas", "Reseña Media", "Reseñas Totales", "Precio")
            )
            legend(x=0.6, 
                   y=1.35, 
                   legend = c("Media"), 
                   bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
        }
    })
    
    
    output$reseñas_mini6 = renderPlot({
        if("CHAMBERI" %in% input$barrio){
            
            radarchart(metricas_terrazas_zip[ metricas_terrazas_zip$desc_distrito_local %in% c("MAXIMO", "MINIMO",
                                                                                               "MEDIA", "CHAMBERI"
            ),c(2,3,4,5,6)],
            title = "CHAMBERI",
            pcol = colors_line[c(1,6)], pfcol = colors_fill[c(1,6)], plwd = 2 , axistype = 1 ,vlabels=c("Número de Sillas", "Número de Mesas", "Reseña Media", "Reseñas Totales", "Precio")
            )
            legend(x=0.6, 
                   y=1.35, 
                   legend = c("Media"), 
                   bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
        }
    })
    
    output$reseñas_mini7 = renderPlot({
        if("CIUDAD LINEAL" %in% input$barrio){
            
            radarchart(metricas_terrazas_zip[ metricas_terrazas_zip$desc_distrito_local %in% c("MAXIMO", "MINIMO",
                                                                                               "MEDIA", "CIUDAD LINEAL"
            ),c(2,3,4,5,6)],
            title = "CIUDAD LINEAL",
            pcol = colors_line[c(1,7)], pfcol = colors_fill[c(1,7)], plwd = 2 , axistype = 1 ,vlabels=c("Número de Sillas", "Número de Mesas", "Reseña Media", "Reseñas Totales", "Precio")
            )
            legend(x=0.6, 
                   y=1.35, 
                   legend = c("Media"), 
                   bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
        }
    })
    
    output$reseñas_mini8 = renderPlot({
        if("FUENCARRAL-EL PARDO" %in% input$barrio){
            
            radarchart(metricas_terrazas_zip[ metricas_terrazas_zip$desc_distrito_local %in% c("MAXIMO", "MINIMO",
                                                                                               "MEDIA", "FUENCARRAL-EL PARDO"
            ),c(2,3,4,5,6)],
            title = "FUENCARRAL-EL PARDO",
            pcol = colors_line[c(1,8)], pfcol = colors_fill[c(1,8)], plwd = 2 , axistype = 1 ,vlabels=c("Número de Sillas", "Número de Mesas", "Reseña Media", "Reseñas Totales", "Precio")
            )
            legend(x=0.6, 
                   y=1.35, 
                   legend = c("Media"), 
                   bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
        }
    })
    
    output$reseñas_mini9 = renderPlot({
        if("HORTALEZA" %in% input$barrio){
            
            radarchart(metricas_terrazas_zip[ metricas_terrazas_zip$desc_distrito_local %in% c("MAXIMO", "MINIMO",
                                                                                               "MEDIA", "HORTALEZA"
            ),c(2,3,4,5,6)],
            title = "HORTALEZA",
            pcol = colors_line[c(1,9)], pfcol = colors_fill[c(1,9)], plwd = 2 , axistype = 1 ,vlabels=c("Número de Sillas", "Número de Mesas", "Reseña Media", "Reseñas Totales", "Precio")
            )
            legend(x=0.6, 
                   y=1.35, 
                   legend = c("Media"), 
                   bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
        }
    })
    
    output$reseñas_mini10 = renderPlot({
        if("LATINA" %in% input$barrio){
            
            radarchart(metricas_terrazas_zip[ metricas_terrazas_zip$desc_distrito_local %in% c("MAXIMO", "MINIMO",
                                                                                               "MEDIA", "LATINA"
            ),c(2,3,4,5,6)],
            title = "LATINA",
            pcol = colors_line[c(1,10)], pfcol = colors_fill[c(1,10)], plwd = 2 , axistype = 1 ,vlabels=c("Número de Sillas", "Número de Mesas", "Reseña Media", "Reseñas Totales", "Precio")
            )
            legend(x=0.6, 
                   y=1.35, 
                   legend = c("Media"), 
                   bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
        }
    })
    
    output$reseñas_mini11 = renderPlot({
        if("MONCLOA-ARAVACA" %in% input$barrio){
            
            radarchart(metricas_terrazas_zip[ metricas_terrazas_zip$desc_distrito_local %in% c("MAXIMO", "MINIMO",
                                                                                               "MEDIA", "MONCLOA-ARAVACA"
            ),c(2,3,4,5,6)],
            title = "MONCLOA-ARAVACA",
            pcol = colors_line[c(1,11)], pfcol = colors_fill[c(1,11)], plwd = 2 , axistype = 1 ,vlabels=c("Número de Sillas", "Número de Mesas", "Reseña Media", "Reseñas Totales", "Precio")
            )
            legend(x=0.6, 
                   y=1.35, 
                   legend = c("Media"), 
                   bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
        }
    })
    
    output$reseñas_mini12 = renderPlot({
        if("MORATALAZ" %in% input$barrio){
            
            radarchart(metricas_terrazas_zip[ metricas_terrazas_zip$desc_distrito_local %in% c("MAXIMO", "MINIMO",
                                                                                               "MEDIA", "MORATALAZ"
            ),c(2,3,4,5,6)],
            title = "MORATALAZ",
            pcol = colors_line[c(1,12)], pfcol = colors_fill[c(1,12)], plwd = 2 , axistype = 1 ,vlabels=c("Número de Sillas", "Número de Mesas", "Reseña Media", "Reseñas Totales", "Precio")
            )
            legend(x=0.6, 
                   y=1.35, 
                   legend = c("Media"), 
                   bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
        }
    })
    
    output$reseñas_mini13 = renderPlot({
        if("PUENTE DE VALLECAS" %in% input$barrio){
            
            radarchart(metricas_terrazas_zip[ metricas_terrazas_zip$desc_distrito_local %in% c("MAXIMO", "MINIMO",
                                                                                               "MEDIA", "PUENTE DE VALLECAS"
            ),c(2,3,4,5,6)],
            title = "PUENTE DE VALLECAS",
            pcol = colors_line[c(1,13)], pfcol = colors_fill[c(1,13)], plwd = 2 , axistype = 1 ,vlabels=c("Número de Sillas", "Número de Mesas", "Reseña Media", "Reseñas Totales", "Precio")
            )
            legend(x=0.6, 
                   y=1.35, 
                   legend = c("Media"), 
                   bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
        }
    })
    
    output$reseñas_mini14 = renderPlot({
        if("RETIRO" %in% input$barrio){
            
            radarchart(metricas_terrazas_zip[ metricas_terrazas_zip$desc_distrito_local %in% c("MAXIMO", "MINIMO",
                                                                                               "MEDIA", "RETIRO"
            ),c(2,3,4,5,6)],
            title = "RETIRO",
            pcol = colors_line[c(1,14)], pfcol = colors_fill[c(1,14)], plwd = 2 , axistype = 1 ,vlabels=c("Número de Sillas", "Número de Mesas", "Reseña Media", "Reseñas Totales", "Precio")
            )
            legend(x=0.6, 
                   y=1.35, 
                   legend = c("Media"), 
                   bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
        }
    })
    
    output$reseñas_mini15 = renderPlot({
        if("SALAMANCA" %in% input$barrio){
            
            radarchart(metricas_terrazas_zip[ metricas_terrazas_zip$desc_distrito_local %in% c("MAXIMO", "MINIMO",
                                                                                               "MEDIA", "SALAMANCA"
            ),c(2,3,4,5,6)],
            title = "SALAMANCA",
            pcol = colors_line[c(1,15)], pfcol = colors_fill[c(1,15)], plwd = 2 , axistype = 1 ,vlabels=c("Número de Sillas", "Número de Mesas", "Reseña Media", "Reseñas Totales", "Precio")
            )
        }
    })
    
    output$reseñas_mini16 = renderPlot({
        if("SAN BLAS-CANILLEJAS" %in% input$barrio){
            
            radarchart(metricas_terrazas_zip[ metricas_terrazas_zip$desc_distrito_local %in% c("MAXIMO", "MINIMO",
                                                                                               "MEDIA", "SAN BLAS-CANILLEJAS"
            ),c(2,3,4,5,6)],
            title = "SAN BLAS-CANILLEJAS",
            pcol = colors_line[c(1,16)], pfcol = colors_fill[c(1,16)], plwd = 2 , axistype = 1 ,vlabels=c("Número de Sillas", "Número de Mesas", "Reseña Media", "Reseñas Totales", "Precio")
            )
            legend(x=0.6, 
                   y=1.35, 
                   legend = c("Media"), 
                   bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
        }
    })
    
    output$reseñas_mini17 = renderPlot({
        if("TETUAN" %in% input$barrio){
            
            radarchart(metricas_terrazas_zip[ metricas_terrazas_zip$desc_distrito_local %in% c("MAXIMO", "MINIMO",
                                                                                               "MEDIA", "TETUAN"
            ),c(2,3,4,5,6)],
            title = "TETUAN",
            pcol = colors_line[c(1,17)], pfcol = colors_fill[c(1,17)], plwd = 2 , axistype = 1 ,vlabels=c("Número de Sillas", "Número de Mesas", "Reseña Media", "Reseñas Totales", "Precio")
            )
            legend(x=0.6, 
                   y=1.35, 
                   legend = c("Media"), 
                   bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
        }
    })
    
    output$reseñas_mini18 = renderPlot({
        if("USERA" %in% input$barrio){
            
            radarchart(metricas_terrazas_zip[ metricas_terrazas_zip$desc_distrito_local %in% c("MAXIMO", "MINIMO",
                                                                                               "MEDIA", "USERA"
            ),c(2,3,4,5,6)],
            title = "USERA",
            pcol = colors_line[c(1,18)], pfcol = colors_fill[c(1,18)], plwd = 2 , axistype = 1 ,vlabels=c("Número de Sillas", "Número de Mesas", "Reseña Media", "Reseñas Totales", "Precio")
            )
        }
    })
    
    output$reseñas_mini19 = renderPlot({
        if("VICALVARO" %in% input$barrio){
            
            radarchart(metricas_terrazas_zip[ metricas_terrazas_zip$desc_distrito_local %in% c("MAXIMO", "MINIMO",
                                                                                               "MEDIA", "VICALVARO"
            ),c(2,3,4,5,6)],
            title = "VICALVARO",
            pcol = colors_line[c(1,19)], pfcol = colors_fill[c(1,19)], plwd = 2 , axistype = 1 ,vlabels=c("Número de Sillas", "Número de Mesas", "Reseña Media", "Reseñas Totales", "Precio")
            )
            legend(x=0.6, 
                   y=1.35, 
                   legend = c("Media"), 
                   bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
        }
    })
    
    
    output$reseñas_mini20 = renderPlot({
        if("VILLA DE VALLECAS" %in% input$barrio){
            
            radarchart(metricas_terrazas_zip[ metricas_terrazas_zip$desc_distrito_local %in% c("MAXIMO", "MINIMO",
                                                                                               "MEDIA", "VILLA DE VALLECAS"
            ),c(2,3,4,5,6)],
            title = "VILLA DE VALLECAS",
            pcol = colors_line[c(1,3)], pfcol = colors_fill[c(1,3)], plwd = 2 , axistype = 1 ,vlabels=c("Número de Sillas", "Número de Mesas", "Reseña Media", "Reseñas Totales", "Precio")
            )
            legend(x=0.6, 
                   y=1.35, 
                   legend = c("Media"), 
                   bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
        }
    })
    
    output$reseñas_mini21 = renderPlot({
        if("VILLAVERDE" %in% input$barrio){
            
            radarchart(metricas_terrazas_zip[ metricas_terrazas_zip$desc_distrito_local %in% c("MAXIMO", "MINIMO",
                                                                                               "MEDIA", "VILLAVERDE"
            ),c(2,3,4,5,6)],
            title = "VILLAVERDE",
            pcol = colors_line[c(1,7)], pfcol = colors_fill[c(1,7)], plwd = 2 , axistype = 1 ,vlabels=c("Número de Sillas", "Número de Mesas", "Reseña Media", "Reseñas Totales", "Precio")
            )
            legend(x=0.6, 
                   y=1.35, 
                   legend = c("Media"), 
                   bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
            
        }
    })
    
    
    
    
    
    
    
    
    output$table = renderDataTable({
        rbind(
            as.data.frame(actual[[input$barrio[1]]]),
            as.data.frame(actual[[input$barrio[2]]]),
            as.data.frame(actual[[input$barrio[3]]]),
            as.data.frame(actual[[input$barrio[4]]]),
            as.data.frame(actual[[input$barrio[5]]]),
            as.data.frame(actual[[input$barrio[6]]]),
            as.data.frame(actual[[input$barrio[7]]]),
            as.data.frame(actual[[input$barrio[8]]]),
            as.data.frame(actual[[input$barrio[9]]]),
            as.data.frame(actual[[input$barrio[10]]]),
            as.data.frame(actual[[input$barrio[11]]]),
            as.data.frame(actual[[input$barrio[12]]]),
            as.data.frame(actual[[input$barrio[13]]]),
            as.data.frame(actual[[input$barrio[14]]]),
            as.data.frame(actual[[input$barrio[15]]]),
            as.data.frame(actual[[input$barrio[16]]]),
            as.data.frame(actual[[input$barrio[17]]])
        )})
    
    pal = colorFactor("Set1", terrazas_select$desc_distrito_local)
    # 
    
    
    output$map = renderLeaflet({
        leaflet() %>% 
            addTiles() %>% 
            addProviderTiles(providers$CartoDB.Positron) %>% 
            addCircleMarkers(data = terrazas_select[terrazas_select$resena_media >= input$cal_resena[1] & terrazas_select$resena_media <=
                                                        input$cal_resena[2] & terrazas_select$desc_distrito_local %in% input$barrio &
                                                        terrazas_select$desc_periodo_terraza == input$calefac & 
                                                        terrazas_select$price_level %in% input$precio & terrazas_select$total_rating >= input$total_reseña[1] & 
                                                        terrazas_select$total_rating <=
                                                        input$total_reseña[2] ,]
                             ,lng = ~longitud, lat = ~latitud , radius = ~3, 
                             popup = ~popup_info, color = ~pal(desc_distrito_local)) %>% 
            addLegend("bottomright", pal = pal, values = input$barrio, opacity = 0.7) %>% 
            
            addMarkers(data = prueba[prueba$barrio %in% input$barrio & prueba$gratis_actos %in% input$gratis, ], 
                       lng = ~long_actos, lat = ~lat_actos, 
                       clusterOptions = markerClusterOptions(),
                       popup = ~popup_info) 
    } 
    )
    
    output$reseña_mini = renderPlot({
        ptlist <- list(pt1())
    }
    )
    
    
    
    
    output$grafico_tiempo <- renderPlot({
        
        yrng = range(listofdfs[[input$barrio[1]]]$nubes)
        
        start = listofdfs[[input$barrio[1]]][["dia_hora"]][1:39]
        
        end = listofdfs[[input$barrio[1]]][["dia_hora"]][-1]
        
        
        cielo_nublado = listofdfs[[input$barrio[1]]][["nubes"]][1:39]
        Cielo = c()
        
        for (i in cielo_nublado){
            if (i > 0){
                Cielo = c(Cielo, "Nublado")
            }
            else {
                Cielo = c(Cielo, "Despejado")
            }
            
        }
        
        df = data.frame("start" = start,
                        "end" = end,
                        "party" = Cielo)
        
        p = ggplot(listofdfs[[input$barrio[1]]], aes(x = dia_hora)) +
            geom_line(aes(y = sensacion_termica, group = 1,  linetype = "Sensación Térmica") ,size = 1.25
                      ,alpha = 0.4) +
            scale_fill_manual( "#9999CC")+
            
            geom_line(aes(y = temperatura_media, group = 1 ),colour = "black" , size = 1.85) +
            geom_bar( aes(y=velocidad_viento/2), stat="identity", size=.1, fill ="#69b3a2",  color="black", alpha=.3)+
            
            coord_cartesian(ylim = c(-10,30)) +
            scale_y_continuous(
                
                breaks = seq(-10,30, by=5), labels = dgr_fmt(seq(-10,30, by=5)),
                name = "Temperatura (Celsius °)",
                sec.axis = sec_axis(trans = ~.*2, name="Velocidad viento (Km/h)")
            )  +
            scale_x_discrete( 
                breaks = c(                       listofdfs[[input$barrio[1]]][["dia_hora"]][2], 
                                                  
                                                  listofdfs[[input$barrio[1]]][["dia_hora"]][4.5],
                                                  
                                                  listofdfs[[input$barrio[1]]][["dia_hora"]][7.5],
                                                  
                                                  listofdfs[[input$barrio[1]]][["dia_hora"]][10.5], 
                                                  
                                                  listofdfs[[input$barrio[1]]][["dia_hora"]][13.5],
                                                  
                                                  listofdfs[[input$barrio[1]]][["dia_hora"]][16.5],
                                                  
                                                  listofdfs[[input$barrio[1]]][["dia_hora"]][19.5],
                                                  
                                                  listofdfs[[input$barrio[1]]][["dia_hora"]][22.5],
                                                  
                                                  listofdfs[[input$barrio[1]]][["dia_hora"]][25.5],
                                                  
                                                  listofdfs[[input$barrio[1]]][["dia_hora"]][28.5],
                                                  
                                                  listofdfs[[input$barrio[1]]][["dia_hora"]][31.5],
                                                  
                                                  listofdfs[[input$barrio[1]]][["dia_hora"]][34.5],
                                                  
                                                  listofdfs[[input$barrio[1]]][["dia_hora"]][37.5],
                                                  
                                                  listofdfs[[input$barrio[1]]][["dia_hora"]][40.5]),
                labels = c(
                    
                    substr(listofdfs[[input$barrio[1]]][["dia_hora"]][2],  start = 6, stop = 16),
                    
                    substr(listofdfs[[input$barrio[1]]][["dia_hora"]][4.5],start = 6, stop = 16),
                    
                    substr(listofdfs[[input$barrio[1]]][["dia_hora"]][7.5],start = 6, stop = 16),
                    
                    substr(listofdfs[[input$barrio[1]]][["dia_hora"]][10.5], start = 6, stop = 16),
                    
                    substr(listofdfs[[input$barrio[1]]][["dia_hora"]][13.5],start = 6, stop = 16),
                    
                    substr(listofdfs[[input$barrio[1]]][["dia_hora"]][16.5],start = 6, stop = 16),
                    
                    substr(listofdfs[[input$barrio[1]]][["dia_hora"]][19.5],start = 6, stop = 16),
                    
                    substr(listofdfs[[input$barrio[1]]][["dia_hora"]][22.5],start = 6, stop = 16),
                    
                    substr(listofdfs[[input$barrio[1]]][["dia_hora"]][25.5],start = 6, stop = 16),
                    
                    substr(listofdfs[[input$barrio[1]]][["dia_hora"]][28.5],start = 6, stop = 16),
                    
                    substr(listofdfs[[input$barrio[1]]][["dia_hora"]][31.5],start = 6, stop = 16),
                    
                    substr(listofdfs[[input$barrio[1]]][["dia_hora"]][34.5],start = 6, stop = 16),
                    
                    substr(listofdfs[[input$barrio[1]]][["dia_hora"]][37.5],start = 6, stop = 16),
                    
                    substr(listofdfs[[input$barrio[1]]][["dia_hora"]][40.5], start = 6, stop = 16)
                    
                )) +
            
            theme(panel.background = element_blank()) +
            
            theme_ipsum() +
            theme(
                axis.title.y = element_text(color = "black", size=13),
                axis.title.y.right = element_text(color = "#69b3a2", size=13)
            )+ theme(axis.text.x = element_text(face = "bold", color = "darkblue", 
                                                size = 11, angle = 30, hjust = 1)) +
            xlab("Fecha") +
            
            geom_rect(data=df, aes(NULL, NULL, xmin = start, xmax = end, fill = Cielo),
                      ymin= -12.5, ymax=35, colour="white", size=0.5, alpha=0.2) +
            
            scale_fill_manual(values=c("Nublado" = "gray", "Despejado" = "yellow1")) +
            ggtitle("Condiciones Metereológicas") +
            theme(plot.title = element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=20))+
            theme(legend.position = "top", legend.direction = "vertical") +
            
            geom_hline(yintercept = -5, colour = "grey", linetype=1) +
            geom_hline(yintercept = -2.5, colour = "grey", linetype=1) +
            geom_hline(yintercept = 2.5, colour = "grey", linetype=1) +
            geom_hline(yintercept = 5, colour = "grey", linetype=1) +
            geom_hline(yintercept = 7.5, colour = "grey", linetype=1) +
            geom_hline(yintercept = 10, colour = "grey", linetype=1) +
            geom_hline(yintercept = 12.5, colour = "grey", linetype=1) +
            geom_hline(yintercept = 15, colour = "grey", linetype=1) +
            geom_hline(yintercept = 17.5, colour = "grey", linetype=1) +
            geom_hline(yintercept = 20, colour = "grey", linetype=1) +
            geom_hline(yintercept = 22.5, colour = "grey", linetype=1) +
            geom_hline(yintercept = 25, colour = "grey", linetype=1) +
            geom_hline(yintercept = 27.5, colour = "grey", linetype=1) +
            
            
            geom_vline(xintercept = 3, colour = "wheat4", linetype=3, size=.8) +
            geom_vline(xintercept = 6, colour = "wheat4", linetype=3, size=.8) +
            geom_vline(xintercept = 9, colour = "wheat4", linetype=3, size=.8) +
            geom_vline(xintercept = 12, colour = "wheat4", linetype=3, size=.8) +
            geom_vline(xintercept = 15, colour = "wheat4", linetype=3, size=.8) +
            geom_vline(xintercept = 18, colour = "wheat4", linetype=3, size=.8) +
            geom_vline(xintercept = 21, colour = "wheat4", linetype=3, size=.8) +
            geom_vline(xintercept = 24, colour = "wheat4", linetype=3, size=.8) +
            geom_vline(xintercept = 27, colour = "wheat4", linetype=3, size=.8) +
            geom_vline(xintercept = 30, colour = "wheat4", linetype=3, size=.8) +
            geom_vline(xintercept = 33, colour = "wheat4", linetype=3, size=.8) +
            geom_vline(xintercept = 36, colour = "wheat4", linetype=3, size=.8) +
            geom_vline(xintercept = 39, colour = "wheat4", linetype=3, size=.8)   
        
        
        print(p)
        
        
        
        
        
    })
    
    
    df_reseña_media = terrazas_select %>% 
        select(desc_distrito_local, resena_media, total_rating, sillas_es, mesas_es) %>% 
        group_by(desc_distrito_local) %>% 
        summarize(resena_media = mean(resena_media), total_rating = mean(total_rating), 
                  sillas = mean(sillas_es), mesas = mean(mesas_es))
    
    
    
    output$reseña_media_df = renderPlotly({
        
        
        
        ff = ggplot() + 
            geom_bar(data= df_reseña_media,aes(x=reorder(desc_distrito_local, -resena_media), y=resena_media, 
                                               fill=desc_distrito_local), stat='identity', position='dodge') +
            coord_flip()+
            theme(legend.position="none")+
            xlab('Barrio') + ylab('Reseña Media') +
            theme(panel.background = element_blank())
        ggplotly(ff)
    })
    
    output$reseña_total_df = renderPlotly({
        
        ff_total_rating = ggplot() + 
            geom_bar(data= df_reseña_media,aes(x=reorder(desc_distrito_local, -total_rating), y=total_rating, 
                                               fill=desc_distrito_local), stat='identity', position='dodge') +
            coord_flip()+
            theme(legend.position="none")+
            xlab('Barrio') + ylab('Número total de reseñas') +
            theme(panel.background = element_blank())
        ggplotly(ff_total_rating)
    })
    
    
    output$reseña_sillas_df = renderPlotly({
        
        ff_sillas = ggplot() + 
            geom_bar(data= df_reseña_media,aes(x=reorder(desc_distrito_local, -sillas), y=sillas, 
                                               fill=desc_distrito_local), stat='identity', position='dodge') +
            coord_flip()+
            theme(legend.position="none")+
            xlab('Barrio') + ylab('Número de Sillas') +
            theme(panel.background = element_blank())
        ggplotly(ff_sillas)
    })
    
    output$reseña_mesas_df = renderPlotly({
        
        
        ff_mesas = ggplot() + 
            geom_bar(data= df_reseña_media,aes(x=reorder(desc_distrito_local, -mesas), y=mesas, 
                                               fill=desc_distrito_local), stat='identity', position='dodge') +
            coord_flip()+
            theme(legend.position="none")+
            xlab('Barrio') + ylab('Número de Mesas') +
            theme(panel.background = element_blank())
        ggplotly(ff_mesas)
    })
    
    aa = as.data.frame(terrazas_select %>% select(resena_media, total_rating, sillas_es, mesas_es))
    
    output$summary = renderTable({
        
        summary(aa)
    })
    
    
    output$texto_conc <- renderUI({
        
        HTML(
            '<p><span style="font-size: 24px;">En el <a href="https://datos.madrid.es/portal/site/egob/menuitem.214413fe61bdd68a53318ba0a8a409a0/?vgnextoid=b07e0f7c5ff9e510VgnVCM1000008a4a900aRCRD&vgnextchannel=b07e0f7c5ff9e510VgnVCM1000008a4a900aRCRD&vgnextfmt=default">Portal de Datos abiertos de Madrid&nbsp;</a>podemos encontrar informaci&oacute;n tanto &nbsp;de las Actividades Culturales y de Ocio Municipal en los pr&oacute;ximo 100 d&iacute;as (<em>API</em>) , como de las terrazas disponibles en Madrid (<em>actualizadas en Enero de 2021, CSV</em>). &nbsp;</span></p>
<p><span style="font-size: 24px;">Adem&aacute;s hemos conectado la localizaci&oacute;n de los barrios donde se sit&uacute;an estas, con la temperatura en tiempo real para cada barrio y la predicci&oacute;n a 5 d&iacute;as (<em>cada 3 horas</em>).</span></p>
<p><span style="font-size: 24px;">&nbsp;Para enriquecer los datos proporcionados por el ayuntamiento de Madrid sobre las terrazas, usamos la API de <a href="https://developers.google.com/maps/documentation/places/web-service/search">Google Place Search</a> para saber el ID de Google asociado a cada terraza y usarlo en la API de <a href="https://developers.google.com/maps/documentation/places/web-service/details">Google Place Details</a>, para obtener las m&eacute;tricas que nos interesan.</span></p>
<p><span style="font-size: 24px;"><br></span></p>
<p><span style="font-size: 24px;">El <strong>objetivo principal de &eacute;sta herramienta</strong>, es planear con la mayor precisi&oacute;n y en base a nuestros gustos cual es el mejor plan para una tarde en Madrid. Porque, &iquest; que hay mejor que disfrutar del buen tiempo con amigos en una terraza (evitando el Covid-19) ?.</span></p>
<p><span style="font-size: 24px;">El resultado es una <strong>Dashboard</strong>, donde puedes filtrar por las m&eacute;tricas m&aacute;s determinantes: rese&ntilde;a media, rese&ntilde;as totales, barrio, terraza acondicionada para el invierno, calificaci&oacute;n del precio (<em>barato,..., muy caro</em>) . Adem&aacute;s seg&uacute;n el o los barrios que sean de tu inter&eacute;s te muestra las actividades culturales (<em>conciertos, obras teatrales, espect&aacute;culos,...</em>) cercanas, pudiendo elegir entre gratuita o no gratuita.</span></p>
<p><span style="font-size: 24px;">Algunas <strong>conclusiones </strong>comparando las terrazas por distritos son:</span></p>
<ul>
    <li><span style="font-size: 24px;">Los barrios con m&aacute;s rese&ntilde;as son los barrios m&aacute;s c&eacute;ntricos de Madrid (Centro, Salamanca y Tetu&aacute;n).</span></li>
    <li><span style="font-size: 24px;">La mayor&iacute;a de la gente pone buenas notas, la media es 4.03. Es un insight interesante a la hora de elegir terraza. Se considera que una terraza (seg&uacute;n las rese&ntilde;as de Google) est&aacute; en el tercer cuartil si su rese&ntilde;a es superior a 4.3.&nbsp;</span></li>
    <li><span style="font-size: 24px;">La media del n&uacute;mero de mesas y sillas es 8 y 33 respectivamente. Punto a tener en cuenta para buscar terraza siendo un grupo grande.</span></li>
    <li><span style="font-size: 24px;">Los barrios menos c&eacute;ntricos son los que mejores rese&ntilde;as tienen. Esto se puede deber a que de verdad son mejores terrazas, o son clientes de confianza y no turistas/transeuntes.</span></li>
    <li><span style="font-size: 24px;">Si filtramos (en el dashboard) las terrazas con una &nbsp;calificaci&oacute;n de rese&ntilde;a menor a &nbsp;3.25, vemos una concentraci&oacute;n en la Plaza Mayor (Centro), es decir, las peores terrazas de Madrid est&aacute;n ah&iacute;.</span></li>
</ul>
<p><br></p>
<p><br></p>'
        )
    })
    
    
    
}
shinyApp(ui, server)





