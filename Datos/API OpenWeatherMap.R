library(rvest)
library(httr)
library(jsonlite)
library(stringr)
library(tidyverse)

load("terrazas.Rda")


terrazas_select = terrazas %>% 
  select(id_terraza, id_local, desc_distrito_local, desc_barrio_local, Cod_Postal,
         coordenada_x_local, coordenada_y_local, rotulo, desc_periodo_terraza, 
         Superficie_ES, desc_ubicacion_terraza, hora_ini_LJ_es, hora_fin_LJ_es,
         direccion, telefono, price_level, resena_media, total_rating, foto, 
         url, ultimas_resenas, longitud, latitud, mesas_es, sillas_es)

terrazas_select$desc_distrito_local = trimws(terrazas_select$desc_distrito_local)

terrazas_select = terrazas_select[sapply(terrazas_select$longitud, is.numeric),]

terrazas_select$latitud = unlist(terrazas_select$latitud)

terrazas_select$desc_periodo_terraza = unlist(terrazas_select$desc_periodo_terraza)

#terrazas_select$resena_media = unlist(terrazas_select$resena_media)

terrazas_select = terrazas_select[sapply(terrazas_select$resena_media, length) == 1,]
terrazas_select$resena_media = unlist(terrazas_select$resena_media)

terrazas_select = terrazas_select[sapply(terrazas_select$price_level, length) == 1,]
terrazas_select$price_level = unlist(terrazas_select$price_level)

terrazas_select$total_rating = unlist(terrazas_select$total_rating)

terrazas_select$longitud = unlist(terrazas_select$longitud)

terrazas_select = terrazas_select[terrazas_select$latitud > 36,]

terrazas_select = terrazas_select[terrazas_select$url != "https://maps.google.com/?cid=1371880154814563850",] 

terrazas_select = terrazas_select[terrazas_select$url != "https://maps.google.com/?cid=6143421280059855218",]

terrazas_select = terrazas_select[terrazas_select$url != "https://maps.google.com/?cid=8808765104563069597",]

terrazas_select = terrazas_select[terrazas_select$url != "https://maps.google.com/?cid=15911124461925012104",]

terrazas_select = terrazas_select[terrazas_select$url != "https://maps.google.com/?cid=9390165938945422051",]

terrazas_select = terrazas_select[terrazas_select$url != "https://maps.google.com/?cid=9849099961770092149",]

terrazas_select = terrazas_select[terrazas_select$url != "https://maps.google.com/?cid=13846468612514755521",]

terrazas_select = terrazas_select[terrazas_select$url != "https://maps.google.com/?cid=16191555920005545654",]

terrazas_select = terrazas_select[terrazas_select$url != "https://maps.google.com/?cid=1489116543677424853",]

terrazas_select = terrazas_select[terrazas_select$url != "https://maps.google.com/?q=Calle+el+Pino,+30509+El+Pino,+Murcia,+Spain&ftid=0xd638763711ff5c5:0xfd0d062dda6e7aa3",]


terrazas_select = terrazas_select[terrazas_select$url != "https://maps.google.com/?cid=8604819272613660000",]


#-------------------------------------------------------------------------------
key_1 = "-"
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

url_parte_2 = ",es&appid=e6ccca7bc87e0fd7db0c9f8fa5e5c-"
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
actual_parte_2 = ',es&appid=e6ccca7bc87e0fd7db0c9f8fa5e5c-'


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







          