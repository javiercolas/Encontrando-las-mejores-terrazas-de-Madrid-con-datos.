library(rvest)
library(httr)
library(jsonlite)
library(stringr)
library(tidyverse)



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



