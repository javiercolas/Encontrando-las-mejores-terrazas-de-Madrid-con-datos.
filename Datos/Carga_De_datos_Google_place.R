library(googleway)
library(tidyverse)

key = "-"

terrazas = read.csv("OPEN DATA Terrazas202101.csv", header = T, sep = ";", encoding = "UTF-8")


prueba = google_places(search_string = "SR CALLE SOFIA NUM 177", key = key)



# LO PRIMERO QUE HACEMOS ES JUNTAR EL NOMBRE DEL BAR,  LA CALLE Y NUM EDIFICIO . PARA QUE LA BUSQUEDA TENGA MAS
# ACCURACY.

terrazas = terrazas %>% 
  mutate(buscador = paste( rotulo , clase_vial_edificio, desc_vial_edificio
                        ))

terrazas = terrazas[validUTF8(terrazas$buscador),]
#--------------------------------------------------------------------------------

lista_id = c()
lista_tipologia = c()
for (busqueda in terrazas$buscador){
  
  identificador = list(google_places(search_string = busqueda, key = key)[["results"]][["place_id"]])
  
  lista_id = c(lista_id, identificador)
  
}

save(terrazas, file="terrazas.Rda")

# si en la lista de id hay mas de 2, no lo cogemos.


terrazas$lista_id = lista_id


save(terrazas, file="terrazas.Rda")




terrazas$out_grupo_id =  sapply(terrazas$lista_id, length)

terrazas = terrazas[terrazas$out_grupo_id < 2, ]

save(terrazas, file="terrazas.Rda")


google_places('bar doctor madrid', key = key)

# de google_places nos interesa el id.

# creamos listas, para luego meterlas en el dataframe , creando nuevas columnas. IMPORTANTE PONER LIST y luego modificamos






lista_direccion = c()
lista_telefono = c()
lista_price_level = c()
lista_resena_media = c()
lista_total_rating = c()
lista_foto = c()
lista_url = c()
lista_ultimas_resenas = c()
lista_longitud = c()
lista_latitud = c()

for (id in terrazas$lista_id){
  raiz_details = google_place_details(id, key = key)
  
  direccion = list(raiz_details[["result"]][["formatted_address"]])
  telefono = list(raiz_details[["result"]][["formatted_phone_number"]])
  price_level = list(raiz_details[["result"]][["price_level"]])
  resena_media = list(raiz_details[["result"]][["rating"]])
  total_rating = list(raiz_details[["result"]][["user_ratings_total"]])
  foto = list(raiz_details[["result"]][["photos"]][["html_attributions"]][[1]])
  url = list(raiz_details[["result"]][["url"]])
  ultimas_resenas = list(raiz_details[["result"]][["reviews"]][["text"]])
  latitud = list(raiz_details[["result"]][["geometry"]][["location"]][["lat"]])
  longitud = list(raiz_details[["result"]][["geometry"]][["location"]][["lng"]])
  
  
  lista_direccion = c(lista_direccion, direccion)
  lista_telefono = c(lista_telefono, telefono)
  lista_price_level = c(lista_price_level, price_level)
  lista_resena_media = c(lista_resena_media, resena_media)
  lista_total_rating = c(lista_total_rating, total_rating)
  lista_foto = c(lista_foto, foto)
  lista_url = c(lista_url, url)
  lista_ultimas_resenas = c(lista_ultimas_resenas, ultimas_resenas)
  lista_longitud = c(lista_longitud, longitud)
  lista_latitud = c(lista_latitud, latitud)
  
  
  
}

terrazas$direccion = lista_direccion
terrazas$telefono = lista_telefono
terrazas$price_level = lista_price_level
terrazas$resena_media = lista_resena_media
terrazas$total_rating = lista_total_rating
terrazas$foto = lista_foto
terrazas$url = lista_url
terrazas$ultimas_resenas = lista_ultimas_resenas
terrazas$longitud = lista_longitud
terrazas$latitud = lista_latitud

save(terrazas, file="terrazas.Rda")

# con google_place_Details sacamos: la direccion, el numero de telefono, el nombre, price level, rating, numero
# total de ratigns, url, y las ultimas reseñas.


terrazas_select = terrazas %>% 
  select(id_terraza, id_local, desc_distrito_local, desc_barrio_local, Cod_Postal,
         coordenada_x_local, coordenada_y_local, rotulo, desc_periodo_terraza, 
         Superficie_ES, desc_ubicacion_terraza, hora_ini_LJ_es, hora_fin_LJ_es,
         direccion, telefono, price_level, resena_media, total_rating, foto, 
         url, ultimas_resenas, longitud, latitud, mesas_es, sillas_es)

save(terrazas_select, file = "terrazas_select.Rda")
      



