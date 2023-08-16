library(httr)
library(curl)
library(jsonlite)
library(tidyverse)

get_data <- function(names_data) {
  get_token <-
    httr::POST(
      'https://fondocuenta.fnd.org.co/ApiOrcaDS/api/Account',
      add_headers('content-type' = 'application/json'),
      body = '{"Username": "OrcaDs", "Password":"ProdDS-2022Orca"}'
    )
  token <- content(get_token)
  
  end_p = Sys.Date()
  
  
  #url <- paste0('https://fondocuenta.fnd.org.co/ApiOrcaDS/api/', names_data, '?fechaInicial=2000-01-01&fechaFinal=', end_p)
  url <- paste0('https://fondocuenta.fnd.org.co/ApiOrcaDS/api/', names_data, '?fechaInicial=2023-08-09&fechaFinal=', end_p)
  respose <-
    httr::GET(
      url,
      add_headers('Authorization' = paste('Bearer', token))
    ) 
  
  result <- fromJSON(content(respose, as = 'text'))
  
  
  
  data_save <- list(
    "Inspecciones" = read_csv("data/inspecciones.csv"),
    "Aprehensiones" = read_csv("data/aprehensiones.csv")
  )
  
  if (identical(result, list())) {
    data <- data_save[[names_data]]
  } else {
    if (names_data == "Inspecciones") {
      result$fecha_inspeccion <- lubridate::dmy(result$fecha_inspeccion)
      result$anio <- lubridate::year(result$fecha_inspeccion)
      print("$$$$$$$$$")
      dx <- result |> filter(anio == "2023")
      print(nrow(dx))
      result$fecha_am <- format(result$fecha_inspeccion, "%Y-%m")
      max(result$fecha_inspeccion)
      result$mcipio <- trimws(result$mcipio)
      result$depto[result$depto == "SAN ANDRES ISLAS"] <- "ARCHIPIELAGO DE SAN ANDRES, PROVIDENCIA Y SANTA CATALINA"
      #readr::write_csv(result, "data/inspecciones.csv")
    } else {
      result$fecha_acta <- lubridate::dmy(result$fecha_acta)
      result$anio <- lubridate::year(result$fecha_acta)
      result$fecha_ym <- format(result$fecha_acta, "%Y-%m")
      max(result$fecha_acta)
      result$mcipio <- trimws(result$mcipio)
      clases_pr <-  c("LICOR EXTRANJERO", "LICOR NACIONAL", "CIGARRILLO EXTRANJERO", "CERVEZA EXTRANJERA", "CIGARRILLO NACIONAL", "CERVEZA NACIONAL")
      result$cat_producto <- ifelse(result$clase_producto %in% clases_pr, "Clases principales", "Otras clases")
      result$clase_producto[result$clase_producto %in% c("LICOR EXTRANJERO", "LICOR NACIONAL")] <- "LICORES"
      result$clase_producto[result$clase_producto %in% c("CIGARRILLO EXTRANJERO", "CIGARRILLO NACIONAL")] <- "CIGARRILLOS"
      result$clase_producto[result$clase_producto %in% c("CERVEZA EXTRANJERA", "CERVEZA NACIONAL")] <- "CERVEZAS"
      result$cierre_establecimiento <- ifelse(result$cierre_establecimiento, "SÍ", "NO")
      result$depto[result$depto == "SAN ANDRES ISLAS"] <- "ARCHIPIELAGO DE SAN ANDRES, PROVIDENCIA Y SANTA CATALINA"
      #readr::write_csv(result, "data/aprehensiones.csv")
    }
    data <- data_save[[names_data]] |> bind_rows(result)
    
  }
  
  data
}

#names_data <- 
#names_data <- "Aprehensiones"
l <- lapply(c("Inspecciones", "Aprehensiones"), function(nd) {
  df <- get_data(nd)
  readr::write_csv(df, paste0("data/", tolower(nd), ".csv"))
})

# readr::write_csv(result$Inspecciones, "data/inspecciones.csv")
# read_csv("data/inspecciones.csv")
# 
# 
# readr::write_csv(result$Aprehensiones, "data/aprehensiones.csv")
# read_csv("data/aprehensiones.csv")
