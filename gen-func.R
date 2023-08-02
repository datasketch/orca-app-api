
caption_info <- function(id_data, r, new_line = 100) {

  tx <- NULL
  
  if (id_data == "Inspecciones") {
    if (!is.null(r$deptosId)) {
      if (length(r$deptosId) == 33) {
        deptos <- "Todos"
      } else {
        deptos <- paste0(stringr::str_to_title(r$deptosId), collapse = ", ")
        deptos <- gsub("\\\n", "<br/>", stringr::str_wrap(deptos, new_line))
      }
      tx <- paste0(tx, "<br/> <b>Departamentos:</b> ", deptos)
    }
    if (!is.null(r$anioId)) {
      if (length(r$anioId) >= 4) {
        df_m <- "Todos"
      } else {
        df_m <- paste0(r$anioId, collapse = ", ")
        df_m <- gsub("\\\n", "<br/>", stringr::str_wrap(df_m, new_line))
      }
      tx <-  paste0(tx, "<br/> <b>Años:</b> ", df_m)
    }
    if (!is.null(r$establecimientoId)) {
      if (length(r$establecimientoId) >= 35) {
        df_m <- "Todos"
      } else {
        df_m <- paste0(r$establecimientoId, collapse = ", ")
        df_m <- gsub("\\\n", "<br/>", stringr::str_wrap(df_m, new_line))
      }
      tx <-  paste0(tx, "<br/> <b>Establecimiento:</b> ", df_m)
    }
  } else {
    if (!is.null(r$apre_deptosId)) {
      if (length(r$apre_deptosId) == 33) {
        deptos <- "Todos"
      } else {
        deptos <- paste0(stringr::str_to_title(r$apre_deptosId), collapse = ", ")
        deptos <- gsub("\\\n", "<br/>", stringr::str_wrap(deptos, new_line))
      }
      tx <- paste0(tx, "<br/> <b>Departamentos:</b> ", deptos)
    }
    if (!is.null(r$apre_anioId)) {
      if (length(r$apre_anioId) >= 11) {
        df_m <- "Todos"
      } else {
        df_m <- paste0(r$apre_anioId, collapse = ", ")
        df_m <- gsub("\\\n", "<br/>", stringr::str_wrap(df_m, new_line))
      }
      tx <-  paste0(tx, "<br/> <b>Años:</b> ", df_m)
    }
    if (!is.null(r$apre_cierreId)) {
        impt <- paste0(stringr::str_to_title(r$apre_cierreId), collapse = ", ")
      tx <- paste0(tx, "<br/><b>Cierre de establecimiento: </b>", impt)
    }
    if (!is.null(r$apre_marcaId)) {
      if (length(r$apre_marcaId) >= 35) {
        df_m <- "Todos"
      } else {
        df_m <- paste0(r$apre_marcaId, collapse = ", ")
        df_m <- gsub("\\\n", "<br/>", stringr::str_wrap(df_m, new_line))
      }
      tx <-  paste0(tx, "<br/> <b>Marca:</b> ", df_m)
    }
    if (!is.null(r$apre_claseId)) {
      if (length(r$apre_claseId) >= 7) {
        df_m <- "Todos"
      } else {
        df_m <- paste0(r$apre_claseId, collapse = ", ")
        df_m <- gsub("\\\n", "<br/>", stringr::str_wrap(df_m, new_line))
      }
      tx <-  paste0(tx, "<br/> <b>Clase de producto:</b> ", df_m)
    }

  }
  
  tx
}

