webshot::install_phantomjs(force = TRUE)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinybusy)
library(parmesan)
library(vctrs)
library(dsmodules)
library(hgchmagic) #767c3867535994f1a1fd8c24594d40db3128843d
library(leaflet.extras)
library(ltgeo)
# dsvizopts bff1582f4b6e17600bf92937adf100270c42b91d
# homodatum 6993e3f907579fc72cbbf605d1dd1184330f451b
Sys.setenv(OPENSSL_CONF="/dev/null")
#source("call-data.R")
source("gen-func.R")
ui <-  fluidPage(
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="custom.css"),
    includeScript("www/handlers.js")
  ),
  busy_start_up(
    loader = tags$img(
      src = "img/loading_gris.gif",
      width = 100
    ),
    mode = "auto",#"manual",
    color = "#435b69",
    background = "#FFF"
  ),
  div(class = "layout-container",
      div(class = "layout-panels",
          div(class = "app-container",
              div(class = "panel top-malibu",
                  div (class = "panel-body",
                       uiOutput("menu_buttons"),
                       uiOutput("controls")
                  ),
                  div(class="footer",
                      tags$a(
                        img(src= 'img/logos-app-orca.png', align = "left", width = 240)))
              ),
              div(class = "panel",
                  div (class = "panel-body",
                       div(style="flex-grow: 1; min-width: 600px;",
                           div(class = "head-viz",
                               div(style = "display:flex;gap:20px;margin-bottom: 20px;align-items: flex-end;",
                                   "VISUALIZACIÓN",
                                   uiOutput("viz_icons")
                               ),
                               uiOutput("descargas")
                           )),
                       div(class = "viz-nucleo",
                           #highchartOutput("hgch_viz")
                           uiOutput("viz_view")
                           
                       )
                  )
              ),
              div(class = "panel",
                  div (class = "panel-body",
                       div(style="flex-grow: 1; min-width: 320px;",
                           div(style = "display:block;",
                               div(class = "viz-center",
                                   div(
                                     uiOutput("down_map"),
                                     leafletOutput("map_extra", height = 300),
                                     uiOutput("info_map")
                                   )
                               )
                           ),
                           div(style = "display:block;",
                               div(class = "viz-center",
                                   div(
                                     uiOutput("down_tree"),
                                     highchartOutput("treemap_extra", height = 280),
                                     uiOutput("info_tree")
                                   )
                               )
                           ),
                           uiOutput("viz_extra"),
                           uiOutput("click_aprhtree")#,
                          #verbatimTextOutput("test")
                       )
                  )
              )
          )
      )
  )
)


server <- function(input, output, session) {
  
  
  output$menu_buttons <- renderUI({
    dsapptools:::make_buttons(c("Inspecciones", "Aprehensiones"),
                              labels = c("Visitas de control", "Aprehensiones"), 
                              default_active = "Inspecciones")
  })
  
  id_data <- reactive({
    id <- input$last_click
    if (is.null(id)) id <- "Inspecciones"
    id
  })
  
  
  
  data <- reactiveValues()
  
  observe({
    if (is.null(id_data())) return()
    if (!is.null(data[[id_data()]])) {
      return()
    } else {
      data[[id_data()]] <- readr::read_csv(paste0("https://raw.githubusercontent.com/datasketch/orca-app-api/main/data/", tolower(id_data()), ".csv"))
    }
    
  })
  
  pickerOpts <- reactive({
    list(
      `actions-box` = TRUE,
      `deselect-all-text` = "Ninguno",
      `select-all-text` = "Todos",
      title = "Todos"
    )
  })
  
  insp_data <- reactive({
    if (is.null(id_data())) return()
    res <- FALSE
    if (id_data() == "Inspecciones") res <- TRUE
    res
  })
  
  deptos_inspecciones <- reactive({
    if (is.null(id_data())) return()
    if (id_data() != "Inspecciones") return()
    req(data[[id_data()]])
    data <- data[[id_data()]]
    sort(unique(data$depto))
  })
  
  anios_inspecciones <- reactive({
    if (is.null(id_data())) return()
    if (id_data() != "Inspecciones") return()
    req(data[[id_data()]])
    data <- data[[id_data()]]
    sort(unique(data$anio))
  })
  
  tipo_inspecciones <- reactive({
    if (is.null(id_data())) return()
    if (id_data() != "Inspecciones") return()
    req(data[[id_data()]])
    data <- data[[id_data()]]
    sort(unique(data$tipo_establecimiento))
  })
  
  deptos_aprehension <- reactive({
    if (is.null(id_data())) return()
    if (id_data() != "Aprehensiones") return()
    req(data[[id_data()]])
    data <- data[[id_data()]]
    sort(unique(data$depto))
  })
  
  anios_aprehension <- reactive({
    if (is.null(id_data())) return()
    if (id_data() != "Aprehensiones") return()
    req(data[[id_data()]])
    data <- data[[id_data()]]
    sort(unique(data$anio))
  }) 
  
  cierre_aprehension <- reactive({
    if (is.null(id_data())) return()
    if (id_data() != "Aprehensiones") return()
    req(data[[id_data()]])
    data <- data[[id_data()]]
    setdiff(sort(unique(data$cierre_establecimiento)), NA)
  })  
  
  pickerOptsAdd <- reactive({
    list(
      `live-search`=TRUE,
      `actions-box` = TRUE,
      `deselect-all-text` = "Ninguno",
      `select-all-text` = "Todos",
      title = "Todos"
      
    )
  })
  
  marca_aprehension <- reactive({
    if (is.null(id_data())) return()
    if (id_data() != "Aprehensiones") return()
    req(data[[id_data()]])
    data <- data[[id_data()]]
    sort(unique(data$marca))
  })
  
  clase_aprehension <- reactive({
    if (is.null(id_data())) return()
    if (id_data() != "Aprehensiones") return()
    req(data[[id_data()]])
    data <- data[[id_data()]]
    l <- lapply(unique(data$cat_producto), function(i) {
      df <- data |> filter(cat_producto %in% i)
      unique(df$clase_producto)
    })
    names(l) <- unique(data$cat_producto)
    l
  })
  
  var_num_opts <- reactive({
    if (is.null(id_data())) return()
    if (id_data() != "Aprehensiones") return()
    c("Cantidad de actas de aprehensión" = "cantidad",
      "Cantidad de productos aprehendidos" = "cantidad_productos",
      "Avalúo comercial" = "valor_comercial")
  })
  
  parmesan <- parmesan_load()
  parmesan_input <- parmesan_watch(input, parmesan)
  
  output_parmesan("controls",
                  input = input, output = output, session = session,
                  env = environment())
  
  list_inputs <- reactive({
    if (is.null(id_data())) return()
    if (id_data() == "Inspecciones") {
      anio_sel <- input$anioId
      if (!is.null(anio_sel)) {
        if (length(anio_sel) > 1) {
          anio_sel <- c(min(as.numeric(anio_sel)), max(as.numeric(anio_sel)))
        }
      }
      list(
        "depto" = input$deptosId,
        "anio" = anio_sel,
        "tipo_establecimiento" = input$establecimientoId
      )
    } else {
      anio_sel <- input$apre_anioId
      if (!is.null(anio_sel)) {
        if (length(anio_sel) > 1) {
          anio_sel <- c(min(as.numeric(anio_sel)), max(as.numeric(anio_sel)))
        }
      }
      list(
        "depto" = input$apre_deptosId,
        "anio" = anio_sel,
        "cierre_establecimiento" = input$apre_cierreId,
        "marca" = input$apre_marcaId,
        "clase_producto" = input$apre_claseId
      )
    }
  })
  
  dic_load <- reactive({
    if (is.null(id_data())) return()
    req(data[[id_data()]])
    data <- data[[id_data()]]
    dic <- homodatum::create_dic(data)
    dic$hdType[dic$hdType == "Yea"] <- "Num"
    dic$hdtype <- dic$hdType
    dic
  })
  
  data_filter <- reactive({
    req(list_inputs())
    data <- data[[id_data()]]
    ls <- list_inputs()
    if ("anio" %in% names(data)) {
      data$anio <- as.character(data$anio)
    }
    df <- dsdataprep::data_filter(data = dplyr::as_tibble(data),
                                  dic = dplyr::as_tibble(dic_load()),
                                  var_inputs = ls,
                                  special_placeholder = NULL)
   
    df
  })
  
  
  
  actual_but <- reactiveValues(active = NULL)
  
  observe({
    if (is.null(input$viz_selection)) return()
    viz_rec <- c("line", "bar","map","table")
    if (input$viz_selection %in% viz_rec) {
      actual_but$active <- input$viz_selection
    } else {
      actual_but$active <- viz_rec[1]
    }
    
  })
  
  output$viz_icons <- renderUI({
    possible_viz <- c("line", "bar","map","table")
    shinyinvoer::buttonImageInput('viz_selection',
                                  " ",
                                  images = possible_viz,
                                  path = "viz_icons/",
                                  active = actual_but$active,
                                  imageStyle = list(shadow = TRUE,
                                                    borderColor = "#ffffff",
                                                    padding = "3px"))
    
  })
  
  data_viz <- reactive({
    if (is.null(id_data())) return()
    req(data_filter())
    if (nrow(data_filter()) == 0) return()
    if (is.null(actual_but$active)) return()
    if (actual_but$active %in% "table") return()
    df <- data_filter()
    if (id_data() == "Inspecciones") {
      var <- "depto"
      var_label <- "Departamento"
      if (actual_but$active %in% c("line", "bar")) {
        var <- "anio"
        var_label <- "Año"
        if (!is.null(input$anioId)) {
          if (length(input$anioId) == 1) {
            var <- "fecha_am"
            var_label <- "Fecha"
          }
        }
        if (!is.null(input$deptosId)) {
          var <- c("depto", var)
        }
      }
      df <- dsdataprep::aggregation_data(df,
                                         agg = "count",
                                         agg_name = "conteo",
                                         group_var = var, 
                                         percentage = TRUE, 
                                         percentage_name = "porcentaje"
      )
      df$porcentaje <- round(df$porcentaje, 2)
      #if (actual_but$active != "map") {
      if (ncol(df) == 3) {
        df$..labels <- paste0(var_label, ": ", df[[var]], 
                              "<br/>Total visitas: ", sitools::f2si(df$conteo),
                              "<br/>Porcentaje: ", df$porcentaje)
      } else {
        df$..labels <- paste0("Departamento", ": ", df$depto,
                              "<br/> Año: ", df[[2]],
                              "<br/>Total visitas: ", sitools::f2si(df$conteo),
                              "<br/>Porcentaje: ", df$porcentaje, "%")
      }
      df <- df[, c(var, "conteo", "..labels")]
      #}
      # if (actual_but$active == "map") {
      #   df$..tooltip <- HTML(df$..labels)
      # }
    } else {
      var <- "depto"
      if (actual_but$active %in% c("line", "bar")) {
        var <- c("clase_producto", "anio")
        if (!is.null(input$apre_anioId)) {
          if (length(input$apre_anioId) == 1) var <- c("clase_producto", "fecha_ym")
        }
        # if (!is.null(input$deptosId)) var <- c("depto", var)
      }
      
      agg <- "count"
      var_num <- NULL
      if (!is.null(input$apre_numId)) {
        if (input$apre_numId != "cantidad") {
          agg <- "sum"
          var_num <- input$apre_numId 
        }
      }
      
      df <- dsdataprep::aggregation_data(df,
                                         agg = agg,
                                         #agg_name = "conteo",
                                         group_var = var,
                                         to_agg = var_num,
                                         percentage = TRUE,
                                         percentage_name = "porcentaje"
      )
      df$porcentaje <- round(df$porcentaje, 2)
      if (actual_but$active != "map") {
        df$..labels <- paste0("Clase producto: ", df[[1]], 
                              "<br/>Año: ", df[[2]], "<br/>",
                              "Total", ": ",  sitools::f2si(df[[3]]), 
                              "<br/>Porcentaje: ", df[[4]], "%")
      } else {
        df$..labels <- paste0("Departamento: ", df[[1]], 
                              "<br/>Total", ": ",  sitools::f2si(df[[2]]), 
                              "<br/>Porcentaje: ", df[[3]], "%")
      }
    }
    # if ("code_depto" %in% names(df)) {
    #   df$code_depto <- sprintf("%02d", df$code_depto)
    # }
    if ("depto" %in% names(df)) {
      df$depto[df$depto == "BOGOTA D.C."] <- "BOGOTA"
    }
    if ("anio" %in% names(df)) {
      df$anio <- as.character(df$anio)
    }
    
    df
  })
  
  
  viz_type <- reactive({
    if (is.null(id_data())) return()
    if (is.null(actual_but$active)) return()
    req(data_viz())
    
    if (nrow(data_viz()) == 0) return()
    type <- "DatNum"
    if (ncol(data_viz()) > 3) type <- "CatDatNum"
    if (actual_but$active != "line") {
      type <- gsub("Dat", "Cat", type)
    }
    if (actual_but$active == "map")  type <- NULL
    type    
  })
  
  viz_func <- reactive({
    if (is.null(id_data())) return()
    if (is.null(actual_but$active)) return()
    #req(viz_type())
    viz <- paste0("hgchmagic::hgch_", actual_but$active, "_", viz_type())
    if (actual_but$active == "map") viz <- "ltgeo::lt_choropleth_GnmNum"
    print(viz)
    viz
  })
  
  title_viz <- reactive({
    if (is.null(id_data())) return()
    if (is.null(actual_but$active)) return()
    title <- NULL
    if (id_data() == "Inspecciones") {
      title <- "Visitas de control realizadas en Colombia"
      if (actual_but$active == "map") {
        title <- "Visitas de control realizadas en Colombia por departamento"
      }
    } else {
      req(input$apre_numId)
      var_num <- input$apre_numId
      title <- "Cantidad de actas de aprehensión realizadas en Colombia"
      if (var_num == "cantidad_productos") title <- "Cantidad de productos aprehendidos en Colombia"
      if (var_num == "valor_comercial") title <- "Avalúo comercial de actas de aprehensión realizadas en Colombia"
      if (actual_but$active == "map") {
        title <- paste0(title, " por departamento")
      }
    }
    title
  })
  
  viz_opts <- reactive({
    if (is.null(id_data())) return()
    if (is.null(actual_but$active)) return()
    req(data_viz())
    if (nrow(data_viz()) == 0) return()
    if (actual_but$active != "map") {
      opts <- list(
        data = data_viz(),
        bar_graph_type = "stacked",
        label_wrap = 100,
        label_wrap_legend = 100,
        collapse_rows = TRUE
        #
      )
    } else {
      opts <- list(
        data = data_viz(),
        map_name = "col_large",
        collapse_rows = TRUE,
        map_tiles = "CartoDB",
        map_zoom_snap = 0.25,
        map_zoom_delta = 0.25,
        palette_colors = c("#d7d1ff", "#4b3c69"),
        #percentage_intra = T,
        map_min_zoom = 5.25,
        map_max_zoom = 12
        # border_color = "#3a3a3a",
        # border_width = 0.3,
        # na_color = "#ffffff",
        # na_label = "NA",
        # map_zoom_snap = 0.25,
        # map_zoom_delta = 0.25,
        # zoom_min = 5.5,
        # caption = add_info(),
        # color_palette_sequential = c("#d7d1ff", "#4b3c69")
      )
      
      # if (id_data() == "Inspecciones") {
      #   var <- "conteo"
      # } else {
      #   req(input$apre_numId)
      #   var <- "count"
      #   if (input$apre_numId != "cantidad") var <- input$apre_numId
      # }
      # opts$var <- var
      
    }
    
    
    opts$title_size <- 15
    opts$text_family <- "Fira Sans"
    opts$title_family <- "Fira Sans"
    opts$title <- title_viz()
    opts
  })
  
  
  viz_down <- reactive({
    print("xxxxxxxx")
    print(viz_func())
    req(data_viz())
    req(viz_func())
    #suppressWarnings(
    do.call(eval(parse(text = viz_func())), viz_opts())
    # )
  })
  
  add_info <- reactive({
    if (is.null(id_data())) return()
    paste0(caption_info(id_data = id_data(),input, new_line = 100),
           "<br/>Los números con las siguientes letras significan:<br/>k: Mil<br/>M: Millones <br/> G: Miles de millones <br/> T: Billones")
  })
  
  
  output$hgch_viz <- highcharter::renderHighchart({
    req(actual_but$active)
    req(data_viz())
    if (actual_but$active %in% c("table", "map")) return()
    h <- viz_down() |> hc_legend(   verticalAlign = "top" )
    if(!is.null(add_info())) {
      h <- h |> hc_caption(text = add_info())
    }
  })
  
  output$lflt_viz <- leaflet::renderLeaflet({
    req(actual_but$active)
    req(data_viz())
    if (!actual_but$active %in% c("map")) return()
    viz_down() |>
      leaflet::setView(lng = -74.29, lat = 3.57, 4)
  })
  
  
  
  output$dt_viz <- DT::renderDataTable({
    req(actual_but$active)
    if (actual_but$active != "table") return()
    req(data_filter())
    df <- data_filter()
    dtable <- DT::datatable(df,
                            rownames = F,
                            selection = 'none',
                            options = list(
                              language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                              scrollX = T,
                              fixedColumns = TRUE,
                              fixedHeader = TRUE,
                              scrollY = "500px",
                              autoWidth = TRUE
                            ))
    dtable
  })
  
  output$viz_view <- renderUI({
    req(actual_but$active)
    if (is.null(input$dimension)) return()
    height_viz <- input$dimension[2] - 130
    tx <- "No hay información para los filtros seleccionados"
    if (actual_but$active != "table") {
      if (is.null(data_viz())) return(tx)
    }
    
    viz <- actual_but$active
    if (viz == "map") {
      shinycustomloader::withLoader(
        leaflet::leafletOutput("lflt_viz", height = height_viz),
        type = "html", loader = "loader4"
      )
    } else if (viz == "table") {
      shinycustomloader::withLoader(
        DT::dataTableOutput("dt_viz", width = input$dimension[1]-500),
        type = "html", loader = "loader4"
      )
    } else {
      shinycustomloader::withLoader(
        highcharter::highchartOutput("hgch_viz", height = height_viz),
        type = "html", loader = "loader4"
      )
    }
  })
  
  
  
  output$descargas <- renderUI({
    if (is.null(actual_but$active)) return()
    if (actual_but$active != "table") {
      dsmodules::downloadImageUI("download_viz", dropdownLabel ="Descargar", formats = c("jpeg", "pdf", "png", "html"), display = "dropdown", text = "Descargar")
    } else {
      dsmodules::downloadTableUI("dropdown_table", dropdownLabel = "Descargar", formats = c("csv", "xlsx", "json"), display = "dropdown", text = "Descargar")
    }
  })
  
  observe({
    dsmodules::downloadTableServer("dropdown_table", element = reactive(data_filter()), formats = c("csv", "xlsx", "json"))
    dsmodules::downloadImageServer("download_viz", element = reactive(viz_down()), lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"), file_prefix = "plot")
  })
  
  
  data_extra_deptos <- reactive({
    if (is.null(id_data())) return()
    req(data_filter())
    if (nrow(data_filter()) == 0) return()
    if (is.null(actual_but$active)) return()
    if (actual_but$active %in% "table") return()
    df <- data_filter()
    if (id_data() == "Inspecciones") {
      df1 <- dsdataprep::aggregation_data(df,
                                          agg = "count",
                                          agg_name = "conteo",
                                          group_var = "depto", 
                                          percentage = TRUE, 
                                          percentage_name = "porcentaje"
      )
      df1$..labels <- "Da click para más detalle"
      df1$depto[df1$depto == "BOGOTA D.C."] <- "BOGOTA"
      df2 <- dsdataprep::aggregation_data(df,
                                          agg = "count",
                                          agg_name = "conteo",
                                          group_var = "tipo_establecimiento", 
                                          percentage = TRUE, 
                                          percentage_name = "porcentaje"
      )
      df2$..labels <- "Da click para más detalle"
      df <- list(
        "deptos" = df1,
        "tipo" = df2
      )
    } else {
      agg <- "count"
      var_num <- NULL
      if (!is.null(input$apre_numId)) {
        if (input$apre_numId != "cantidad") {
          agg <- "sum"
          var_num <- input$apre_numId 
        }
      }
      df1 <- dsdataprep::aggregation_data(df,
                                          agg = agg,
                                          #agg_name = "conteo",
                                          group_var = "depto",
                                          to_agg = var_num,
                                          percentage = TRUE,
                                          percentage_name = "porcentaje"
      )
      df1$..labels <- "Da click para más detalle"
      df1$depto[df1$depto == "BOGOTA D.C."] <- "BOGOTA"
      df2 <- dsdataprep::aggregation_data(df,
                                          agg = agg,
                                          #agg_name = "conteo",
                                          group_var = "cierre_establecimiento",
                                          to_agg = var_num,
                                          percentage = TRUE,
                                          percentage_name = "porcentaje"
      )
      df2$..labels <- "Da click para más detalle"
      df3 <- dsdataprep::aggregation_data(df,
                                          agg = agg,
                                          #agg_name = "conteo",
                                          group_var = "clase_producto",
                                          to_agg = var_num,
                                          percentage = TRUE,
                                          percentage_name = "porcentaje"
      )
      df3$..labels <- "Da click para más detalle"
      df <- list(
        "deptos" = df1,
        "cierre" = df2,
        "clase" = df3
      )
    }
    
    df
  })
  
  info_click_map <- reactive({
    if (is.null(id_data())) return()
    if (is.null(input$map_extra_shape_click)) return()
    req(data_extra_deptos())
    df <- data_extra_deptos()$deptos
    if (nrow(df) == 0) return()
    df <- df |> filter(depto %in% stringi::stri_trans_general(input$map_extra_shape_click$id, "Latin-ASCII"))
    if (id_data() == "Inspecciones") {
      tx <- paste0("Departamento: ", df$depto, 
                   "<br/>Total visitas: ", sitools::f2si(df$conteo),
                   "<br/>Porcentaje: ", round(df$porcentaje, digits = 2))
    } else {
      tx <- paste0("Departamento: ", df$depto, 
                   "<br/>", input$apre_numId ,": ", sitools::f2si(df[[2]]),
                   "<br/>Porcentaje: ", round(df$porcentaje, digits = 2))
    }
    tx
  })
  
  output$info_map <- renderUI({
    req(info_click_map())
    HTML(info_click_map())
  })
  
  map_down <- reactive({
    if (is.null(id_data())) return()
    req(data_extra_deptos())
    df <- data_extra_deptos()$deptos
    var_num <- "conteo"
    tx <- NULL
    if (id_data() == "Inspecciones") {
      tx <- "Visitas de control realizadas en Colombia"
    } else {
      tx <- "Cantidad de actas de aprehensión realizadas en Colombia"
      var_num <- "count"
      if (!is.null(input$apre_numId)) {
        if (input$apre_numId != "cantidad") {
          var_num <- input$apre_numId 
        }
      }
    }
    
    ltgeo::lt_choropleth_GnmNum(data = df, 
                                #var = var_num,
                                title = tx,
                                title_size = 10,
                                text_family = "Fira Sans",
                                title_family = "Fira Sans",
                                map_name = "col_large",
                                legend_show = FALSE,
                                palette_colors = c("#d7d1ff", "#4b3c69"),
                                collapse_rows = TRUE,
                                map_tiles = "CartoDB"
                                #                        zoom_show = FALSE,
                                #                        border_color = "#3a3a3a",
                                #                        border_width = 0.3,
                                #                        na_color = "#ffffff",
                                #                        na_label = "NA",
                                #                        map_zoom_snap = 0.25,
                                #                        map_zoom_delta = 0.25,
                                #                        zoom_min = 3.5,
                                #                        legend_show = FALSE,
                                #                        color_palette_sequential = c("#d7d1ff", "#4b3c69")
    )
  })
  
  output$map_extra <- renderLeaflet({
    req(map_down())
    map_down()
  })
  
  output$down_map <- renderUI({
    dsmodules::downloadImageUI("download_map",
                               dropdownLabel = img(src= 'img/descarga-icon-w.svg', class = "img-down"),
                               formats = c("jpeg", "pdf", "png", "html"),
                               display = "dropdown", text = "  ")
  })
  
  observe({
    dsmodules::downloadImageServer("download_map", element = reactive(map_down()), lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"), file_prefix = "plot")
  })
  
  info_click_tree <- reactive({
    if (is.null(id_data())) return()
    req(data_extra_deptos())
    
    if (id_data() == "Inspecciones") {
      if (is.null(input$id_ins)) return()
      df <- data_extra_deptos()$tipo
      if (nrow(df) == 0) return()
      df <- df |> filter(tipo_establecimiento %in% input$id_ins$id)
      tx <- paste0("Tipo de visita: ", df[[1]], 
                   "<br/>Total visitas: ", sitools::f2si(df$conteo),
                   "<br/>Porcentaje: ", round(df$porcentaje, digits = 2))
    } else {
      if (is.null(input$id_apr)) return()
      df <- data_extra_deptos()$clase
      if (nrow(df) == 0) return()
      df <- df |> filter(clase_producto %in% gsub("<br/>", " ", input$id_apr$id))
      tx <- paste0("Clase de aprehensión: ", df[[1]], 
                   "<br/>", input$apre_numId ,": ", sitools::f2si(df[[2]]),
                   "<br/>Porcentaje: ", round(df$porcentaje, digits = 2))
    }
    tx
  })
  
  output$info_tree <- renderUI({
    req(info_click_tree())
    HTML(info_click_tree())
  })
  
  tree_down <- reactive({
    if (is.null(id_data())) return()
    req(data_extra_deptos())
    df <- NULL
    if (id_data() == "Inspecciones") {
      id_click <- "id_ins"
      df <- data_extra_deptos()$tipo
      df <- df |> arrange(-conteo) #|> filter(tipo_establecimiento != "(NA)")
      #df$tipo_establecimiento[6:(nrow(df))] <- "Otros"
      tx <- "Visitas de control realizadas en Colombia por tipo de establecimiento"
    } else {
      id_click <- "id_apr"
      df <- data_extra_deptos()$clase
      tx <- "Actas de aprehensión realizadas en Colombia por clase del producto"
    }
    
    hgch_treemap_CatNum(data = df, 
                        title = tx, 
                        title_size = 10, 
                        collapse_rows = TRUE,
                        data_labels_align = 'middle',
                        data_labels_inside = TRUE,
                        data_labels_show = TRUE,
                        text_family = "Fira Sans",
                        title_family = "Fira Sans",
                        shiny_cursor = "pointer",
                        shiny_clickable = TRUE,
                        shiny_input_name = id_click)
  })
  
  output$treemap_extra <- renderHighchart({
    req(tree_down())
    tree_down()
  })
  
  output$down_tree <- renderUI({
    dsmodules::downloadImageUI("download_tree",
                               dropdownLabel = img(src= 'img/descarga-icon-w.svg', class = "img-down"),
                               formats = c("jpeg", "pdf", "png", "html"),
                               display = "dropdown", text = "  ")
  })
  
  observe({
    dsmodules::downloadImageServer("download_tree", element = reactive(tree_down()), lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"), file_prefix = "plot")
  })
  
  
  info_click_aprhtree <- reactive({
    if (is.null(id_data())) return()
    req(data_extra_deptos())
    
    if (id_data() == "Inspecciones") return()
    
    if (is.null(input$hcClicked)) return()
    df <- data_extra_deptos()$cierre
    if (nrow(df) == 0) return()
    df <- df |> filter(cierre_establecimiento %in% gsub("<br/>", " ", input$hcClicked$id))
    tx <- paste0("Cierre de establecimiento: ", df[[1]], 
                 "<br/>", input$apre_numId ,": ", sitools::f2si(df[[2]]),
                 "<br/>Porcentaje: ", round(df$porcentaje, digits = 2))
    
    tx
  })
  
  output$click_aprhtree <- renderUI({
    req(info_click_aprhtree())
    HTML(info_click_aprhtree())
  })
  
  apreh_tree_down <- reactive({
    if (is.null(id_data())) return()
    req(data_extra_deptos())
    df <- NULL
    if (id_data() == "Inspecciones") return()
    df <- data_extra_deptos()$cierre
    
    
    hgch_treemap_CatNum(data = df,
                        title = "Actas de aprehensión por cierre de establecimiento", 
                        data_labels_show = TRUE,
                        title_size = 10,
                        collapse_rows = TRUE,
                        data_labels_align = 'middle',
                        data_labels_inside = TRUE,
                        text_family = "Fira Sans",
                        title_family = "Fira Sans",
                        shiny_cursor = "pointer",
                        shiny_clickable = TRUE)
  })
  
  output$treemap_extra_apreh <- renderHighchart({
    req(apreh_tree_down())
    apreh_tree_down()
  })
  
  output$viz_extra <- renderUI({
    if (is.null(id_data())) return()
    req(data_extra_deptos())
    df <- NULL
    if (id_data() == "Inspecciones") return()
    div(style = "display:block;",
        div(class = "viz-center",
            div(
              uiOutput("down_tree_apreh"),
              highchartOutput("treemap_extra_apreh", height = 130)
            )
        )
    )
  })
  
  output$down_tree_apreh <- renderUI({
    dsmodules::downloadImageUI("download_tree_apreh",
                               dropdownLabel = img(src= 'img/descarga-icon-w.svg', class = "img-down"),
                               formats = c("jpeg", "pdf", "png", "html"),
                               display = "dropdown", text = "  ")
  })
  
  observe({
    dsmodules::downloadImageServer("download_tree_apreh", element = reactive(apreh_tree_down()), lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"), file_prefix = "plot")
  })
  
  output$test <- renderPrint({
    input$map_extra_shape_click
  })
  
}



shinyApp(ui, server)

