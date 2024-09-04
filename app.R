
# Paquetes y funciones ----------------------------------------------------

source("R/helpers.R")

# Lista de paquetes necesarios
app_list <- c("shiny", "dplyr", "DT", "highcharter")

# Instala y carga los paquetes que faltan
lapply(app_list, function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
})


# App ---------------------------------------------------------------------

##### Data #####

data <-
  list(
    data_nominal = readRDS("data/tbl_agregacion.rds"),
    data_real = readRDS("data/tbl_agregacion_real.rds"),
    data_desestacionalizada = readRDS("data/tbl_desestacionalizado.rds") |>
      format_data_desestacionalizada(),
    data_brechas = readRDS("data/tbl_agregacion_gap.rds")
  )

##### Shiny UI #####
titulo <- "Explorador de datos IR-ICL año base 2023"

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(titulo, windowTitle = titulo),
  sidebarLayout(
    sidebarPanel(
      helpText("Seleccione los filtros y presione 'buscar'. Se renderizará una tabla siempre y cuando existan datos para la combinatoria requerida."),
      selectInput(
        "tipo_indicador", "Tipo indicador:",
        choices = c("", "estimador", "indice", "indice real", "indice desestacionalizado", "brecha indice", "brecha estimador"),
        selected = "indice"
      ),
      selectInput(
        "desagregacion", "Desagregacion:",
        choices = NULL
      ),
      selectInput(
        "tipo_valor", "Tipo valor:",
        choices = NULL
      ),
      selectInput(
        "tipo_parametro", "Parametro:",
        choices = NULL
      ),
      numericInput("ano_from", "Desde (año):", 2023, min = 2010, max = 2050, step = 1),
      numericInput("mes_from", "Desde (mes):", 1, min = 1, max = 12, step = 1),
      numericInput("ano_to", "Hasta (año):", 2050, min = 2010, max = 2050, step = 1),
      numericInput("mes_to", "Hasta (mes):", 12, min = 1, max = 12, step = 1),
      helpText("Filtro adiccional: (evite usar espacios)"),
      textInput("columna_filtro", "Ingrese nombre columna de filtro:", ""),
      textInput("valor_filtro", "Ingrese valor de filtro:", ""),
      helpText("Evite usar espacios y recuerde borrar los filtros cuando las columnas no coinciden con el nuevo cuadro."),
      selectInput("tipo_grafico", "Tipo de gráfico:", choices = c("line", "column")),
      actionButton("search", "Buscar")
    ),
    mainPanel(
      DT::DTOutput("results"),
      highchartOutput("grafico_resultados")  # Agregar espacio para el gráfico
    ),
    position = "left"
  )
)

##### Shiny server #####

# Define server logic required to draw a histogram
server <-
  function(input, output, session) {

    # Observa cuando cambia el tipo de indicador
    observeEvent(input$tipo_indicador, {

      filtered_data <- switch(input$tipo_indicador,
                              "estimador" = tidyr::unnest(data$data_nominal |> dplyr::filter(mind_name == "estimador"), cols = agregacion),
                              "indice" = tidyr::unnest(data$data_nominal |> dplyr::filter(mind_name == "indice"), cols = agregacion),
                              "indice real" = tidyr::unnest(data$data_real, cols = agregacion),
                              "indice desestacionalizado" = tidyr::unnest(data$data_desestacionalizada, cols = agregacion),
                              "brecha indice" = tidyr::unnest(data$data_brechas |> dplyr::filter(mind_name == "indice"), cols = agregacion),
                              "brecha estimador" = tidyr::unnest(data$data_brechas |> dplyr::filter(mind_name == "estimador"), cols = agregacion),
                              NULL)


      # Extrae las opciones únicas para el selector de desagregación basado en la columna by_name
      new_choices_desagregacion <- unique(filtered_data$by_name)

      # Extrae las opciones únicas para el selector de tipo_valor basado en la columna fn_name
      new_choices_tipo_valor <- unique(filtered_data$fn_name)

      # Extrae las opciones únicas para el selector de tipo_parametro basado en la columna id_parametro
      new_choices_tipo_parametro <- unique(filtered_data$id_parametro)

      # Actualiza los selectores con las nuevas opciones
      updateSelectInput(session, "desagregacion", choices = new_choices_desagregacion, selected = new_choices_desagregacion[1])
      updateSelectInput(session, "tipo_valor", choices = new_choices_tipo_valor, selected = new_choices_tipo_valor[1])
      updateSelectInput(session, "tipo_parametro", choices = new_choices_tipo_parametro, selected = new_choices_tipo_parametro[1])
    })

    fechas_filtradas <-
      reactive(
        {
          extraer_rango_fechas(
            input$ano_from,
            input$mes_from,
            input$ano_to,
            input$mes_to
          )
        }
      )

    # Datos iniciales filtrados por los primeros criterios y fechas
    initial_filtered_data <-
      reactive(
        {
          req(input$search)
          if (input$tipo_indicador == "indice real"){
            data$data_real |>
              filter(
                by_name == input$desagregacion,
                fn_name == input$tipo_valor
              ) |>
              pull(agregacion) |>
              first() |>
              filter(id_parametro %in% paste0(input$tipo_parametro)) |>
              dplyr::mutate(
                !!input$tipo_valor :=
                  dplyr::case_when(
                    input$tipo_valor %in% c("var_01", "var_12", "var_ud") ~ round(!!sym(input$tipo_valor), 2),
                    input$tipo_valor %in% c("inc_01", "inc_12", "inc_ud") ~ round(!!sym(input$tipo_valor), 3),
                    TRUE ~ round(!!sym(input$tipo_valor), 2)
                  ),
                periodo = paste0(ano, "-", mes)
              ) |>
              dplyr::inner_join(fechas_filtradas(), by = c("ano", "mes"))
          } else if (input$tipo_indicador == "indice desestacionalizado"){
            data$data_desestacionalizada |>
              filter(
                by_name == input$desagregacion,
                fn_name == input$tipo_valor
              ) |>
              pull(agregacion) |>
              first() |>
              filter(id_parametro %in% paste0(input$tipo_parametro)) |>
              dplyr::inner_join(fechas_filtradas(), by = c("ano", "mes"))
          } else if (input$tipo_indicador %in% c("brecha indice", "brecha estimador")){
            data$data_brechas |>
              filter(
                mind_name == stringr::str_sub(
                  input$tipo_indicador,
                  start = 8L
                ),
                by_name == input$desagregacion,
                fn_name == "gap"
              ) |>
              pull(agregacion) |>
              first() |>
              filter(id_parametro %in% input$tipo_parametro) |>
              dplyr::mutate(
                  dplyr::across(
                    dplyr::contains("brecha"),
                    ~ round(. * 100, 2)
                  )
                  ) |>
              dplyr::inner_join(fechas_filtradas(), by = c("ano", "mes"))
          } else {
            data$data_nominal |>
              filter(
                mind_name == input$tipo_indicador,
                by_name == input$desagregacion,
                fn_name == input$tipo_valor
              ) |>
              pull(agregacion) |>
              first() |>
              filter(id_parametro %in% input$tipo_parametro) |>
              dplyr::mutate(
                !!input$tipo_valor :=
                  dplyr::case_when(
                    input$tipo_valor == "lvl" & input$tipo_parametro %in% c("roho", "rehe", "clht", "roreht")  ~ round(!!sym(input$tipo_valor), 0),
                    input$tipo_valor == "lvl" & input$tipo_parametro %in% c("hont", "hent", "htnt")  ~ round(!!sym(input$tipo_valor), 1),
                    input$tipo_valor %in% c("var_01", "var_12", "var_ud") ~ round(!!sym(input$tipo_valor), 2),
                    input$tipo_valor %in% c("inc_01", "inc_12", "inc_ud") ~ round(!!sym(input$tipo_valor), 3),
                    TRUE ~ round(!!sym(input$tipo_valor), 2)
                  ),
                periodo = paste0(ano, "-", mes)
              ) |>
              dplyr::inner_join(fechas_filtradas(), by = c("ano", "mes")) |>
              dplyr::relocate(periodo, .before = ano) |>
              dplyr::select(-ano, -mes) |>
              dplyr::mutate(
                general = input$desagregacion,
                valores = !!sym(input$tipo_valor),
                dplyr::across(
                  c(-valores),
                  ~ as.character(.)
                ),
                !!input$desagregacion :=
                  case_when(
                    !stringr::str_detect(input$desagregacion, "-") ~ as.character(!!sym(input$desagregacion)),
                    TRUE ~ "general"
                  ),
                agrupado =
                  dplyr::case_when(
                    !input$desagregacion %in% c("general") ~ !!sym(input$desagregacion),
                    TRUE  ~ general
                  )
              )
          }

        }
      )

    final_filtered_data  <-
      eventReactive(
        input$search,
        {
          if (input$columna_filtro != "" & input$valor_filtro != "") {
            initial_filtered_data() |>
              dplyr::filter(
                !!sym(input$columna_filtro) == input$valor_filtro
              )
          } else {
            initial_filtered_data()
          }
        }
      )

    output$results <- DT::renderDT(
      final_filtered_data(),
      options = list(
        lengthChange = TRUE,
        pageLength = 100,
        processing = FALSE,
        initComplete = I('function(setting, json) { alert("done"); }'),
        dom = 'Bfrtip',  # Añade un panel de botones al principio de la tabla
        buttons = list(
          'copy', 'csv', 'excel', 'pdf', 'print',  # Botones de exportación
          list(extend = 'colvis', text = 'Mostrar/Ocultar Columnas')  # Botón de visibilidad de columnas
        )
      ),
      extensions = 'Buttons',  # Añade la extensión para los botones
      width = "auto",
      rownames = FALSE
    )

    ## Problemas:
    ## - Si quiero ver un gráfico de algo desagregado, se ve mal.
    ## - Periodo se ve con número del 1 al ....
    # Generar el gráfico con highcharter
    output$grafico_resultados <- renderHighchart({

      hchart(
        final_filtered_data(),
        input$tipo_grafico,
        hcaes(x = periodo, y = valores,  group = agrupado),
      )  |>
        hc_title(text = paste("Evolución de", input$tipo_valor)) |>
        hc_xAxis(categories = final_filtered_data()$mes) |>
        hc_yAxis(title = list(text = input$tipo_valor)) |>
        highcharter::hc_add_theme(highcharter::hc_theme_bloom())

    })


  }
# Run the application
shinyApp(ui = ui, server = server)
