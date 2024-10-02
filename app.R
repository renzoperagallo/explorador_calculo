
# Configuracion inicial ---------------------------------------------------

# TRUE si se quiere usar el fileserver, false si se quiere usar la carpeta data.
file_server_data = TRUE
ano = "2024" # String con el año en caso de usar fileserver.
mes = "08" # String con el mes en caso de usar fileserver.

# Paquetes y funciones ----------------------------------------------------

source("R/helpers.R")

# Lista de paquetes necesarios
app_list <- c("shiny", "dplyr", "DT", "highcharter", "shinyWidgets")

# Instala y carga los paquetes que faltan
lapply(app_list, function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
})

# Rutas -------------------------------------------------------------------

if(file_server_data){
  ruta_data <-
    paste0("//Buvmfswinp01/IR/EMRCL/calculo/build/", ano, "_", mes, "/")
} else{
  ruta_data <- "./data/"
}

ruta_data_nominal <-paste0(ruta_data, "tbl_agregacion.rds")
ruta_data_real <- paste0(ruta_data, "tbl_agregacion_real.rds")
ruta_data_desestacionalizada <- paste0(ruta_data, "tbl_desestacionalizado.rds")
ruta_data_brechas <- paste0(ruta_data, "tbl_agregacion_gap.rds")

# App ---------------------------------------------------------------------

##### Data #####
data <-
  list(
    data_nominal = readRDS(ruta_data_nominal),
    data_real = readRDS(ruta_data_real),
    data_desestacionalizada = readRDS(ruta_data_desestacionalizada) |>
      format_data_desestacionalizada(),
    data_brechas = readRDS(ruta_data_brechas)
  )

# data <-
#   list(
#     data_nominal = readRDS("data/tbl_agregacion.rds"),
#     data_real = readRDS("data/tbl_agregacion_real.rds"),
#     data_desestacionalizada = readRDS("data/tbl_desestacionalizado.rds") |>
#       format_data_desestacionalizada(),
#     data_brechas = readRDS("data/tbl_agregacion_gap.rds")
#   )

##### Shiny UI #####
titulo <- "Explorador de datos IR-ICL año base 2023"

# Define UI for application that draws a histogram
ui <- fluidPage(
  # use a gradient in background
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "linear",
    direction = "bottom"
  ),
  titlePanel(titulo, windowTitle = titulo),
  tabsetPanel(
    tabPanel(
      "Cuadros estadísticos",
      sidebarLayout(
        sidebarPanel(
          helpText("Seleccione los filtros y presione 'buscar' (o presione 'enter'). Se renderizará una tabla siempre y cuando existan datos para la combinatoria solicitada"),
          selectInput(
            "tipo_indicador", "Tipo indicador:",
            choices = c("", "estimador", "indice", "indice real", "indice desestacionalizado", "brecha indice", "brecha estimador"),
            selected = "indice"
          ),
          selectInput(
            "redondear", "Redondear:",
            choices = c("", "Si", "No"),
            selected = "Si"
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
          actionButton("search", "Buscar")
        ),
        mainPanel(
          DT::DTOutput("results"),
        ),
        position = "left"
      )
    ),
    tabPanel(
      "Gráficos",
      sidebarLayout(
        sidebarPanel(
          helpText("Seleccione tipo de gráfico"),
          selectInput("tipo_grafico", "Tipo de gráfico:", choices = c("line", "column"))
        ),
        mainPanel(
          highchartOutput("grafico_resultados")  # Agregar espacio para el gráfico
        ),
        position = "left"
      )
    )
  ),
  tags$script(HTML("
    $(document).on('keypress', function(e) {
      if(e.which == 13) {  // 13 es el código ASCII para 'Enter'
        $('#search').click();  // Simula un clic en el botón con id 'buscar'
      }
    });
  "))  # Script para activar el botón con "Enter"
)

##### Shiny server #####

# Define server logic required to draw a histogram
server <-
  function(input, output, session) {

    # Observa cuando cambia el tipo de indicador
    observeEvent(input$tipo_indicador, {

      filtered_data <-
        switch(
          input$tipo_indicador,
          "estimador" = tidyr::unnest(data$data_nominal |> dplyr::filter(mind_name == "estimador"), cols = agregacion),
          "indice" = tidyr::unnest(data$data_nominal |> dplyr::filter(mind_name == "indice"), cols = agregacion),
          "indice real" = tidyr::unnest(data$data_real, cols = agregacion),
          "indice desestacionalizado" = tidyr::unnest(data$data_desestacionalizada, cols = agregacion),
          "brecha indice" = tidyr::unnest(data$data_brechas |> dplyr::filter(mind_name == "indice"), cols = agregacion),
          "brecha estimador" = tidyr::unnest(data$data_brechas |> dplyr::filter(mind_name == "estimador"), cols = agregacion),
          NULL
        )


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
              redondear_valores(input$tipo_valor, input$tipo_parametro, input$redondear) |>
              dplyr::mutate(
                periodo = paste0(ano, "-", mes),
                valores = !!sym(input$tipo_valor)
              ) |>
              dplyr::inner_join(fechas_filtradas(), by = c("ano", "mes")) |>
              add_label(input$desagregacion)
          } else if (input$tipo_indicador == "indice desestacionalizado"){
            data$data_desestacionalizada |>
              filter(
                by_name == input$desagregacion,
                fn_name == input$tipo_valor
              ) |>
              pull(agregacion) |>
              first() |>
              filter(id_parametro %in% paste0(input$tipo_parametro)) |>
              redondear_valores(input$tipo_valor, input$tipo_parametro, input$redondear) |>
              dplyr::mutate(
                periodo = paste0(ano, "-", mes),
                valores = !!sym(input$tipo_valor)
              ) |>
              dplyr::inner_join(fechas_filtradas(), by = c("ano", "mes")) |>
              add_label(input$desagregacion)
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
              redondear_valores_brechas(input$redondear) |>
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
              redondear_valores(input$tipo_valor, input$tipo_parametro, input$redondear) |>
              dplyr::mutate(
                periodo = paste0(ano, "-", mes),
                valores = !!sym(input$tipo_valor)
              ) |>
              dplyr::inner_join(fechas_filtradas(), by = c("ano", "mes")) |>
              add_label(input$desagregacion)
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
      final_filtered_data()|> dplyr::select(-any_of("periodo")),
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
    ## - Si quiero ver un gráfico de algo desagregadox2, se ve mal.
    # Generar el gráfico con highcharter
    output$grafico_resultados <- renderHighchart({
      hchart(
        final_filtered_data(),
        input$tipo_grafico,
        hcaes(x = periodo, y = valores,  group = label),
      )  |>
        hc_title(text = paste("Evolución de", input$tipo_valor)) |>
        hc_xAxis(categories = final_filtered_data()$mes) |>
        hc_yAxis(title = list(text = input$tipo_valor)) |>
        highcharter::hc_add_theme(highcharter::hc_theme_538())
    })
  }
# Run the application
shinyApp(ui = ui, server = server)
