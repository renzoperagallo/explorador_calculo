source("R/helpers.R")

app_list <- c("shiny", "dplyr", "DT")

length(app_list)

for (i in app_list){

}

library(shiny)
library(dplyr)
if (!library("DT", logical.return = TRUE,quietly = TRUE)){install.packages("DT")}

data <-
  list(
    data_nominal = readRDS("data/tbl_agregacion.rds"),
    data_real = readRDS("data/tbl_agregacion_real.rds"),
    data_desestacionalizada = readRDS("data/tbl_desestacionalizado.rds"),
    data_brechas = readRDS("data/tbl_agregacion_gap.rds")
  )

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Datos IR-ICL año base 2023"),

  # Usando tabsetPanel para organizar contenido en pestañas
  tabsetPanel(
    # Pestaña para la Búsqueda de Datos
    tabPanel("Datos nominales",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "tipo_indicador", "Tipo indicador:",
                   choices = c("", "estimador", "indice")
                 ),
                 selectInput(
                   "desagregacion", "Desagregacion:",
                   choices = c("", "general", "sexo", "grupo", "tamano", "categoria", "categoria-sexo", "categoria-grupo", "categoria-tamaño", "categoria-grupo-sexo")
                 ),
                 selectInput(
                   "tipo_valor", "Tipo valor:",
                   choices = c("", "lvl", "var_01", "var_12", "var_ud", "inc_01", "inc_12", "inc_ud")
                 ),
                 selectInput(
                   "tipo_parametro", "Parametro:",
                   choices = c("", "ir", "icl", "clht", "hent", "hont", "htnt", "roho", "roreht"),
                   multiple = TRUE
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
                 tableOutput("results")
               )
             )
    ),
    tabPanel("Datos reales",
             h3("En construcción")
    ),
    tabPanel("Datos desestacionalziados",
             h3("En construcción")
    ),
    tabPanel("Brechas",
             h3("En construcción")
    )
  )
)

# Define server logic required to draw a histogram
server <-
  function(input, output) {

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
          data$data_nominal |>
            filter(
              mind_name == input$tipo_indicador,
              by_name == input$desagregacion,
              fn_name == input$tipo_valor
            ) |>
            pull(agregacion) |>
            first() |>
            filter(id_parametro %in% input$tipo_parametro) |>
            dplyr::inner_join(fechas_filtradas(), by = c("ano", "mes"))
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

    # Mostrar los resultados filtrados
    output$results <- renderTable({
      final_filtered_data()
    })
  }
# Run the application
shinyApp(ui = ui, server = server)
