fechas <-
  tibble::tibble(
    ano = seq(2000, 2100)
  ) |>
  tidyr::expand_grid(
    mes = seq(1,12)
  ) |>
  dplyr::arrange(ano, mes) |>
  dplyr::mutate(
    id = seq(1, dplyr::n())
  )

extraer_rango_fechas <-
  function(ano_from, mes_from, ano_to, mes_to){

    id_from <-
      fechas |>
      dplyr::filter(ano == as.integer(ano_from), mes == as.integer(mes_from)) |>
      dplyr::pull(id)

    id_to <-
      fechas |>
      dplyr::filter(ano == as.integer(ano_to), mes == as.integer(mes_to)) |>
      dplyr::pull(id)

    id_range <-
      seq(id_from, id_to)

    out <-
      fechas |>
      dplyr::filter(id %in% id_range) |>
      dplyr::select(-id)

    return(out)
  }

format_data_desestacionalizada <-
  function(data_desestacionalizada){
    out <-
      data_desestacionalizada |>
      dplyr::rowwise() |>
      dplyr::mutate(
        agregacion = list(
          agregacion |>
            dplyr::mutate(id_parametro = id_parametro) |>
            dplyr::relocate("id_parametro", dplyr::everything())
        )
      ) |>
      dplyr::ungroup() |>
      tidyr::pivot_wider(
        names_from = "id_parametro",
        values_from = "agregacion"
      ) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        agregacion =
          list(
            dplyr::bind_rows(
              ir_emp_seas,
              icl_emp_seas
            )
          )
      ) |>
      dplyr::select(
        -ir_emp_seas,
        -icl_emp_seas
      )

    return(out)
  }

