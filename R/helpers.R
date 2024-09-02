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
