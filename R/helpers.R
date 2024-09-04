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



add_label <-
  function(data, des) {

    if(des != "general" & !stringr::str_detect(des, "-")) {

      data |>
        dplyr::left_join(
          openxlsx::read.xlsx("data-raw/label.xlsx", sheet = des)
        ) |>
        dplyr::rename(
           txt = !!dplyr::sym(paste0(des, "_texto"))
        ) |>
        dplyr::select(-!!dplyr::sym(des)) |>
        dplyr::mutate(
          txt = stringr::str_trim(txt),
          id_parametro = stringr::str_to_upper(id_parametro)
        ) |>
        dplyr::select(
          periodo, id_parametro, txt, valores
        )

    } else if (des == "general") {
      data |>
        dplyr::mutate(
          id_parametro = stringr::str_to_upper(id_parametro),
          txt = "general"
        )  |>
        dplyr::select(
          periodo, id_parametro, txt, valores
        )
    } else if(stringr::str_detect(des, "-")) {


    data |>
      dplyr::left_join(
        cross_join(
          openxlsx::read.xlsx("data-raw/label.xlsx", sheet = stringr::str_split_1(des, "-")[1]),
          openxlsx::read.xlsx("data-raw/label.xlsx", sheet = stringr::str_split_1(des, "-")[2])
        )
      ) |>
      dplyr::mutate(
        txt =
          paste0(
            !!dplyr::sym(paste0(stringr::str_split_1(des, "-")[1], "_texto")),
            "-",
            !!dplyr::sym(paste0(stringr::str_split_1(des, "-")[2], "_texto"))
          )
      ) |>
      dplyr::select(-!!dplyr::sym(stringr::str_split_1(des, "-")[1]), -!!dplyr::sym(stringr::str_split_1(des, "-")[2])) |>
      dplyr::mutate(
        txt = stringr::str_trim(txt),
        id_parametro = stringr::str_to_upper(id_parametro)
      ) |>
      dplyr::select(
        periodo, id_parametro, txt, valores
      )

    } else {
      data |>
        dplyr::mutate(
          txt = "general"
        )

    }

  }
