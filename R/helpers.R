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

order_df <-
  function(df){

    desagregaciones <- c("sexo", "categoria", "tamano", "grupo")

    out <-
      df |>
      dplyr::relocate(
        id_parametro,
        ano,
        mes,
        periodo,
        dplyr::any_of(desagregaciones),
        label,
        .before = valores
        )

    out <-
      out |>
      dplyr::select(
        -dplyr::any_of(ends_with("_texto")),
        -dplyr::any_of(c("lvl", "var_12", "var_01", "var_ud", "inc_01", "inc_12", "inc_ud"))
      )

    return(out)
  }

arrange_df <-
  function(df){

    col_names <- c("id_parametro", "ano", "mes", "tamano", "categoria", "sexo", "grupo")

    out <-
      df |>
      dplyr::arrange(
        dplyr::across(
          dplyr::any_of(col_names)
        )
      )

    return(out)
  }

redondear_valores <-
  function(df, tipo_valor, tipo_parametro, redondear){

    if(redondear == "Si"){
      out <-
        df |>
        dplyr::mutate(
          !!tipo_valor :=
            dplyr::case_when(
              tipo_valor == "lvl" & tipo_parametro %in% c("roho", "rehe", "clht", "roreht")  ~ round(!!sym(tipo_valor), 0),
              tipo_valor == "lvl" & tipo_parametro %in% c("hont", "hent", "htnt")  ~ round(!!sym(tipo_valor), 1),
              tipo_valor %in% c("var_01", "var_12", "var_ud") ~ round(!!sym(tipo_valor), 2),
              tipo_valor %in% c("inc_01", "inc_12", "inc_ud") ~ round(!!sym(tipo_valor), 3),
              TRUE ~ round(!!sym(tipo_valor), 2)
            )
        )
    } else {
      out <- df
    }
      return(out)
  }

redondear_valores_brechas <-
  function(df, redondear){

    if(redondear == "Si"){
      out <-
        df |>
        dplyr::mutate(
          dplyr::across(
            !c("id_parametro", "ano", "mes"),
            ~ round(., 2)
          )
        )
    } else {
      out <- df
    }

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
           label = !!dplyr::sym(paste0(des, "_texto"))
        ) |>
        dplyr::mutate(
          label = stringr::str_trim(label),
          id_parametro = stringr::str_to_upper(id_parametro)
        ) |>
        order_df() |>
        arrange_df()

    } else if (des == "general") {
      data |>
        dplyr::mutate(
          id_parametro = stringr::str_to_upper(id_parametro),
          label = "general"
        )  |>
        order_df() |>
        arrange_df()

    } else if(stringr::str_detect(des, "-")) {

    data |>
      dplyr::left_join(
        cross_join(
          openxlsx::read.xlsx("data-raw/label.xlsx", sheet = stringr::str_split_1(des, "-")[1]),
          openxlsx::read.xlsx("data-raw/label.xlsx", sheet = stringr::str_split_1(des, "-")[2])
        )
      ) |>
      dplyr::mutate(
        label =
          paste0(
            !!dplyr::sym(paste0(stringr::str_split_1(des, "-")[1], "_texto")),
            "-",
            !!dplyr::sym(paste0(stringr::str_split_1(des, "-")[2], "_texto"))
          )
      ) |>
      dplyr::mutate(
        label = stringr::str_trim(label),
        id_parametro = stringr::str_to_upper(id_parametro)
      ) |>
        order_df() |>
        arrange_df()

    } else {
      data |>
        dplyr::mutate(
          label = "general"
        ) |>
        order_df() |>
        arrange_df()
    }
  }
