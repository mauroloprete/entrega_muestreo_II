## ----include = FALSE----------------------------------------------------------

if(!require(pacman)) {
  install.packages("pacman")
}

pacman::p_load(
    here,
    dplyr,
    srvyr,
    readxl,
    dplyr,
    forcats,
    tidytable,
    kableExtra,
    magrittr,
    ggplot2,
    tidymodels,
    xgboost,
    PracTools
)

here(
    "data",
    "muestra grupo 4.xlsx"
) %>%
read_excel(
    .
) %>%
assign(
    "muestra",
    .,
    envir = .GlobalEnv
)

options(kableExtra.latex.load_packages = FALSE)

 knitr::write_bib(
   .packages(),
   here(
     "document",
     "biblio",
     "bib.bib"
   )
 )



# Para poder recuperar el script de los chunk

knitr::purl(
  here::here(
    "document",
    "Doc.Rnw"
  )
)


## -----------------------------------------------------------------------------
muestra %>%
    as_survey_design(
        ids = id_hogar,
        weight = w0,
        strata = estrato
    ) %T>%
    assign(
        "diseño",
        .,
        envir = .GlobalEnv
    ) %>%
    filter(
        R > 0
    ) %>%
    summarize(
        td = survey_ratio(
            desocupado,
            activo,
            deff = TRUE,
            vartype = c("se","cv")
        ),
        pobre = survey_mean(
            pobreza,
            deff = TRUE,
            vartype = c("se","cv")
        ),
        yprom = survey_mean(
            ingreso,
            deff = TRUE,
            vartype = c("se","cv")
        )
    ) %>%
    assign(
      "est_originales",
      .,
      envir = .GlobalEnv
    )


## ----echo = FALSE-------------------------------------------------------------
 est_originales %>%
    pivot_longer.(
        names_to = "v",
        values_to = "value"
    ) %>%
    mutate.(
        tipo = stringr::str_extract(
            v,
            "[a-z]*$"
        ),
        variable = stringr::str_extract(
            v,
            "^[a-z]*"
        ),
        .keep = "unused"
    ) %>%
    mutate.(
        tipo = fct_collapse(
            tipo,
            "estimacion" = c(
                "td",
                "yprom",
                "pobre"
            )
        )
    ) %>%
    mutate.(
        across.(
            where(is.numeric),
            ~ round(
                .x,
                3
            )
        )
    ) %>%
    pivot_wider.(
        names_from = "tipo",
        values_from = "value"
    ) %>%
    relocate.(
      variable,
      estimacion,
      se,
      cv,
      deff
    ) %>%
    set_names(
      c(
        "Variable",
        "Estimación puntual",
        "Error estandar",
        "CV",
        "deff"
      )
    ) %>%
    kbl(
        booktabs = T,
        caption = "Estimaciones poblacionales usando ponderadores originales"
    ) %>%
    kable_styling(
        latex_options = c(
            "striped",
            "hold_position"
        )
    )


## -----------------------------------------------------------------------------
muestra %>%
    summarize.(
        tr = mean(R)
    ) %>%
    mutate.(
        tnr = 1 - tr
    ) %>%
    assign(
        "tasaRespuesta",
        .,
        envir = .GlobalEnv
    )


## ----echo = FALSE-------------------------------------------------------------
tasaRespuesta %>%
    set_names(
        c(
            "Tasa de Respuesta",
            "Tasa de No Respuesta"
        )
    ) %>%
    mutate.(
        across.(
            .fns = ~ round(.x,2)
        )
    ) %>%
    kbl(
        booktabs = T,
        caption = "Tasa de Respuesta"
    ) %>%
    kable_styling(
        latex_options = c(
            "striped",
            "hold_position"
        )
    )


## -----------------------------------------------------------------------------
muestra %>%
    summarize.(
        tr = sum(R*w0) / sum(w0)
    ) %>%
    assign(
        "tasaRespuestapob",
        .,
        envir = .GlobalEnv
    )


## ----echo = FALSE-------------------------------------------------------------
tasaRespuestapob %>%
    mutate.(
        tnr = 1 - tr
    ) %>%
    mutate.(
        across.(
            .fns = ~ round(.x,2)
        )
    ) %>%
    set_names(
        c(
            "Tasa de respuesta poblacional",
            "Tasa de no respuesta poblacional"
        )
    ) %>%
    kbl(
        booktabs = T,
        caption = "Tasa de Respuesta poblacional"
    ) %>%
    kable_styling(
        latex_options = c(
            "striped",
            "hold_position"
        )
    )


## ----echo = FALSE-------------------------------------------------------------
muestra %>%
    summarize.(
        tr_w_estrato_dpto = weighted.mean(
            R
        ),
        .by = c(
            "estrato",
            "dpto"
        )
    ) %>%
    assign(
        "tr_estrato_dpto",
        .,
        envir = .GlobalEnv
    )

muestra %<>%
    left_join.(
        tr_estrato_dpto,
        by = c(
            "estrato" = "estrato",
            "dpto" = "dpto"
        )
    ) %>%
    mutate.(
        w_nr_post = w0/tr_w_estrato_dpto
    )




## ----echo = FALSE-------------------------------------------------------------
tr_estrato_dpto %>%
    mutate.(
        dpto_label = case_when.(
            dpto == 1 ~ "Montevideo",
            dpto == 3 ~ "Canelones",
            dpto == 16 ~ "San José"
        )
    ) %>%
    mutate.(
      estrato_label = case_when.(
          estrato == 1 ~ "Montevideo Bajo",
          estrato == 2 ~ "Montevideo Medio Bajo",
          estrato == 3 ~ "Montevideo medio",
          estrato == 4 ~ "Montevideo medio alto",
          estrato == 5 ~ "Montevideo Alto",
          estrato == 6 ~ "Zona metropolitana",
          estrato == 7 ~ "Interior norte",
          estrato == 8 ~ "Costa Este",
          estrato == 9 ~ "Litoral Norte",
          estrato == 10 ~ "Litoral sur",
          estrato == 11 ~ "Centro Norte",
          estrato == 12 ~ "Centro sur"
      )
    ) %>%
    filter.(
        dpto %in% c(
            1,
            3,
            16
        )
    ) %>%
    ggplot(
        aes(
            x = as.factor(estrato),
            y = tr_w_estrato_dpto,
            color = estrato_label
        )
    ) + 
    geom_segment(
        aes(
            xend = as.factor(estrato),
            y = 0,
            yend = tr_w_estrato_dpto
        ),
        size = 0.9
    ) + 
    geom_point() + 
    scale_y_continuous(
        labels = scales::percent,
        limits = c(0, NA)
    ) + 
    facet_wrap(
        vars(
            dpto_label
        ),
        scales = "free_x"
    ) + 
    labs(
        x = "",
        y = "Tasa de respuesta ponderada",
        color = "Estrato"
    ) + 
    theme_bw() + 
    theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom"
    ) + 
    scale_colour_viridis_d(
        option = "inferno"
    )


## -----------------------------------------------------------------------------
muestra %>%
    as_survey_design(
        ids = id_hogar,
        weight = w_nr_post,
        strata = estrato
    ) %T>%
    assign(
        "diseño",
        .,
        envir = .GlobalEnv
    ) %>%
    filter(
        R > 0
    ) %>%
    summarize(
        td = survey_ratio(
            desocupado,
            activo,
            deff = TRUE,
            vartype = c("se","cv")
        ),
        pobre = survey_mean(
            pobreza,
            deff = TRUE,
            vartype = c("se","cv")
        ),
        yprom = survey_mean(
            ingreso,
            deff = TRUE,
            vartype = c("se","cv")
        ),
        deffK = deff(
          w_nr_post,
          type = "kish"
        )
    ) %>%
    assign(
      "est_ponderados_nr",
      .,
      envir = .GlobalEnv
    )


## ----echo = FALSE-------------------------------------------------------------
est_ponderados_nr %>%
    pivot_longer.(
        - deffK,
        names_to = "v",
        values_to = "value"
    ) %>%
    mutate.(
        tipo = stringr::str_extract(
            v,
            "[a-z]*$"
        ),
        variable = stringr::str_extract(
            v,
            "^[a-z]*"
        ),
        .keep = "unused"
    ) %>%
    mutate.(
        tipo = fct_collapse(
            tipo,
            "estimacion" = c(
                "td",
                "yprom",
                "pobre"
            )
        )
    ) %>%
    mutate.(
        across.(
            where(is.numeric),
            ~ round(
                .x,
                3
            )
        )
    ) %>%
    pivot_wider.(
        names_from = "tipo",
        values_from = "value"
    ) %>%
    relocate.(
      variable,
      estimacion,
      se,
      cv,
      deff,
      deffK
    ) %>%
    set_names(
      c(
        "Variable",
        "Estimación puntual",
        "Error estandar",
        "CV",
        "deff",
        "Efecto diseño de Kish"
      )
    ) %>%
    kbl(
        booktabs = T,
        caption = "Estimaciones poblacionales usando ponderadores por no respuesta"
    ) %>%
    kable_styling(
        latex_options = c(
            "striped",
            "hold_position"
        )
    )

