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
    magrittr
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
        ids = 1,
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
        ) * 100,
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

