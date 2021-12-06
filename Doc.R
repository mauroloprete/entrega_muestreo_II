## ----include = FALSE------------------------------------------------------------

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
mutate.(
    edad_tramo = cut(
        edad,
        breaks = c(0,14,20,25,30,40,50,60,Inf),
        right=FALSE
    )
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


## -------------------------------------------------------------------------------
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


## ----echo = FALSE---------------------------------------------------------------
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


## -------------------------------------------------------------------------------
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


## ----echo = FALSE---------------------------------------------------------------
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


## -------------------------------------------------------------------------------
muestra %>%
    summarize.(
        tr = sum(R*w0) / sum(w0)
    ) %>%
    assign(
        "tasaRespuestapob",
        .,
        envir = .GlobalEnv
    )


## ----echo = FALSE---------------------------------------------------------------
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


## -------------------------------------------------------------------------------
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
        w_nr_post = w0 / tr_w_estrato_dpto
    )




## ----echo = FALSE---------------------------------------------------------------
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


## -------------------------------------------------------------------------------
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


## ----echo = FALSE---------------------------------------------------------------
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


## ----echo = FALSE---------------------------------------------------------------
muestra %>%
    summarize.(
        tr_w = mean(R),
        .by = c(
            "edad",
            "sexo"
        )
    ) %>%
    mutate.(
        sexo = case_when.(
            sexo == 1 ~ "Masculino",
            TRUE ~ "Femenino"
        )
    ) %>%
    ggplot(
        aes(
            x = edad,
            y = tr_w 
        )
    ) + 
    geom_point(
        aes(
            color = sexo
        ),
        size = 2
    ) +
    labs(
        x = "Edad",
        y = "Tasa de respuesta ponderada",
        color = "Sexo"
    ) + 
    theme_bw() + 
    theme(
        legend.position = "bottom"
    ) + 
    scale_colour_viridis_d()


## ----warning = FALSE------------------------------------------------------------
boost_tree(
    trees = 300
) %>%
set_engine(
    "xgboost"
) %>%
set_mode(
    "classification"
) %>%
fit(
    as.factor(R) ~ estrato + sexo + edad + dpto, data = muestra
) %>%
assign(
    "modelo_boost",
    .,
    envir = .GlobalEnv
)


## ----echo = FALSE---------------------------------------------------------------
tibble(
    predict(
        modelo_boost,
        muestra, 
        type = "prob"
    ),
    predict(
        modelo_boost,
        muestra
    )
) %>%
rename.(
    pred_boost = .pred_1
) %>%
assign(
    "pred_boost",
    .,
    envir = .GlobalEnv
)

nombres <- c(1,2)

names(nombres) <- c("Predicción","Observados")


conf_mat(
  data = bind_cols(
    select(muestra, R),
    select(pred_boost,.pred_class)
  ),
  truth = R, 
  estimate = .pred_class
) %$%
table %>%
kbl(
    booktabs = TRUE,
    caption = "Matriz de confusión"
) %>%
add_header_above(
    nombres
) %>%
kable_styling(
        latex_options = c(
            "striped",
            "hold_position"
        )
    )


## -------------------------------------------------------------------------------

muestra %<>%
    bind_cols.(
        pred_boost
    ) %>%
    mutate.(
        w_nr_boost = (w0*R)/(pred_boost)
    )


## -------------------------------------------------------------------------------
muestra %>%
    as_survey_design(
        ids = id_hogar,
        weight = w_nr_boost,
        strata = estrato
    ) %T>%
    assign(
        "diseño_boost",
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
          w_nr_boost,
          type = "kish"
        )
    ) %>%
    assign(
      "est_ponderados_nr_boost",
      .,
      envir = .GlobalEnv
    )


## ----echo = FALSE---------------------------------------------------------------
est_ponderados_nr_boost %>%
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
        caption = "Estimaciones poblacionales usando ponderadores por no respuesta utilizando Boosting"
    ) %>%
    kable_styling(
        latex_options = c(
            "striped",
            "hold_position"
        )
    )


## ----echo = FALSE---------------------------------------------------------------
muestra %>%
    filter.(
        R > 0
    ) %>%
    ggplot(
        aes(
            x = w0,
            y = w_nr_boost
        )
    ) + 
    geom_point(
        size = 2
    ) +
    labs(
        x = "Ponderadores originales",
        y = "Ponderadores NR con boosting"
    ) + 
    theme_bw()


## ----echo = FALSE---------------------------------------------------------------
muestra %>%
    ggplot(
        aes(
            x = pred_boost
        )
    ) + 
    geom_histogram(
        bins = nrow(muestra) ^ 0.5,
        fill = "blue",
        alpha = 0.6
    ) + 
    theme_bw()


## -------------------------------------------------------------------------------

muestra %<>%
    mutate.(
        boost_class = cut(
            pred_boost,
            breaks = quantile(
                pred_boost,
                probs = seq(0,1,1/5)
            ),
            include.lowest = TRUE
        )
    ) %>%
    mutate.(
        ajuste_boost_clases = 1/median(pred_boost),
        .by = boost_class
    ) %>%
    mutate.(
        w_nr_boost_clases = R * w0 * ajuste_boost_clases
    )



## -------------------------------------------------------------------------------
muestra %>%
    as_survey_design(
        ids = id_hogar,
        weight = w_nr_boost_clases,
        strata = estrato
    ) %T>%
    assign(
        "diseño_nr_boost_clases",
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
          w_nr_boost_clases,
          type = "kish"
        )
    ) %>%
    assign(
      "est_ponderados_nr_clases",
      .,
      envir = .GlobalEnv
    )


## ----echo = FALSE---------------------------------------------------------------
est_ponderados_nr_boost %>%
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
        caption = "Estimaciones poblacionales usando ponderadores por no respuesta utilizando las propensiones del punto anterior por clases"
    ) %>%
    kable_styling(
        latex_options = c(
            "striped",
            "hold_position"
        )
    )


## -------------------------------------------------------------------------------
read_excel(
    here(
        "data",
        "dpto.xlsx"
    )
) %>%
rename.(
    personas_dpto = personas 
) %>%
full_join.(
    muestra,
    by = "dpto"
) %T>%
assign(
    "muestra",
    .,
    envir = .GlobalEnv
) %>%
pull.(
    "personas_dpto"
) %>%
unique() %>%
assign(
    "total_dpto",
    .,
    envir = .GlobalEnv
)

edad_sexo <- read_excel(
    here(
        "data",
        "sexo_edad.xlsx"
    )
)

edad_sexo  %>%
    mutate.(
        total = hombres + mujeres,
        .keep = "unused"
    ) %>%
    mutate.(
        edad_tramo = cut(
            edad,
            breaks = c(0,14,20,25,30,40,50,60,Inf),
            right=FALSE
        )
    ) %>%
    summarize.(
        total = sum(total),
        .by = "edad_tramo"
    ) %>% 
    rename.(
        total_edad = total
    ) %>%
    full_join.(
        muestra,
        by = "edad_tramo"
    )%T>%
    assign(
        "muestra",
        .,
        envir = .GlobalEnv
    ) %>%
    pull.(
        "total_edad"
    ) %>%
    unique() %>%
    assign(
        "total_edad",
        .,
        envir = .GlobalEnv
    )

edad_sexo  %>%
    summarize.(
        hombres = sum(hombres),
        mujeres = sum(mujeres)
    ) %>%
    pivot_longer.(
        names_to = "sexo",
        values_to = "valor"
    ) %>%
    rename.(
        total_sexo = valor
    ) %>%
    mutate.(
        sexo = ifelse.(
            sexo == "hombres",
            1,
            2
        )
    ) %>%
    full_join.(
        muestra,
        by = "sexo"
    ) %T>%
    assign(
        "muestra",
        .,
        envir = .GlobalEnv
    ) %>%
    pull.(
        "total_sexo"
    ) %>%
    unique() %>%
    assign(
        "total_sexo",
         .,
        envir = .GlobalEnv
    )



conteos <- c(
    sum(muestra$w0),
    total_dpto[-1],
    total_edad[-1],
    total_sexo[-1]
)


survey::calibrate(
    design = diseño_nr_boost_clases,
    formula = ~ dpto + edad_tramo + sexo,
    population = conteos,
    calfun="raking"
) -> r1


