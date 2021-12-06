


muestra %>%
    as_survey_design(
        ids = id_hogar,
        weight = w0
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
            pet,
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
    kbl(
        booktabs = T,
        caption = "Estimaciones poblacionales usando ponderadores origniales"
    ) %>%
    kable_styling(
        latex_options = c(
            "striped",
            "hold_position"
        )
    )


muestra %>% 
  summarize.(
    n(),
    .by = c("w0","estrato")
  )




muestra %>%
    summarize.(
        tr_w = mean(R),
        .by = c(
            "edad",
            "sexo"
        )
    ) %>%
    ggplot(
        aes(
            x = edad,
            y = tr_w 
        )
    ) + 
    geom_point() + 
    facet_grid(
        vars(sexo)
    )


muestra %>% 
  summarize.(
    sum(w_nr_boost_clases_calibrados),
    .by = "sexo"
  )

muestra %>% 
  summarize.(
    sum(w_nr_boost_clases_calibrados),
    .by = "dpto"
  ) %>% 
  arrange.(
    dpto
  )

muestra %>% 
  summarize.(
    sum(w_nr_boost_clases_calibrados),
    .by = "edad_tramo"
  ) %>% 
  arrange.(
    edad_tramo
  )