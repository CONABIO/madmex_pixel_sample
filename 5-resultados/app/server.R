function(input, output, session) {
    re <- reactive({
        leer_tabs_total(input$agregacion)
    })
    output$tab_ests_total <- renderDataTable({
        re <- re()
        filtro <- input$tipo_est_total
        if(filtro == "Ambas"){ filtro <- c("Top", "Top3") }
        re %>% filter(tipo %in% filtro)
    }, rownames = FALSE)
    output$plot_ests_total <- renderPlot({
        if(input$agregacion == "Edo"){
            data_nal <- leer_tabs_total("Nacional")
            ggplot(re(), aes(x = reorder(edo, -est), y = est, ymin = est - ee, 
                ymax = est + ee, color = tipo)) +
                geom_linerange(size = 1.5, alpha = 0.6) +
                geom_hline(data = data_nal, aes(yintercept = est, 
                    color = tipo), alpha = 0.3) +
                facet_wrap(~n_clases, ncol = 1) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                labs(title = "Estimaciones +- error estándar", x = "") 
        } else {
            NULL
            # ggplot(re(), aes(x = factor(n_clases), y = est, ymin = est - ee, 
            #     ymax = est + ee, 
            #     color = tipo)) +
            #     geom_linerange() +
            #     theme(axis.text.x = element_text(angle = 90, hjust = 1))
        }
        
    })
    re_clase <- reactive({
        leer_tabs_clase(input$edo)
    })
    output$int <- renderPlot({
        ests_top_top3 <- bind_rows(!!!re_clase(), .id = "tipo_est") %>% 
            gather(var, value, -tipo_est, -clase) %>% 
            mutate(
                tipo = ifelse(str_detect(var, "usuario"), "usuario", 
                    "productor"),
                tipo_value = case_when(
                    str_detect(var, "_se") ~ "ee", 
                    str_detect(var, "est") ~ "est", 
                    str_detect(var, "n_") ~ "n"
                )
            ) %>% 
            select(-var) %>% 
            spread(tipo_value, value)
            ggplot(ests_top_top3, aes(x = factor(clase), y = ee, ymin = est - ee, 
                ymax = est + ee, color = tipo)) +
                geom_linerange(alpha = 0.5, size = 3) +
                facet_wrap(~ tipo_est, ncol = 1) +
                theme_minimal()
    })
    output$tab_est_clase <- renderDataTable({
        if (input$tipo_est == "Top") {
            re_clase()$top
        } else {
            re_clase()$top3
        }
    })
    output$tab_clases <- renderTable({
        if (input$N_clases == "17") {
            tab_clases_17
        } else {
            tab_clases_31
        }
    })
}