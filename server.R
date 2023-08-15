source("global.R") # loads initial data

server <- function(input, output) {

  output$lexi_plots <- renderPlot({
    lexi_type <- input$lexi

    if (lexi_type == "Bing"){
      sum_df %>% filter(Title == input$books) %>%
        inner_join(bing, by = "word") %>%
        count(word, sentiment, sort = TRUE) %>%
        group_by(sentiment) %>%
        slice_head(n=input$top_n) %>% 
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        theme_light() +
        labs(title = "Top Negative/Positive Words",
             subtitle = "BING lexicon",
             x = "Number of words",
             y = NULL)
    }
    
    else if (lexi_type == "NRC") {
      sum_df %>% filter(Title == input$books) %>%
        inner_join(nrc, by = "word") %>%
        count(word, sentiment, sort = TRUE) %>%
        filter(!word %in% c("words", "feeling")) %>% #remove tokens
        group_by(sentiment) %>%
        slice_head(n=input$top_n) %>% 
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        theme_light() +
        labs(title = "Top Emotional Words",
             subtitle = "NRC lexicon",
             x = NULL, y = NULL,
             caption = "")
    }
    else {
      sum_df %>% filter(Title == input$books) %>%
        inner_join(afinn, by = "word") %>%
        group_by(chapter) %>%
        summarise(sentiment = sum(value)) %>%
        ungroup() %>%
        mutate(positive = sentiment > 0,
               chapter = as.factor(chapter)) %>%
        ggplot(aes(chapter, sentiment)) +
        geom_col(aes(fill = positive), show.legend = FALSE) +
        theme_minimal() +
        theme(panel.grid.major.x = element_blank()) +
        labs(title = "Overall Sentiment analysis",
             subtitle = "Score by chapter, afinn lexicon",
             caption = "")
    }
  })

  # Correlation plot across texts  -----------------------------------------------
  
  output$corr_plot <- renderPlot({
    
    c_type <- input$to_compare1

    if (c_type == "A Clergyman's Daughter") {
      frequency_plot1 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`Animal Farm`,
                     names_to = "Title", values_to = "Proportion")
  
      ggplot(frequency_plot1, aes(x = Proportion, y = `A Clergyman's Daughter`, 
                                  color = abs(`A Clergyman's Daughter` - Proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), 
                             low = "darkslategray4", high = "gray75") +
        facet_wrap(~Title, ncol = 4) +
        theme(legend.position="none") +
        labs(y = "A Clergyman's Daughter", x = NULL)
    }
    else if (c_type == "Animal Farm") {
      frequency_plot2 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`A Clergyman's Daughter`,
                     names_to = "Title", values_to = "Proportion")
      
      ggplot(frequency_plot2, aes(x = Proportion, y = `Animal Farm`, 
                                  color = abs(`Animal Farm` - Proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), 
                             low = "darkslategray4", high = "gray75") +
        facet_wrap(~Title, ncol = 1) +
        theme(legend.position="none") +
        labs(y = "Animal Farm", x = NULL)
    }
    else if (c_type == "Nineteen Eighty-Four") {
      frequency_plot3 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`A Clergyman's Daughter`,
                     names_to = "Title", values_to = "Proportion")
      
      ggplot(frequency_plot3, aes(x = Proportion, y = `Nineteen Eighty-Four`, 
                                  color = abs(`Nineteen Eighty-Four` - Proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), 
                             low = "darkslategray4", high = "gray75") +
        facet_wrap(~Title, ncol = 1) +
        theme(legend.position="none") +
        labs(y = "Nineteen Eighty-Four", x = NULL)
    }
    else if (c_type == "Coming up for Air") {
      frequency_plot4 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`A Clergyman's Daughter`,
                     names_to = "Title", values_to = "Proportion")
      
      ggplot(frequency_plot4, aes(x = Proportion, y = `Coming up for Air`, 
                                  color = abs(`Coming up for Air` - Proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), 
                             low = "darkslategray4", high = "gray75") +
        facet_wrap(~Title, ncol = 1) +
        theme(legend.position="none") +
        labs(y = "Coming up for Air", x = NULL)
    }
    else {
      frequency_plot5 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`A Clergyman's Daughter`,
                     names_to = "Title", values_to = "Proportion")
      
      ggplot(frequency_plot5, aes(x = Proportion, y = `Homage to Catalonia`, 
                                  color = abs(`Homage to Catalonia` - Proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), 
                             low = "darkslategray4", high = "gray75") +
        facet_wrap(~Title, ncol = 1) +
        theme(legend.position="none") +
        labs(y = "Homage to Catalonia", x = NULL)
    }
    
  })
  
  output$corr_plot2 <- renderPlot({
    if (input$to_compare1 == "Animal Farm") {
    frequency_plot2.1 <- raw_frequency_plot %>% 
      pivot_wider(names_from = Title, values_from = Proportion) %>%
      pivot_longer(`Nineteen Eighty-Four`,
                   names_to = "Title", values_to = "Proportion")
    
    ggplot(frequency_plot2.1, aes(x = Proportion, y = `Animal Farm`, 
                                  color = abs(`Animal Farm` - Proportion))) +
      geom_abline(color = "gray40", lty = 2) +
      geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
      geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
      scale_x_log10(labels = percent_format()) +
      scale_y_log10(labels = percent_format()) +
      scale_color_gradient(limits = c(0, 0.001), 
                           low = "darkslategray4", high = "gray75") +
      facet_wrap(~Title, ncol = 1) +
      theme(legend.position="none") +
      labs(y = "Animal Farm", x = NULL)
    }
    
    else if (input$to_compare1 == "Coming up for Air") {
      frequency_plot2.2 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`Nineteen Eighty-Four`,
                     names_to = "Title", values_to = "Proportion")
      
      ggplot(frequency_plot2.2, aes(x = Proportion, y = `Coming up for Air`, 
                                    color = abs(`Coming up for Air` - Proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), 
                             low = "darkslategray4", high = "gray75") +
        facet_wrap(~Title, ncol = 1) +
        theme(legend.position="none") +
        labs(y = "Coming up for Air", x = NULL)
    }
    
    else if (input$to_compare1 == "Homage to Catalonia") {
      frequency_plot2.3 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`Nineteen Eighty-Four`,
                     names_to = "Title", values_to = "Proportion")
      
      ggplot(frequency_plot2.3, aes(x = Proportion, y = `Homage to Catalonia`, 
                                    color = abs(`Homage to Catalonia` - Proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), 
                             low = "darkslategray4", high = "gray75") +
        facet_wrap(~Title, ncol = 1) +
        theme(legend.position="none") +
        labs(y = "Homage to Catalonia", x = NULL)
    }
    
    else if (input$to_compare1 == "Nineteen Eighty-Four") {
      frequency_plot2.4 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`Animal Farm`,
                     names_to = "Title", values_to = "Proportion")
      
      ggplot(frequency_plot2.4, aes(x = Proportion, y = `Nineteen Eighty-Four`, 
                                    color = abs(`Nineteen Eighty-Four` - Proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), 
                             low = "darkslategray4", high = "gray75") +
        facet_wrap(~Title, ncol = 1) +
        theme(legend.position="none") +
        labs(y = "Nineteen Eighty-Four", x = NULL)
    }
    else {
      frequency_plot2.5 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`Nineteen Eighty-Four`,
                     names_to = "Title", values_to = "Proportion")
      
      ggplot(frequency_plot2.5, aes(x = Proportion, y = `A Clergyman's Daughter`, 
                                    color = abs(`A Clergyman's Daughter` - Proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), 
                             low = "darkslategray4", high = "gray75") +
        facet_wrap(~Title, ncol = 1) +
        theme(legend.position="none") +
        labs(y = "A Clergyman's Daughter", x = NULL)
    }
  })
  
  output$corr_plot3 <- renderPlot({
    if (input$to_compare1 == "Animal Farm") {
      frequency_plot3.1 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`Coming up for Air`,
                     names_to = "Title", values_to = "Proportion")
      
      ggplot(frequency_plot3.1, aes(x = Proportion, y = `Animal Farm`, 
                                    color = abs(`Animal Farm` - Proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), 
                             low = "darkslategray4", high = "gray75") +
        facet_wrap(~Title, ncol = 1) +
        theme(legend.position="none") +
        labs(y = "Animal Farm", x = NULL)
    }
    else if (input$to_compare1 == "Coming up for Air") {
      frequency_plot3.2 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`Animal Farm`,
                     names_to = "Title", values_to = "Proportion")
      
      ggplot(frequency_plot3.2, aes(x = Proportion, y = `Coming up for Air`, 
                                    color = abs(`Coming up for Air` - Proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), 
                             low = "darkslategray4", high = "gray75") +
        facet_wrap(~Title, ncol = 1) +
        theme(legend.position="none") +
        labs(y = "Coming up for Air", x = NULL)
    }
    else if (input$to_compare1 == "Homage to Catalonia") {
      frequency_plot3.3 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`Coming up for Air`,
                     names_to = "Title", values_to = "Proportion")
      
      ggplot(frequency_plot3.3, aes(x = Proportion, y = `Homage to Catalonia`, 
                                    color = abs(`Homage to Catalonia` - Proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), 
                             low = "darkslategray4", high = "gray75") +
        facet_wrap(~Title, ncol = 1) +
        theme(legend.position="none") +
        labs(y = "Homage to Catalonia", x = NULL)
    }
    
    else if (input$to_compare1 == "Nineteen Eighty-Four") {
      frequency_plot3.4 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`Coming up for Air`,
                     names_to = "Title", values_to = "Proportion")
      
      ggplot(frequency_plot3.4, aes(x = Proportion, y = `Nineteen Eighty-Four`, 
                                    color = abs(`Nineteen Eighty-Four` - Proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), 
                             low = "darkslategray4", high = "gray75") +
        facet_wrap(~Title, ncol = 1) +
        theme(legend.position="none") +
        labs(y = "Nineteen Eighty-Four", x = NULL)
    }
    
    else {
      frequency_plot3.5 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`Coming up for Air`,
                     names_to = "Title", values_to = "Proportion")
      
      ggplot(frequency_plot3.5, aes(x = Proportion, y = `A Clergyman's Daughter`, 
                                    color = abs(`A Clergyman's Daughter` - Proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), 
                             low = "darkslategray4", high = "gray75") +
        facet_wrap(~Title, ncol = 1) +
        theme(legend.position="none") +
        labs(y = "A Clergyman's Daughter", x = NULL)
    }
  })
  
  output$corr_plot4 <- renderPlot({
    if (input$to_compare1 == "Animal Farm") {
      frequency_plot4.1 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`Homage to Catalonia`,
                     names_to = "Title", values_to = "Proportion")
      
      ggplot(frequency_plot4.1, aes(x = Proportion, y = `Animal Farm`, 
                                    color = abs(`Animal Farm` - Proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), 
                             low = "darkslategray4", high = "gray75") +
        facet_wrap(~Title, ncol = 1) +
        theme(legend.position="none") +
        labs(y = "Animal Farm", x = NULL)
    }
    
    else if (input$to_compare1 == "Coming up for Air") {
      frequency_plot4.2 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`Homage to Catalonia`,
                     names_to = "Title", values_to = "Proportion")
      
      ggplot(frequency_plot4.2, aes(x = Proportion, y = `Coming up for Air`, 
                                    color = abs(`Coming up for Air` - Proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), 
                             low = "darkslategray4", high = "gray75") +
        facet_wrap(~Title, ncol = 1) +
        theme(legend.position="none") +
        labs(y = "Coming up for Air", x = NULL)
    }
    else if (input$to_compare1 == "Homage to Catalonia") {
      frequency_plot4.3 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`Animal Farm`,
                     names_to = "Title", values_to = "Proportion")
      
      ggplot(frequency_plot4.3, aes(x = Proportion, y = `Homage to Catalonia`, 
                                    color = abs(`Homage to Catalonia` - Proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), 
                             low = "darkslategray4", high = "gray75") +
        facet_wrap(~Title, ncol = 1) +
        theme(legend.position="none") +
        labs(y = "Homage to Catalonia", x = NULL)
    }
    
    else if (input$to_compare1 == "Nineteen Eighty-Four") {
      frequency_plot4.4 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`Homage to Catalonia`,
                     names_to = "Title", values_to = "Proportion")
      
      ggplot(frequency_plot4.4, aes(x = Proportion, y = `Nineteen Eighty-Four`, 
                                    color = abs(`Nineteen Eighty-Four` - Proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), 
                             low = "darkslategray4", high = "gray75") +
        facet_wrap(~Title, ncol = 1) +
        theme(legend.position="none") +
        labs(y = "Nineteen Eighty-Four", x = NULL)
    }
    else {
      frequency_plot4.5 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`Homage to Catalonia`,
                     names_to = "Title", values_to = "Proportion")
      
      ggplot(frequency_plot4.5, aes(x = Proportion, y = `A Clergyman's Daughter`, 
                                    color = abs(`A Clergyman's Daughter` - Proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), 
                             low = "darkslategray4", high = "gray75") +
        facet_wrap(~Title, ncol = 1) +
        theme(legend.position="none") +
        labs(y = "A Clergyman's Daughter", x = NULL)
    }
  })
  
  output$cor_sum <- renderPrint({
    if (input$to_compare1 == "Animal Farm") {
      freq1 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`Animal Farm`,
                     names_to = "Title", values_to = "Proportion")
      
      cor_test_1984 <- cor.test(data = freq1[freq1$Title == "Animal Farm",],
                                ~ Proportion + `Nineteen Eighty-Four`)
      cor_test_catalonia <- cor.test(data = freq1[freq1$Title == "Animal Farm",],
                                     ~ Proportion + `Homage to Catalonia`)
      cor_test_air <- cor.test(data = freq1[freq1$Title == "Animal Farm",],
                               ~ Proportion + `Coming up for Air`)
      cor_test_daughter <- cor.test(data = freq1[freq1$Title == "Animal Farm",],
                                    ~ Proportion + `A Clergyman's Daughter`)
      
      results <- list(
        "Correlation with Nineteen Eighty-Four" = cor_test_1984,
        "Correlation with Homage to Catalonia" = cor_test_catalonia,
        "Correlation with Coming up for Air" = cor_test_air,
        "Correlation with A Clergyman's Daughter" = cor_test_daughter
      )
      
      return(results)
    }
    else if (input$to_compare1 == "Nineteen Eighty-Four") {
      freq1 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`Nineteen Eighty-Four`,
                     names_to = "Title", values_to = "Proportion")
      
      cor_test_animal <- cor.test(data = freq1[freq1$Title == "Nineteen Eighty-Four",],
                                ~ Proportion + `Animal Farm`)
      cor_test_catalonia <- cor.test(data = freq1[freq1$Title == "Nineteen Eighty-Four",],
                                     ~ Proportion + `Homage to Catalonia`)
      cor_test_air <- cor.test(data = freq1[freq1$Title == "Nineteen Eighty-Four",],
                               ~ Proportion + `Coming up for Air`)
      cor_test_daughter <- cor.test(data = freq1[freq1$Title == "Nineteen Eighty-Four",],
                                    ~ Proportion + `A Clergyman's Daughter`)
      
      results <- list(
        "Correlation with Animal Farm" = cor_test_animal,
        "Correlation with Homage to Catalonia" = cor_test_catalonia,
        "Correlation with Coming up for Air" = cor_test_air,
        "Correlation with A Clergyman's Daughter" = cor_test_daughter
      )
      
      return(results)
    }
    else if (input$to_compare1 == "Homage to Catalonia") {
      freq1 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`Homage to Catalonia`,
                     names_to = "Title", values_to = "Proportion")
      
      cor_test_animal <- cor.test(data = freq1[freq1$Title == "Homage to Catalonia",],
                                  ~ Proportion + `Animal Farm`)
      cor_test_1984 <- cor.test(data = freq1[freq1$Title == "Homage to Catalonia",],
                                     ~ Proportion + `Nineteen Eighty-Four`)
      cor_test_air <- cor.test(data = freq1[freq1$Title == "Homage to Catalonia",],
                               ~ Proportion + `Coming up for Air`)
      cor_test_daughter <- cor.test(data = freq1[freq1$Title == "Homage to Catalonia",],
                                    ~ Proportion + `A Clergyman's Daughter`)
      
      results <- list(
        "Correlation with Animal Farm" = cor_test_animal,
        "Correlation with Nineteen Eighty-Four" = cor_test_1984,
        "Correlation with Coming up for Air" = cor_test_air,
        "Correlation with A Clergyman's Daughter" = cor_test_daughter
      )
      
      return(results)
    }
    
    else if (input$to_compare1 == "Coming up for Air") {
      freq1 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`Coming up for Air`,
                     names_to = "Title", values_to = "Proportion")
      
      cor_test_animal <- cor.test(data = freq1[freq1$Title == "Coming up for Air",],
                                  ~ Proportion + `Animal Farm`)
      cor_test_1984 <- cor.test(data = freq1[freq1$Title == "Coming up for Air",],
                                ~ Proportion + `Nineteen Eighty-Four`)
      cor_test_homage <- cor.test(data = freq1[freq1$Title == "Coming up for Air",],
                               ~ Proportion + `Homage to Catalonia`)
      cor_test_daughter <- cor.test(data = freq1[freq1$Title == "Coming up for Air",],
                                    ~ Proportion + `A Clergyman's Daughter`)
      
      results <- list(
        "Correlation with Animal Farm" = cor_test_animal,
        "Correlation with Nineteen Eighty-Four" = cor_test_1984,
        "Correlation with Homage to Catalonia" = cor_test_homage,
        "Correlation with A Clergyman's Daughter" = cor_test_daughter
      )
      
      return(results)
    }
    
    else {
      freq1 <- raw_frequency_plot %>% 
        pivot_wider(names_from = Title, values_from = Proportion) %>%
        pivot_longer(`A Clergyman's Daughter`,
                     names_to = "Title", values_to = "Proportion")
      
      cor_test_animal <- cor.test(data = freq1[freq1$Title == "A Clergyman's Daughter",],
                                  ~ Proportion + `Animal Farm`)
      cor_test_1984 <- cor.test(data = freq1[freq1$Title == "A Clergyman's Daughter",],
                                ~ Proportion + `Nineteen Eighty-Four`)
      cor_test_homage <- cor.test(data = freq1[freq1$Title == "A Clergyman's Daughter",],
                                  ~ Proportion + `Homage to Catalonia`)
      cor_test_air <- cor.test(data = freq1[freq1$Title == "A Clergyman's Daughter",],
                                    ~ Proportion + `Coming up for Air`)
      
      results <- list(
        "Correlation with Animal Farm" = cor_test_animal,
        "Correlation with Nineteen Eighty-Four" = cor_test_1984,
        "Correlation with Homage to Catalonia" = cor_test_homage,
        "Correlation with Coming up for Air" = cor_test_air
      )
      
      return(results)
    }
    
  })
  
  
  # N-Gram Plot -----------------------------------------------
  
  output$ngram_plot <- renderPlot({
    if (input$n_gram == 2) { #bigram
        df_bigrams <- sum_df_raw %>% filter(Title == input$books) %>%
          unnest_tokens(bigram, text, token = 'ngrams', n = 2) %>% filter(!is.na(bigram))
        
        bigrams_separated <- df_bigrams %>%
          separate(bigram, c("word1", "word2"), sep = " ")
        
        bigrams_filtered <- bigrams_separated %>%
          filter(!word1 %in% stop_words$word) %>%
          filter(!word2 %in% stop_words$word)
        
        # new bigram counts:
        bigram_counts <- bigrams_filtered %>% 
          count(word1, word2, sort = TRUE)
        
        bigram_graph <- bigram_counts %>%
          filter(n > 5) %>%
          graph_from_data_frame()
        
        set.seed(2020)
        
        a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
        
        ggraph(bigram_graph, layout = "fr") +
          geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                         arrow = a, end_cap = circle(.07, 'inches')) +
          geom_node_point(color = "lightblue", size = 5) +
          geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
          theme_void()

    }
    else if (input$n_gram == 3) {
      df_trigrams <- sum_df_raw %>% filter(Title == input$books) %>%
        unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
        filter(!is.na(trigram)) %>%
        separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
        filter(!word1 %in% stop_words$word,
               !word2 %in% stop_words$word,
               !word3 %in% stop_words$word) %>%
        count(word1, word2, word3, sort = TRUE)
      
      trigram_graph <- df_trigrams %>%
        filter(n > 1) %>%
        graph_from_data_frame()
      
      set.seed(2021)
      
      a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
      
      ggraph(trigram_graph, layout = "fr") +
        geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                       arrow = a, end_cap = circle(.07, 'inches')) +
        geom_node_point(color = "lightgreen", size = 5) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
        theme_void()
      
    }
  })
}
