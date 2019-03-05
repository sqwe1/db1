shinyServer(function(input, output, session) {
  datasetInput <- reactive({
    switch(
      input$dataset,
      "ThreeUK" = ThreeUK,
      "VodafoneUK" = VodafoneUK,
      "TescoMobile" = TescoMobile,
      "VirginMobile" = VirginMobile,
      "EE" = EE,
      "O2" = O2
    )
  })
  
  emotionInput <- reactive({
    switch(input$emotion,
           c("Positive", "Negative"))
  })
  
  
  output$No.ofreviews <- renderInfoBox({
    if (input$dataset == "ThreeUK") {
      infoBox(title = "No. of reviews",
              value = nrow(ThreeUK))
    }
    else if (input$dataset == "VodafoneUK") {
      infoBox(title = "No. of reviews",
              value = nrow(VodafoneUK))
    }
    else if (input$dataset == "TescoMobile") {
      infoBox(title = "No. of reviews",
              value = nrow(TescoMobile))
    }
    else if (input$dataset == "VirginMobile") {
      infoBox(title = "No. of reviews",
              value = nrow(VirginMobile))
    }
    else if (input$dataset == "EE") {
      infoBox(title = "No. of reviews",
              value = nrow(EE))
    }
    else {
      infoBox(title = "No. of reviews",
              value = nrow(O2))
    }
    
  })
  
  output$Positive <- renderInfoBox({
    if (input$dataset == "ThreeUK") {
      infoBox(
        title = "",
        value = as.data.frame(table(ThreeUK$category_senti1))[3, ],
        icon("arrow-up"),
        subtitle = "",
        color = "green"
      )
    }
    else if (input$dataset == "VodafoneUK") {
      infoBox(
        title = "",
        value = as.data.frame(table(VodafoneUK$category_senti1))[3, ],
        icon("arrow-up"),
        subtitle = "",
        color = "green"
      )
    }
    else if (input$dataset == "TescoMobile") {
      infoBox(
        title = "",
        value = as.data.frame(table(TescoMobile$category_senti1))[3, ],
        icon("arrow-up"),
        subtitle = "",
        color = "green"
      )
    }
    else if (input$dataset == "VirginMobile") {
      infoBox(
        title = "",
        value = as.data.frame(table(VirginMobile$category_senti1))[3, ],
        icon("arrow-up"),
        subtitle = "",
        color = "green"
      )
    }
    else if (input$dataset == "EE") {
      infoBox(
        title = "",
        value = as.data.frame(table(EE$category_senti1))[3, ],
        icon("arrow-up"),
        subtitle = "",
        color = "green"
      )
    }
    else {
      infoBox(
        title = "",
        value = as.data.frame(table(O2$category_senti1))[3, ],
        icon("arrow-up"),
        subtitle = "",
        color = "green"
      )
    }
    
  })
  output$Neutral <- renderInfoBox({
    if (input$dataset == "ThreeUK") {
      infoBox(
        title = "",
        value = as.data.frame(table(ThreeUK$category_senti1))[2, ],
        icon("arrows-h"),
        subtitle = "",
        color = "yellow"
      )
    }
    else if (input$dataset == "VodafoneUK") {
      infoBox(
        title = "",
        value = as.data.frame(table(VodafoneUK$category_senti1))[2, ],
        icon("arrows-h"),
        subtitle = "",
        color = "yellow"
      )
    }
    else if (input$dataset == "TescoMobile") {
      infoBox(
        title = "",
        value = as.data.frame(table(TescoMobile$category_senti1))[2, ],
        icon("arrows-h"),
        subtitle = "",
        color = "yellow"
      )
    }
    else if (input$dataset == "VirginMobile") {
      infoBox(
        title = "",
        value = as.data.frame(table(VirginMobile$category_senti1))[2, ],
        icon("arrows-h"),
        subtitle = "",
        color = "yellow"
      )
    }
    else if (input$dataset == "EE") {
      infoBox(
        title = "",
        value = as.data.frame(table(EE$category_senti1))[2, ],
        icon("arrows-h"),
        subtitle = "",
        color = "yellow"
      )
    }
    else {
      infoBox(
        title = "",
        value = as.data.frame(table(O2$category_senti1))[2, ],
        icon("arrows-h"),
        subtitle = "",
        color = "yellow"
      )
    }
    
  })
  
  
  output$Negative <- renderInfoBox({
    if (input$dataset == "ThreeUK") {
      infoBox(
        title = "",
        value = as.data.frame(table(ThreeUK$category_senti1))[1, ],
        icon("arrow-down"),
        subtitle = "",
        color = "red"
      )
    }
    else if (input$dataset == "VodafoneUK") {
      infoBox(
        title = "",
        value = as.data.frame(table(VodafoneUK$category_senti1))[1, ],
        icon("arrow-down"),
        subtitle = "",
        color = "red"
      )
    }
    else if (input$dataset == "TescoMobile") {
      infoBox(
        title = "",
        value = as.data.frame(table(TescoMobile$category_senti1))[1, ],
        icon("arrow-down"),
        subtitle = "",
        color = "red"
      )
    }
    else if (input$dataset == "VirginMobile") {
      infoBox(
        title = "",
        value = as.data.frame(table(VirginMobile$category_senti1))[1, ],
        icon("arrow-down"),
        subtitle = "",
        color = "red"
      )
    }
    else if (input$dataset == "EE") {
      infoBox(
        title = "",
        value = as.data.frame(table(EE$category_senti1))[1, ],
        icon("arrow-down"),
        subtitle = "",
        color = "red"
      )
    }
    else {
      infoBox(
        title = "",
        value = as.data.frame(table(O2$category_senti1))[1, ],
        icon("arrow-down"),
        subtitle = "",
        color = "red"
      )
    }
    
  })
  
  
  output$all <-
    renderPlot({
      plot(
        table(ThreeUK$Month.and.Year),
        type = "line",
        col = "Blue",
        xaxt = "n",
        main = "Volume chart for all months",
        xlab = "Months",
        ylab = "Frequency",
        ylim = c(0, 500)
      )
      lines(table(TescoMobile$Month.and.Year),
            type = "line",
            col = "Dark Green")
      lines(table(VirginMobile$Month.and.Year),
            type = "line",
            col = "Maroon")
      lines(table(VodafoneUK$Month.and.Year),
            type = "line",
            col = "Red")
      lines(table(EE$Month.and.Year),
            type = "line",
            col = "Orange")
      lines(table(O2$Month.and.Year),
            type = "line",
            col = "Purple")
      axis(1, at = 1:19, labels = ThreeUK[1:19, 37])
      legend(
        15,
        520,
        legend = c(
          "ThreeUK",
          "TescoMobile",
          "VirginMobile",
          "VodafoneUK",
          "EE",
          "O2"
        ),
        col = c("Blue", "Dark Green", "Pink", "Red", "Orange", "Purple"),
        lty = 1,
        cex = 0.8
      )
    })
  
  

  output$histogram2 <-
    renderPlot({
      ifelse(
        input$dataset == "ThreeUK",
        hist(
          ThreeUK$sent.val1,
          breaks = input$bins,
          main = paste("Histogram of sentiment values of ThreeUK"),
          xlab = paste("Sentiment values"),
          ylab = paste("Frequency"),
          ylim = c(0, 2000)
        ),
        ifelse(
          input$dataset == "VodafoneUK",
          hist(
            VodafoneUK$sent.val1,
            breaks = input$bins,
            main = paste("Histogram of sentiment values of VodafoneUK"),
            xlab = paste("Sentiment values"),
            ylab = paste("Frequency"),
            ylim = c(0, 2000)
          ),
          ifelse(
            input$dataset == "TescoMobile",
            hist(
              TescoMobile$sent.val1,
              breaks = input$bins,
              main = paste("Histogram of sentiment values of TescoMobile"),
              xlab = paste("Sentiment values"),
              ylab = paste("Frequency"),
              ylim = c(0, 50)
            ),
            ifelse(
              input$dataset == "VirginMobile",
              hist(
                VirginMobile$sent.val1,
                breaks = input$bins,
                main = paste("Histogram of sentiment values of VirginMobile"),
                xlab = paste("Sentiment values"),
                ylab = paste("Frequency"),
                ylim = c(0, 2000)
              ),
              ifelse(
                input$dataset == "EE",
                hist(
                  EE$sent.val1,
                  breaks = input$bins,
                  main = paste("Histogram of sentiment values of EE"),
                  xlab = paste("Sentiment values"),
                  ylab = paste("Frequency"),
                  ylim = c(0, 2000)
                ),
                hist(
                  O2$sent.val1,
                  breaks = input$bins,
                  main = paste("Histogram of sentiment values of O2"),
                  xlab = paste("Sentiment values"),
                  ylab = paste("Frequency"),
                  ylim = c(0, 2000)
                )
              )
            )
          )
        )
      )
      
      
    })
  
  output$line1 <- renderPlot({
    if (input$dataset == "ThreeUK") {
      plot(
        ThreeUK$sent.val1,
        type = "line",
        main = "Line Chart of sentiment values of ThreeUK",
        xlab = "Reviews",
        ylab = "Sentiment values"
      )
    }
    else if (input$dataset == "VodafoneUK") {
      plot(
        VodafoneUK$sent.val1,
        type = "line",
        main = "Line Chart of sentiment values of VodafoneUK",
        xlab = "Reviews",
        ylab = "Sentiment values"
      )
    }
    else if (input$dataset == "TescoMobile") {
      plot(
        TescoMobile$sent.val1,
        type = "line",
        main = "Line Chart of sentiment values of TescoMobile",
        xlab = "Reviews",
        ylab = "Sentiment values"
      )
    }
    else if (input$dataset ==
             "VirginMobile") {
      plot(
        VirginMobile$sent.val1,
        type = "line",
        main = "Line Chart of sentiment values of VirginMobile",
        xlab = "Reviews",
        ylab = "Sentiment values"
      )
    }
    else if (input$dataset ==
             "EE") {
      plot(
        EE$sent.val1,
        type = "line",
        main = "Line Chart of sentiment values of EE",
        xlab = "Reviews",
        ylab = "Sentiment values"
      )
    }
    else{
      plot(
        O2$sent.val1,
        type = "line",
        main = "Line Chart of sentiment values of O2",
        xlab = "Reviews",
        ylab = "Sentiment values"
      )
    }
  })
  
  output$bar1 <- renderPlot({
    if (input$dataset == "ThreeUK") {
      barplot(
        table(ThreeUK$Month.and.Year),
        main = "Bar Chart of no. of monthly reviews of ThreeUK",
        xlab = "Months",
        ylab = "Count"
      )
    }
    else if (input$dataset == "VodafoneUK") {
      barplot(
        table(VodafoneUK$Month.and.Year),
        main = "Bar Chart of no. of monthly reviews of VodafoneUK",
        xlab = "Months",
        ylab = "Count"
      )
    }
    else if (input$dataset == "TescoMobile") {
      barplot(
        table(TescoMobile$Month.and.Year),
        main = "Bar Chart of no. of monthly reviews of TescoMobile",
        xlab = "Months",
        ylab = "Count"
      )
    }
    else if (input$dataset == "VirginMobile") {
      barplot(
        table(VirginMobile$Month.and.Year),
        main = "Bar Chart of no. of monthly reviews of VirginMobile",
        xlab = "Months",
        ylab = "Count"
      )
    }
    else if (input$dataset == "EE") {
      barplot(
        table(EE$Month.and.Year),
        main = "Bar Chart of no. of monthly reviews of EE",
        xlab = "Months",
        ylab = "Count"
      )
    }
    else{
      barplot(
        table(O2$Month.and.Year),
        main = "Bar Chart of no. of monthly reviews of O2",
        xlab = "Months",
        ylab = "Count"
      )
    }
  })
  
  output$bar2 <- renderPlot({
    if (input$dataset == "ThreeUK") {
      barplot(
        table(ThreeUK$category_senti1, sort(ThreeUK$Month.and.Year)),
        main = "Bar Chart of no. of monthly reviews of ThreeUK",
        axisnames = F,
        ylab = "Count",
        ylim = c(0, 200),
        col = c("Red", "yellow", "Dark Green"),
        legend = rownames(
          table(ThreeUK$category_senti1, ThreeUK$Month.and.Year)
        )
      )
      axis(1,
           at = 1:19,
           labels = ThreeUK[1:19, 37],
           las = 2)
    }
    else if (input$dataset == "VodafoneUK") {
      barplot(
        table(VodafoneUK$category_senti1, VodafoneUK$Month.and.Year),
        axisnames = F,
        main = "Bar Chart of no. of monthly reviews of VodafoneUK",
        ylab = "Count",
        ylim = c(0, 200),
        col = c("Red", "yellow", "Dark Green"),
        legend = rownames(
          table(ThreeUK$category_senti1, ThreeUK$Month.and.Year)
        )
      )
      axis(1,
           at = 1:15,
           labels = VodafoneUK[1:15, 37],
           las = 2)
    }
    else if (input$dataset == "TescoMobile") {
      barplot(
        table(
          TescoMobile$category_senti1,
          TescoMobile$Month.and.Year
        ),
        main = "Bar Chart of no. of monthly reviews of TescoMobile",
        ylim = c(0, 10),
        ylab = "Count",
        axisnames = F,
        col = c("Red", "yellow", "Dark Green"),
        legend = rownames(
          table(ThreeUK$category_senti1, ThreeUK$Month.and.Year)
        )
      )
      axis(1,
           at = 1:14,
           labels = TescoMobile[1:14, 37],
           las = 2)
    }
    else if (input$dataset == "VirginMobile") {
      barplot(
        table(
          VirginMobile$category_senti1,
          VirginMobile$Month.and.Year
        ),
        main = "Bar Chart of no. of monthly reviews of VirginMobile",
        ylim = c(0, 100),
        axisnames = F,
        ylab = "Count",
        col = c("Red", "yellow", "Dark Green"),
        legend = rownames(
          table(ThreeUK$category_senti1, ThreeUK$Month.and.Year)
        )
      )
      axis(1,
           at = 1:51,
           labels = VirginMobile[1:51, 37],
           las = 2)
    }
    else if (input$dataset == "EE") {
      barplot(
        table(EE$category_senti1, EE$Month.and.Year),
        main = "Bar Chart of no. of monthly reviews of EE",
        axisnames = F,
        ylim = c(0, 100),
        ylab = "Count",
        col = c("Red", "yellow", "Dark Green"),
        legend = rownames(
          table(ThreeUK$category_senti1, ThreeUK$Month.and.Year)
        )
      )
      axis(1,
           at = 1:37,
           labels = EE[1:37, 37],
           las = 2)
      
    }
    else{
      barplot(
        table(O2$category_senti1, O2$Month.and.Year),
        main = "Bar Chart of no. of monthly reviews of O2",
        axisnames = F,
        xlab = "Months",
        ylab = "Count",
        col = c("Red", "yellow", "Dark Green"),
        legend = rownames(
          table(ThreeUK$category_senti1, ThreeUK$Month.and.Year)
        )
      )
      axis(1, at = 1:81, labels = O2[1:81, 37])
    }
  })
  
  output$line2 <-
    renderPlot({
      if (input$dataset == "ThreeUK") {
        plot(
          ThreeUK[, input$inField2],
          type = "line",
          xlim = c(0, 200),
          xlab = "Reviews",
          ylab = "Sentiment values",
          main = "Line Chart of monthly sentiment values of ThreeUK"
        )
      }
      else if (input$dataset == "VodafoneUK") {
        plot(
          VodafoneUK[, input$inField2],
          type = "line",
          xlim = c(0, 180),
          main = "Line Chart of monthly sentiment values of VodafoneUK",
          xlab = "Reviews",
          ylab = "Sentiment values"
        )
      }
      else if (input$dataset == "TescoMobile") {
        plot(
          TescoMobile[, input$inField2],
          type = "line",
          xlim = c(0, 10),
          main = "Line Chart of monthly sentiment values of TescoMobile",
          xlab = "Reviews",
          ylab = "Sentiment values"
        )
      }
      else if (input$dataset == "VirginMobile") {
        plot(
          VirginMobile[, input$inField2],
          type = "line",
          xlim = c(0, 75),
          main = "Line Chart of monthly sentiment values of VirginMobile",
          xlab = "Reviews",
          ylab = "Sentiment values"
        )
      }
      else if (input$dataset == "EE") {
        plot(
          EE[, input$inField2],
          type = "line",
          xlim = c(0, 110),
          main = "Line Chart of monthly sentiment values of EE",
          xlab = "Reviews",
          ylab = "Sentiment values"
        )
      }
      else{
        plot(
          O2[, input$inField2],
          type = "line",
          xlim = c(0, 75),
          main = "Line Chart of monthly sentiment values of O2",
          xlab = "Reviews",
          ylab = "Sentiment values"
        )
      }
    })
  
  output$pie1 <- renderPlot({
    if (input$dataset == "ThreeUK") {
      pie(
        x = table(ThreeUK$category_senti1),
        labels = paste(names(table(
          ThreeUK$category_senti1
        )), " ", round(
          table(ThreeUK$category_senti1) / sum(table(ThreeUK$category_senti1)) * 100, 2
        ), "%", sep = ''),
        col = c("Red", "yellow", "Dark Green"),
        main = "Pie Chart of sentiment categories of ThreeUK"
      )
      
    }
    else if (input$dataset == "VodafoneUK") {
      pie(
        x = table(VodafoneUK$category_senti1),
        labels = paste(names(
          table(VodafoneUK$category_senti1)
        ), " ", round(
          table(VodafoneUK$category_senti1) / sum(table(VodafoneUK$category_senti1)) *
            100, 2
        ), "%", sep = ''),
        col = c("Red", "yellow", "Dark Green"),
        main = "Pie Chart of sentiment categories of VodafoneUK"
      )
      
    }
    else if (input$dataset == "TescoMobile") {
      pie(
        x = table(TescoMobile$category_senti1),
        labels = paste(names(
          table(TescoMobile$category_senti1)
        ), " ", round(
          table(TescoMobile$category_senti1) / sum(table(TescoMobile$category_senti1)) *
            100, 2
        ), "%", sep = ''),
        col = c("Red", "yellow", "Dark Green"),
        main = "Pie Chart of sentiment categories of TescoMobile"
      )
    }
    else if (input$dataset == "VirginMobile") {
      pie(
        x = table(VirginMobile$category_senti1),
        labels = paste(names(
          table(VirginMobile$category_senti1)
        ), " ", round(
          table(VirginMobile$category_senti1) / sum(table(VirginMobile$category_senti1)) *
            100,
          2
        ), "%", sep = ''),
        col = c("Red", "yellow", "Dark Green"),
        main = "Pie Chart of sentiment categories of VirginMobile"
      )
    }
    else if (input$dataset == "EE") {
      pie(
        x = table(EE$category_senti1),
        labels = paste(names(table(
          EE$category_senti1
        )), " ", round(
          table(EE$category_senti1) / sum(table(EE$category_senti1)) * 100, 2
        ), "%", sep = ''),
        col = c("Red", "yellow", "Dark Green"),
        main = "Pie Chart of sentiment categories of EE"
      )
    }
    else{
      pie(
        x = table(O2$category_senti1),
        labels = paste(names(table(
          O2$category_senti1
        )), " ", round(
          table(O2$category_senti1) / sum(table(O2$category_senti1)) * 100, 2
        ), "%", sep = ''),
        col = c("Red", "yellow", "Dark Green"),
        main = "Pie Chart of sentiment categories of O2"
      )
    }
  })
  
  output$word10 <- renderPlot({
    if (input$emotion == "Positive") {
      p11_th <- subset(nth, cat_sent_th == "Positive")
      p11_th <- as.data.frame(p11_th)
      wordcloud(
        words = p11_th$`wordcloud_bigram_th.ngrams`,
        freq = p11_th$`wordcloud_bigram_th.freq`,
        min.freq = 2,
        max.words = 500,
        random.order = FALSE,
        color = "Dark Green"
      )
      
    }
    else{
      b11_th <- subset(nth, cat_sent_th == "Negative")
      b11_th <- as.data.frame(b11_th)
      wordcloud(
        words = b11_th$`wordcloud_bigram_th.ngrams`,
        freq = b11_th$`wordcloud_bigram_th.freq`,
        min.freq = 2,
        max.words = 200,
        random.order = FALSE,
        colors = "Red"
      )
    }
  })
  
  output$word11 <- renderPlot({
    if (input$emotion == "Positive") {
      p11_vo <- subset(nvo, cat_sent_vo == "Positive")
      p11_vo <- as.data.frame(p11_vo)
      wordcloud(
        words = p11_vo$`wordcloud_bigram_vo.ngrams`,
        freq = p11_vo$`wordcloud_bigram_vo.freq`,
        min.freq = 2,
        max.words = 500,
        random.order = FALSE,
        color = "Dark Green"
      )
    }
    else{
      b11_vo <- subset(nvo, cat_sent_vo == "Negative")
      b11_vo <- as.data.frame(b11_vo)
      wordcloud(
        words = b11_vo$`wordcloud_bigram_vo.ngrams`,
        freq = b11_vo$`wordcloud_bigram_vo.freq`,
        min.freq = 2,
        max.words = 500,
        random.order = FALSE,
        colors = "Red"
      )
      
    }
  })
  
  
  output$word12 <- renderPlot({
    if (input$emotion == "Positive") {
      p11_oo <- subset(noo, cat_sent_oo == "Positive")
      p11_oo <- as.data.frame(p11_oo)
      wordcloud(
        words = p11_oo$`wordcloud_bigram_oo.ngrams`,
        freq = p11_oo$`wordcloud_bigram_oo.freq`,
        min.freq = 2,
        max.words = 500,
        random.order = FALSE,
        color = "Dark Green"
      )
      
    }
    else{
      b11_oo <- subset(noo, cat_sent_oo == "Negative")
      b11_oo <- as.data.frame(b11_oo)
      text("title of my cloud")
      wordcloud(
        words = b11_oo$`wordcloud_bigram_oo.ngrams`,
        freq = b11_oo$`wordcloud_bigram_oo.freq`,
        min.freq = 2,
        max.words = 500,
        random.order = FALSE,
        colors = "Red"
      )
    }
  })
  
  output$word13 <- renderPlot({
    if (input$emotion == "Positive") {
      p11_vi <- subset(nvi, cat_sent_vi == "Positive")
      p11_vi <- as.data.frame(p11_vi)
      wordcloud(
        words = p11_vi$`wordcloud_bigram_vi.ngrams`,
        freq = p11_vi$`wordcloud_bigram_vi.freq`,
        min.freq = 2,
        max.words = 500,
        random.order = FALSE,
        color = "Dark Green"
      )
    }
    else{
      b11_vi <- subset(nvi, cat_sent_vi == "Negative")
      b11_vi <- as.data.frame(b11_vi)
      wordcloud(
        words = b11_vi$`wordcloud_bigram_vi.ngrams`,
        freq = b11_vi$`wordcloud_bigram_vi.freq`,
        min.freq = 2,
        max.words = 500,
        random.order = FALSE,
        colors = "Red"
      )
    }
  })
  
  output$word14 <- renderPlot({
    if (input$emotion == "Positive") {
      p11_ee <- subset(nee, cat_sent_ee == "Positive")
      p11_ee <- as.data.frame(p11_ee)
      wordcloud(
        words = p11_ee$`wordcloud_bigram_ee.ngrams`,
        freq = p11_ee$`wordcloud_bigram_ee.freq`,
        min.freq = 2,
        max.words = 500,
        random.order = FALSE,
        color = "Dark Green"
      )
    }
    else{
      b11_ee <- subset(nee, cat_sent_ee == "Negative")
      b11_ee <- as.data.frame(b11_ee)
      wordcloud(
        words = b11_ee$`wordcloud_bigram_ee.ngrams`,
        freq = b11_ee$`wordcloud_bigram_ee.freq`,
        min.freq = 2,
        max.words = 500,
        random.order = FALSE,
        colors = "Red"
      )
      
    }
  })
  output$word15 <- renderPlot({
    if (input$emotion == "Positive") {
      p11_te <- subset(nte, cat_sent_te == "Positive")
      p11_te <- as.data.frame(p11_te)
      wordcloud(
        words = p11_te$`wordcloud_bigram_te.ngrams`,
        freq = p11_te$`wordcloud_bigram_te.freq`,
        min.freq = 2,
        max.words = 500,
        random.order = FALSE,
        color = "Dark Green"
      )
    }
    else{
      b11_te <- subset(nte, cat_sent_te == "Negative")
      
      b11_te <- as.data.frame(b11_te)
      wordcloud(
        words = b11_te$`wordcloud_bigram_te.ngrams`,
        freq = b11_te$`wordcloud_bigram_te.freq`,
        min.freq = 2,
        max.words = 500,
        random.order = FALSE,
        colors = "Red"
      )
    }
  })
  
  output$hist3 <-
    renderPlot({
      if (input$dataset == "ThreeUK" & input$inField1 == "anger") {
        hist(
          ThreeUK$anger[ThreeUK$anger >= 1 & ThreeUK$anger <= 11],
          breaks = input$bins,
          main = "Histogram of anger for ThreeUK",
          xlab = "Anger",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
        
      }
      else if (input$dataset == "ThreeUK" &
               input$inField1 == "anticipation") {
        hist(
          ThreeUK$anticipation[ThreeUK$anticipation >= 1 &
                                 ThreeUK$anticipation <= 11],
          breaks = input$bins,
          main = "Histogram of anticipation for ThreeUK",
          xlab = "Anticipation",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "ThreeUK" &
               input$inField1 == "disgust") {
        hist(
          ThreeUK$disgust[ThreeUK$disgust >= 1 & ThreeUK$disgust <= 11],
          breaks = input$bins,
          main = "Histogram of disgust for ThreeUK",
          xlab = "Disgust",
          ylim = c(0, 800)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "ThreeUK" & input$inField1 == "fear") {
        hist(
          ThreeUK$fear[ThreeUK$fear >= 1 & ThreeUK$fear <= 11],
          breaks = input$bins,
          main = "Histogram of fear for ThreeUK",
          xlab = "Fear",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "ThreeUK" & input$inField1 == "joy") {
        hist(
          ThreeUK$joy[ThreeUK$joy >= 1 & ThreeUK$joy <= 11],
          breaks = input$bins,
          main = "Histogram of joy for ThreeUK",
          xlab = "Joy",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "ThreeUK" &
               input$inField1 == "sadness") {
        hist(
          ThreeUK$sadness[ThreeUK$sadness >= 1 &
                            ThreeUK$sadness <= 11],
          breaks = input$bins,
          main = "Histogram of sadness for ThreeUK",
          xlab = "Sadness",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "ThreeUK" &
               input$inField1 == "surprise") {
        hist(
          ThreeUK$surprise[ThreeUK$surprise >= 1 &
                             ThreeUK$surprise <= 11],
          breaks = input$bins,
          main = "Histogram of surprise for ThreeUK",
          xlab = "Surprise",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "ThreeUK" & input$inField1 == "trust") {
        hist(
          ThreeUK$trust[ThreeUK$trust >= 1 &
                          ThreeUK$trust <= 11],
          breaks = input$bins,
          main = "Histogram of trust for ThreeUK",
          xlab = "Trust",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "ThreeUK" &
               input$inField1 == "negative") {
        hist(
          ThreeUK$negative[ThreeUK$negative >= 1 &
                             ThreeUK$negative <= 11],
          breaks = input$bins,
          main = "Histogram of negative for ThreeUK",
          xlab = "Negative",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "ThreeUK" &
               input$inField1 == "positive") {
        hist(
          ThreeUK$positive[ThreeUK$positive >= 1 &
                             ThreeUK$positive <= 11],
          breaks = input$bins,
          main = "Histogram of positive for ThreeUK",
          xlab = "Positive",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      
      else if (input$dataset == "VirginMobile" &
               input$inField1 == "anger") {
        hist(
          VirginMobile$anger[VirginMobile$anger >= 1 &
                               VirginMobile$anger <= 11],
          breaks = input$bins,
          main = "Histogram of anger for VirginMobile",
          xlab = "Anger",
          ylim = c(0, 500)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
        
      }
      else if (input$dataset == "VirginMobile" &
               input$inField1 == "anticipation") {
        hist(
          VirginMobile$anticipation[VirginMobile$anticipation >= 1 &
                                      VirginMobile$anticipation <= 11],
          breaks = input$bins,
          main = "Histogram of anticipation for VirginMobile",
          xlab = "Anticipation",
          ylim = c(0, 500)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "VirginMobile" &
               input$inField1 == "disgust") {
        hist(
          VirginMobile$disgust[VirginMobile$disgust >= 1 &
                                 VirginMobile$disgust <= 11],
          breaks = input$bins,
          main = "Histogram of disgust for VirginMobile",
          xlab = "Disgust",
          ylim = c(0, 500)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "VirginMobile" &
               input$inField1 == "fear") {
        hist(
          VirginMobile$fear[VirginMobile$fear >= 1 &
                              VirginMobile$fear <= 11],
          breaks = input$bins,
          main = "Histogram of fear for VirginMobile",
          xlab = "Fear",
          ylim = c(0, 600)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "VirginMobile" &
               input$inField1 == "joy") {
        hist(
          VirginMobile$joy[VirginMobile$joy >= 1 &
                             VirginMobile$joy <= 11],
          breaks = input$bins,
          main = "Histogram of joy for VirginMobile",
          xlab = "Joy",
          ylim = c(0, 600)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "VirginMobile" &
               input$inField1 == "sadness") {
        hist(
          VirginMobile$sadness[VirginMobile$sadness >= 1 &
                                 VirginMobile$sadness <= 11],
          breaks = input$bins,
          main = "Histogram of sadness for VirginMobile",
          xlab = "Sadness",
          ylim = c(0, 600)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "VirginMobile" &
               input$inField1 == "surprise") {
        hist(
          VirginMobile$surprise[VirginMobile$surprise >= 1 &
                                  VirginMobile$surprise <= 11],
          breaks = input$bins,
          main = "Histogram of surprise for VirginMobile",
          xlab = "Surprise",
          ylim = c(0, 300)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "VirginMobile" &
               input$inField1 == "trust") {
        hist(
          VirginMobile$trust[VirginMobile$trust >= 1 &
                               VirginMobile$trust <= 11],
          breaks = input$bins,
          main = "Histogram of trust for VirginMobile",
          xlab = "Trust",
          ylim = c(0, 500)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "VirginMobile" &
               input$inField1 == "negative") {
        hist(
          VirginMobile$negative[VirginMobile$negative >= 1 &
                                  VirginMobile$negative <= 11],
          breaks = input$bins,
          main = "Histogram of negative for VirginMobile",
          xlab = "Negative",
          ylim = c(0, 500)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "VirginMobile" &
               input$inField1 == "positive") {
        hist(
          VirginMobile$positive[VirginMobile$positive >= 1 &
                                  VirginMobile$positive <= 11],
          breaks = input$bins,
          main = "Histogram of positive for VirginMobile",
          xlab = "Positive",
          ylim = c(0, 600)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "VodafoneUK" &
               input$inField1 == "anger") {
        hist(
          VodafoneUK$anger[VodafoneUK$anger >= 1 &
                             VodafoneUK$anger <= 11],
          breaks = input$bins,
          main = "Histogram of anger for VodafoneUK",
          xlab = "Anger",
          ylim = c(0, 1200)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
        
      }
      else if (input$dataset == "VodafoneUK" &
               input$inField1 == "anticipation") {
        hist(
          VodafoneUK$anticipation[VodafoneUK$anticipation >= 1 &
                                    VodafoneUK$anticipation <= 11],
          breaks = input$bins,
          main = "Histogram of anticipation for VodafoneUK",
          xlab = "Anticipation",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:20, labels = seq(1, 20, 1))
      }
      else if (input$dataset == "VodafoneUK" &
               input$inField1 == "disgust") {
        hist(
          VodafoneUK$disgust[VodafoneUK$disgust >= 1 &
                               VodafoneUK$disgust <= 11],
          breaks = input$bins,
          main = "Histogram of disgust for VodafoneUK",
          xlab = "Disgust",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "VodafoneUK" &
               input$inField1 == "fear") {
        hist(
          VodafoneUK$fear[VodafoneUK$fear >= 1 &
                            VodafoneUK$fear <= 11],
          breaks = input$bins,
          main = "Histogram of fear for VodafoneUK",
          xlab = "Fear",
          ylim = c(0, 1200)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "VodafoneUK" &
               input$inField1 == "joy") {
        hist(
          VodafoneUK$joy[VodafoneUK$joy >= 1 &
                           VodafoneUK$joy <= 11],
          breaks = input$bins,
          main = "Histogram of joy for VodafoneUK",
          xlab = "Joy",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "VodafoneUK" &
               input$inField1 == "sadness") {
        hist(
          VodafoneUK$sadness[VodafoneUK$sadness >= 1 &
                               VodafoneUK$sadness <= 11],
          breaks = input$bins,
          main = "Histogram of sadness for VodafoneUK",
          xlab = "Sadness",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "VodafoneUK" &
               input$inField1 == "surprise") {
        hist(
          VodafoneUK$surprise[VodafoneUK$surprise >= 1 &
                                VodafoneUK$surprise <= 11],
          breaks = input$bins,
          main = "Histogram of surprise for VodafoneUK",
          xlab = "Surprise",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "VodafoneUK" &
               input$inField1 == "trust") {
        hist(
          VodafoneUK$trust[VodafoneUK$trust >= 1 &
                             VodafoneUK$trust <= 11],
          breaks = input$bins,
          main = "Histogram of trust for VodafoneUK",
          xlab = "Trust",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "VodafoneUK" &
               input$inField1 == "negative") {
        hist(
          VodafoneUK$negative[VodafoneUK$negative >= 1 &
                                VodafoneUK$negative <= 11],
          breaks = input$bins,
          main = "Histogram of negative for VodafoneUK",
          xlab = "Negative",
          ylim = c(0, 1500)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "VodafoneUK" &
               input$inField1 == "positive") {
        hist(
          VodafoneUK$positive[VodafoneUK$positive >= 1 &
                                VodafoneUK$positive <= 11],
          breaks = input$bins,
          main = "Histogram of positive for VodafoneUK",
          xlab = "Positive",
          ylim = c(0, 1500)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "O2" & input$inField1 == "anger") {
        hist(
          O2$anger[O2$anger >= 1 &
                     O2$anger <= 11],
          breaks = input$bins,
          main = "Histogram of anger for O2",
          xlab = "Anger",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
        
      }
      else if (input$dataset == "O2" &
               input$inField1 == "anticipation") {
        hist(
          O2$anticipation[O2$anticipation >= 1 &
                            O2$anticipation <= 11],
          breaks = input$bins,
          main = "Histogram of anticipation for O2",
          xlab = "Anticipation",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "O2" & input$inField1 == "disgust") {
        hist(
          O2$disgust[O2$disgust >= 1 &
                       O2$disgust <= 11],
          breaks = input$bins,
          main = "Histogram of disgust for O2",
          xlab = "Disgust",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "O2" & input$inField1 == "fear") {
        hist(
          O2$fear[O2$fear >= 1 &
                    O2$fear <= 11],
          breaks = input$bins,
          main = "Histogram of fear for O2",
          xlab = "Fear",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "O2" & input$inField1 == "joy") {
        hist(
          O2$joy[O2$joy >= 1 &
                   O2$joy <= 11],
          breaks = input$bins,
          main = "Histogram of joy for O2",
          xlab = "Joy",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "O2" & input$inField1 == "sadness") {
        hist(
          O2$sadness[O2$sadness >= 1 &
                       O2$sadness <= 11],
          breaks = input$bins,
          main = "Histogram of sadness for O2",
          xlab = "Sadness",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "O2" & input$inField1 == "surprise") {
        hist(
          O2$surprise[O2$surprise >= 1 &
                        O2$surprise <= 11],
          breaks = input$bins,
          main = "Histogram of surprise for O2",
          xlab = "Surprise",
          ylim = c(0, 800)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "O2" & input$inField1 == "trust") {
        hist(
          O2$trust[O2$trust >= 1 &
                     O2$trust <= 11],
          breaks = input$bins,
          main = "Histogram of trust for O2",
          xlab = "Trust",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "O2" & input$inField1 == "negative") {
        hist(
          O2$negative[O2$negative >= 1 &
                        O2$negative <= 11],
          breaks = input$bins,
          main = "Histogram of negative for O2",
          xlab = "Negative",
          ylim = c(0, 800)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "O2" & input$inField1 == "positive") {
        hist(
          O2$positive[O2$positive >= 1 &
                        O2$positive <= 11],
          breaks = input$bins,
          main = "Histogram of positive for O2",
          xlab = "Positive",
          ylim = c(0, 1500)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "TescoMobile" &
               input$inField1 == "anger") {
        hist(
          TescoMobile$anger[TescoMobile$anger >= 1 &
                              TescoMobile$anger <= 11],
          breaks = input$bins,
          main = "Histogram of anger for TescoMobile",
          xlab = "Anger",
          ylim = c(0, 25)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
        
      }
      else if (input$dataset == "TescoMobile" &
               input$inField1 == "anticipation") {
        hist(
          TescoMobile$anticipation[TescoMobile$anticipation >= 1 &
                                     TescoMobile$anticipation <= 11],
          breaks = input$bins,
          main = "Histogram of anticipation for TescoMobile",
          xlab = "Anticipation",
          ylim = c(0, 50)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "TescoMobile" &
               input$inField1 == "disgust") {
        hist(
          TescoMobile$disgust[TescoMobile$disgust >= 1 &
                                TescoMobile$disgust <= 11],
          breaks = input$bins,
          main = "Histogram of disgust for TescoMobile",
          xlab = "Disgust",
          ylim = c(0, 20)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "TescoMobile" &
               input$inField1 == "fear") {
        hist(
          TescoMobile$fear[TescoMobile$fear >= 1 &
                             TescoMobile$fear <= 11],
          breaks = input$bins,
          main = "Histogram of fear for TescoMobile",
          xlab = "Fear",
          ylim = c(0, 20)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "TescoMobile" &
               input$inField1 == "joy") {
        hist(
          TescoMobile$joy[TescoMobile$joy >= 1 &
                            TescoMobile$joy <= 11],
          breaks = input$bins,
          main = "Histogram of joy for TescoMobile",
          xlab = "Joy",
          ylim = c(0, 20)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "TescoMobile" &
               input$inField1 == "sadness") {
        hist(
          TescoMobile$sadness[TescoMobile$sadness >= 1 &
                                TescoMobile$sadness <= 11],
          breaks = input$bins,
          main = "Histogram of sadness for TescoMobile",
          xlab = "Sadness",
          ylim = c(0, 30)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "TescoMobile" &
               input$inField1 == "surprise") {
        hist(
          TescoMobile$surprise[TescoMobile$surprise >= 1 &
                                 TescoMobile$surprise <= 11],
          main = "Histogram of surprise for TescoMobile",
          xlab = "Surprise",
          ylim = c(0, 20)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "TescoMobile" &
               input$inField1 == "trust") {
        hist(
          TescoMobile$trust[TescoMobile$trust >= 1 &
                              TescoMobile$trust <= 11],
          breaks = input$bins,
          main = "Histogram of trust for TescoMobile",
          xlab = "Trust",
          ylim = c(0, 30)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "TescoMobile" &
               input$inField1 == "negative") {
        hist(
          TescoMobile$negative[TescoMobile$negative >= 1 &
                                 TescoMobile$negative <= 11],
          breaks = input$bins,
          main = "Histogram of negative for TescoMobile",
          xlab = "Negative",
          ylim = c(0, 30)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "TescoMobile" &
               input$inField1 == "positive") {
        hist(
          TescoMobile$positive[TescoMobile$positive >= 1 &
                                 TescoMobile$positive <= 11],
          breaks = input$bins,
          main = "Histogram of positive for TescoMobile",
          xlab = "Positive",
          ylim = c(0, 30)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "EE" & input$inField1 == "anger") {
        hist(
          EE$anger[EE$anger >= 1 &
                     EE$anger <= 11],
          breaks = input$bins,
          main = "Histogram of anger for EE",
          xlab = "Anger",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
        
      }
      else if (input$dataset == "EE" &
               input$inField1 == "anticipation") {
        hist(
          EE$anticipation[EE$anticipation >= 1 &
                            EE$anticipation <= 11],
          breaks = input$bins,
          main = "Histogram of anticipation for EE",
          xlab = "Anticipation",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "EE" & input$inField1 == "disgust") {
        hist(
          EE$disgust[EE$disgust >= 1 &
                       EE$disgust <= 11],
          breaks = input$bins,
          main = "Histogram of disgust for EE",
          xlab = "Disgust",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "EE" & input$inField1 == "fear") {
        hist(
          EE$fear[EE$fear >= 1 &
                    EE$fear <= 11],
          breaks = input$bins,
          main = "Histogram of fear for EE",
          xlab = "Fear",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "EE" & input$inField1 == "joy") {
        hist(
          EE$joy[EE$joy >= 1 &
                   EE$joy <= 11],
          breaks = input$bins,
          main = "Histogram of joy for EE",
          xlab = "Joy",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "EE" & input$inField1 == "sadness") {
        hist(
          EE$sadness[EE$sadness >= 1 &
                       EE$sadness <= 11],
          breaks = input$bins,
          main = "Histogram of sadness for EE",
          xlab = "Sadness",
          ylim = c(0, 1200)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "EE" & input$inField1 == "surprise") {
        hist(
          EE$surprise[EE$surprise >= 1 &
                        EE$surprise <= 11],
          breaks = input$bins,
          main = "Histogram of surprise for EE",
          xlab = "Surprise",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "EE" & input$inField1 == "trust") {
        hist(
          EE$trust[EE$trust >= 1 &
                     EE$trust <= 11],
          breaks = input$bins,
          main = "Histogram of trust for EE",
          xlab = "Trust",
          ylim = c(0, 1000)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else if (input$dataset == "EE" & input$inField1 == "negative") {
        hist(
          EE$negative[EE$negative >= 1 &
                        EE$negative <= 11],
          breaks = input$bins,
          main = "Histogram of negative for EE",
          xlab = "Negative",
          ylim = c(0, 1500)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      else{
        hist(
          EE$positive[EE$positive >= 1 &
                        EE$positive <= 11],
          breaks = input$bins,
          main = "Histogram of positive for EE",
          xlab = "Positive",
          ylim = c(0, 1500)
        )
        axis(1, at = 1:11, labels = c(seq(1, 10, 1), "11+"))
      }
      
      
    })
})
