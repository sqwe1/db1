shinyServer(function(input,output,session) {
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Th" = Th,
           "Vo" = Vo,
           "Te"=Te,
           "Vi"=Vi,
           "EE"=EE,
           "Oo"=OO)
  })
  
  emotionInput <- reactive({
    switch(input$emotion,
           c("Positive","Negative") )
  })
  
  # output$DateRange <- renderText({
  #   # make sure end date later than start date
  #   validate(
  #     need(input$dates[2] > input$dates[1], "end date is earlier than start date"
  #     )
  #   )
  #   
  #   # make sure greater than 2 week difference
  #   validate(
  #     need(difftime(input$dates[2], input$dates[1], "days") > 14, "date range less the 14 days"
  #     )
  #   )
  #   
  #   paste("Your date range is", 
  #         difftime(input$dates[2], input$dates[1], units="days"),
  #         "days")
  # })

  output$No.ofreviews<- renderInfoBox({
    
    if(input$dataset=="Th"){
      infoBox(title = "No. of reviews", 
              value = nrow(Th)
      ) }
    else if (input$dataset=="Vo"){
      infoBox(title = "No. of reviews", 
              value = nrow(Vo)
            
      )}
    else if (input$dataset=="Te"){
      infoBox(title = "No. of reviews", 
              value = nrow(Te)
      )}
    else if (input$dataset=="Vi"){
      infoBox(title = "No. of reviews", 
              value = nrow(Vi)
      )}
    else if (input$dataset=="EE"){
      infoBox(title = "No. of reviews", 
              value = nrow(EE)
      )}
    else {infoBox(title = "No. of reviews", 
                  value = nrow(OO))}
    
  })

    output$Positive <- renderInfoBox({
    
    if(input$dataset=="Th"){
      infoBox(title = "", 
              value = as.data.frame(table(Th$category_senti1))[3,],
              icon("arrow-up"),
              subtitle = "",
              color = "green"
      ) }
    else if (input$dataset=="Vo"){
      infoBox(title = "", 
              value = as.data.frame(table(Vo$category_senti1))[3,],
              icon("arrow-up"),
              subtitle = "",
              color = "green"
      )}
    else if (input$dataset=="Te"){
      infoBox(title = "", 
              value = as.data.frame(table(Te$category_senti1))[3,],
              icon("arrow-up"),
              subtitle = "",
              color = "green"
      )}
    else if (input$dataset=="Vi"){
      infoBox(title = "", 
              value = as.data.frame(table(Vi$category_senti1))[3,],
              icon("arrow-up"),
              subtitle = "",
              color = "green"
      )}
    else if (input$dataset=="EE"){
      infoBox(title = "", 
              value = as.data.frame(table(EE$category_senti1))[3,],
              icon("arrow-up"),
              subtitle = "",
              color = "green"
      )}
    else {infoBox(title = "", 
                  value = as.data.frame(table(OO$category_senti1))[3,],
                  icon("arrow-up"),
                  subtitle = "",
                  color = "green")}
    
  })
  output$Neutral <- renderInfoBox({if(input$dataset=="Th"){
    infoBox(title = "", 
            value = as.data.frame(table(Th$category_senti1))[2,],
            icon("arrows-h"),
            subtitle = "",
            color = "yellow"
    ) }
    else if (input$dataset=="Vo"){
      infoBox(title = "", 
              value = as.data.frame(table(Vo$category_senti1))[2,],
              icon("arrows-h"),
              subtitle = "",
              color = "yellow"
      )}
    else if (input$dataset=="Te"){
      infoBox(title = "", 
              value = as.data.frame(table(Te$category_senti1))[2,],
              icon("arrows-h"),
              subtitle = "",
              color = "yellow"
      )}
    else if (input$dataset=="Vi"){
      infoBox(title = "", 
              value = as.data.frame(table(Vi$category_senti1))[2,],
              icon("arrows-h"),
              subtitle = "",
              color = "yellow"
      )}
    else if (input$dataset=="EE"){
      infoBox(title = "", 
              value = as.data.frame(table(EE$category_senti1))[2,],
              icon("arrows-h"),
              subtitle = "",
              color = "yellow"
      )}
    else {infoBox(title = "", 
                  value = as.data.frame(table(OO$category_senti1))[2,],
                  icon("arrows-h"),
                  subtitle = "",
                  color = "yellow")}
    
  })
  
  
  output$Negative <- renderInfoBox({
    if(input$dataset=="Th"){
      infoBox(title = "", 
              value = as.data.frame(table(Th$category_senti1))[1,],
              icon("arrow-down"),
              subtitle = "",
              color = "red"
      ) }
    else if (input$dataset=="Vo"){
      infoBox(title = "", 
              value = as.data.frame(table(Vo$category_senti1))[1,],
              icon("arrow-down"),
              subtitle = "",
              color = "red"
      )}
    else if (input$dataset=="Te"){
      infoBox(title = "", 
              value = as.data.frame(table(Te$category_senti1))[1,],
              icon("arrow-down"),
              subtitle = "",
              color = "red"
      )}
    else if (input$dataset=="Vi"){
      infoBox(title = "", 
              value = as.data.frame(table(Vi$category_senti1))[1,],
              icon("arrow-down"),
              subtitle = "",
              color = "red"
      )}
    else if (input$dataset=="EE"){
      infoBox(title = "", 
              value = as.data.frame(table(EE$category_senti1))[1,],
              icon("arrow-down"),
              subtitle = "",
              color = "red"
      )}
    else {infoBox(title = "", 
                  value = as.data.frame(table(OO$category_senti1))[1,],
                  icon("arrow-down"),
                  subtitle = "",
                  color = "red")}
    
  })
  
  
  output$all<-renderPlot({plot(table(Th$Month.and.Year),type = "line",col="Blue",xaxt="n",main="Volume chart for all months",xlab="Months",ylab = "Frequency",ylim=c(0,500))
    lines(table(Te$Month.and.Year),type="line",col="Dark Green")
    lines(table(Vi$Month.and.Year),type="line",col="Maroon")
    lines(table(Vo$Month.and.Year),type="line",col="Red")
    lines(table(EE$Month.and.Year),type = "line",col="Orange")
    lines(table(OO$Month.and.Year),type="line",col="Purple")
    axis(1,at=1:19, labels=Th[1:19,37])
    legend(15,520,legend = c("Three","Tesco mobile","Virgin mobile","Vodafone","EE","O2"),
           col=c("Blue","Dark Green","Pink","Red","Orange","Purple"),lty=1,cex=0.8
    )                         })

 
  
  # output$all1<-renderPlot({if(input$all2=="Th"){plot(table(Th$Month.and.Year),type = "line",col="Blue",main="Volume chart for all months",xlab="Months",ylab = "Companies",ylim=c(0,500))}
  #                                       else if (input$all2=="Vo"){plot(table(Vo$Month.and.Year),type = "line",col="Blue",main="Volume chart for all months",xlab="Months",ylab = "Companies",ylim=c(0,500))}
  #                                       else if(input$all2=="Te"){plot(table(Te$Month.and.Year),type = "line",col="Blue",main="Volume chart for all months",xlab="Months",ylab = "Companies",ylim=c(0,500))}
  #                                       else if(input$all2=="Vi"){plot(table(Vi$Month.and.Year),type = "line",col="Blue",main="Volume chart for all months",xlab="Months",ylab = "Companies",ylim=c(0,500))}
  #                                       else if(input$all2=="EE"){plot(table(EE$Month.and.Year),type = "line",col="Blue",main="Volume chart for all months",xlab="Months",ylab = "Companies",ylim=c(0,500))}
  #                                                               else{plot(table(Oo$Month.and.Year),type = "line",col="Blue",main="Volume chart for all months",xlab="Months",ylab = "Companies",ylim=c(0,500))}
  #   
  # })
  # 
  # output$all1<-renderPlot({plot(table(input$'all2$Month.and.Year'),type = "line",col="Blue",main="Volume chart for all months",xlab="Months",ylab = "Companies",ylim=c(0,500))
  # })

  output$histogram1<-renderPlot({ifelse(input$dataset=="Th",hist(Th[,input$inField1[input$inField1>=5]], breaks = input$bins,main = paste("Histogram of emotion scores of Three"),xlab = paste("Emotion Score"),ylab = paste("Frequency")),
                                        ifelse(input$dataset=="Vo",hist(Vo[,input$inField1], breaks = input$bins,main = paste("Histogram of emotion scores of Vodafone"),xlab = paste("Emotion Score"),ylab = paste("Frequency")),
                                               ifelse(input$dataset=="Te",hist(Te[,input$inField1], breaks = input$bins,main = paste("Histogram of emotion scores of Tesco"),xlab = paste("Emotion Score"),ylab = paste("Frequency")),
                                                      ifelse(input$dataset=="Vi",hist(Vi[,input$inField1], breaks = input$bins,main = paste("Histogram of emotion scores of Virgin mobile"),xlab = paste("Emotion Score"),ylab = paste("Frequency")),
                                                             ifelse(input$dataset=="EE",hist(EE[,input$inField1], breaks = input$bins,main = paste("Histogram of emotion scores of EE"),xlab = paste("Emotion Score"),ylab = paste("Frequency")),
                                                                    hist(OO[,input$inField1], breaks = input$bins,main = paste("Histogram of emotion scores of O2"),xlab = paste("Emotion Score"),ylab = paste("Frequency")))))))
    
  })
  
  
  output$histogram2<-renderPlot({ifelse(input$dataset=="Th",hist(Th$sent.val1, breaks = input$bins,main=paste("Histogram of sentiment values of Three"),xlab = paste("Sentiment values"),ylab = paste("Frequency"),ylim = c(0,2000)),
                                        ifelse(input$dataset=="Vo",hist(Vo$sent.val1, breaks = input$bins,main=paste("Histogram of sentiment values of Vodafone"),xlab = paste("Sentiment values"),ylab = paste("Frequency"),ylim = c(0,2000)),
                                               ifelse(input$dataset=="Te",hist(Te$sent.val1, breaks = input$bins,main=paste("Histogram of sentiment values of Tesco"),xlab = paste("Sentiment values"),ylab = paste("Frequency"),ylim = c(0,50)),
                                                      ifelse(input$dataset=="Vi",hist(Vi$sent.val1, breaks = input$bins,main=paste("Histogram of sentiment values of Virgin mobile"),xlab = paste("Sentiment values"),ylab = paste("Frequency"),ylim = c(0,2000)),
                                                             ifelse(input$dataset=="EE",hist(EE$sent.val1, breaks = input$bins,main=paste("Histogram of sentiment values of EE"),xlab = paste("Sentiment values"),ylab = paste("Frequency"),ylim = c(0,2000)),
                                                                    hist(OO$sent.val1, breaks = input$bins,main=paste("Histogram of sentiment values of O2"),xlab = paste("Sentiment values"),ylab = paste("Frequency"),ylim = c(0,2000)))))))


})
 
 output$line1<-renderPlot({if(input$dataset=="Th"){
   plot(Th$sent.val1, type="line",main = "Line Chart of sentiment values of Three",xlab = "Reviews",ylab = "Sentiment values")
   }
                                else if(input$dataset=="Vo"){
                                  plot(Vo$sent.val1, type="line",main = "Line Chart of sentiment values of Vodafone",xlab = "Reviews",ylab = "Sentiment values")
                                }
                                         else if(input$dataset=="Te"){plot(Te$sent.val1, type="line",main = "Line Chart of sentiment values of Tesco",xlab = "Reviews",ylab = "Sentiment values")}
                                                else if(input$dataset=="Vi"){plot(Vi$sent.val1, type="line",main = "Line Chart of sentiment values of Virgin mobile",xlab = "Reviews",ylab = "Sentiment values")}
                                                       else if(input$dataset=="EE"){plot(EE$sent.val1, type="line",main = "Line Chart of sentiment values of EE",xlab = "Reviews",ylab = "Sentiment values")}
                                                              else{plot(OO$sent.val1, type="line",main = "Line Chart of sentiment values of O2",xlab = "Reviews",ylab = "Sentiment values")}
})

 output$bar1<-renderPlot({if(input$dataset=="Th"){
   barplot(table(Th$Month.and.Year),main = "Bar Chart of no. of monthly reviews of Three",xlab = "Months",ylab = "Count")
 }
   else if(input$dataset=="Vo"){
     barplot(table(Vo$Month.and.Year),main = "Bar Chart of no. of monthly reviews of Vodafone",xlab = "Months",ylab = "Count")
   }
   else if(input$dataset=="Te"){barplot(table(Te$Month.and.Year),main = "Bar Chart of no. of monthly reviews of Tesco",xlab = "Months",ylab = "Count")}
   else if(input$dataset=="Vi"){barplot(table(Vi$Month.and.Year),main = "Bar Chart of no. of monthly reviews of Virgin mobile",xlab = "Months",ylab = "Count")}
   else if(input$dataset=="EE"){barplot(table(EE$Month.and.Year),main = "Bar Chart of no. of monthly reviews of EE",xlab = "Months",ylab = "Count")}
   else{barplot(table(OO$Month.and.Year),main = "Bar Chart of no. of monthly reviews of O2",xlab = "Months",ylab = "Count")}})
 
 output$bar2<-renderPlot({if(input$dataset=="Th"){
   barplot(table(Th$category_senti1,sort(Th$Month.and.Year)),main = "Bar Chart of no. of monthly reviews of Three",axisnames=F,xlab = "Months",ylab = "Count",col=c("Red","yellow","Dark Green"),legend=rownames(table(Th$category_senti1,Th$Month.and.Year)))
   axis(1,at=1:19, labels=Th[1:19,37])
 }
   else if(input$dataset=="Vo"){
     barplot(table(Vo$category_senti1,Vo$Month.and.Year),axisnames = F,main = "Bar Chart of no. of monthly reviews of Vodafone",xlab = "Months",ylab = "Count",col=c("Red","yellow","Dark Green"),legend=rownames(table(Th$category_senti1,Th$Month.and.Year)))
     axis(1,at=1:15, labels=Vo[1:15,37])
     }
   else if(input$dataset=="Te"){barplot(table(Te$category_senti1,Te$Month.and.Year),main = "Bar Chart of no. of monthly reviews of Tesco",xlab = "Months",ylab = "Count",axisnames=F,col=c("Red","yellow","Dark Green"),legend=rownames(table(Th$category_senti1,Th$Month.and.Year)))
     axis(1,at=1:14, labels=Te[1:14,37])}
   else if(input$dataset=="Vi"){barplot(table(Vi$category_senti1,Vi$Month.and.Year),main = "Bar Chart of no. of monthly reviews of Virgin mobile",axisnames = F,xlab = "Months",ylab = "Count",col=c("Red","yellow","Dark Green"),legend=rownames(table(Th$category_senti1,Th$Month.and.Year)))
     axis(1,at=1:51, labels=Vi[1:51,37])}
   else if(input$dataset=="EE"){barplot(table(EE$category_senti1,EE$Month.and.Year),main = "Bar Chart of no. of monthly reviews of EE",axisnames = F,xlab = "Months",ylab = "Count",col=c("Red","yellow","Dark Green"),legend=rownames(table(Th$category_senti1,Th$Month.and.Year)))
     axis(1,at=1:37, labels=EE[1:37,37])
     
     }
   else{barplot(table(OO$category_senti1,OO$Month.and.Year),main = "Bar Chart of no. of monthly reviews of O2",axisnames = F,xlab = "Months",ylab = "Count",col=c("Red","yellow","Dark Green"),legend=rownames(table(Th$category_senti1,Th$Month.and.Year)))
     axis(1,at=1:81, labels=OO[1:81,37])
   }})
 
 output$line2<-renderPlot({if(input$dataset=="Th"){plot(Th[,input$inField2], type="line", xlim = c(0,200),xlab = "Reviews",ylab = "Sentiment values",main = "Line Chart of monthly sentiment values of Three")}
     else if(input$dataset=="Vo"){plot(Vo[,input$inField2], type="line", xlim = c(0,180),main = "Line Chart of monthly sentiment values of Vodafone",xlab = "Reviews",ylab = "Sentiment values")}
     else if(input$dataset=="Te"){plot(Te[,input$inField2], type="line", xlim = c(0,10),main = "Line Chart of monthly sentiment values of Tesco",xlab = "Reviews",ylab = "Sentiment values")}
     else if(input$dataset=="Vi"){plot(Vi[,input$inField2], type="line", xlim = c(0,75),main = "Line Chart of monthly sentiment values of Virgin mobile",xlab = "Reviews",ylab = "Sentiment values")}
     else if(input$dataset=="EE"){plot(EE[,input$inField2], type="line", xlim = c(0,110),main = "Line Chart of monthly sentiment values of EE",xlab = "Reviews",ylab = "Sentiment values")}
     else{plot(OO[,input$inField2], type="line", xlim = c(0,75),main = "Line Chart of monthly sentiment values of O2",xlab = "Reviews",ylab = "Sentiment values")}
 })
 
 output$pie1<-renderPlot({if(input$dataset=="Th"){
   pie(x=table(Th$category_senti1),
       density = 50,
       labels = paste(names(table(Th$category_senti1))," ",round(table(Th$category_senti1)/sum(table(Th$category_senti1))*100,2),"%",sep=''),
       col=c("Red","yellow","Dark Green"),
       main = "Pie Chart of sentiment categories of Three")
   
 }
   else if(input$dataset=="Vo"){
     pie(x=table(Vo$category_senti1),
         density = 50,
         labels = paste(names(table(Vo$category_senti1))," ",round(table(Vo$category_senti1)/sum(table(Vo$category_senti1))*100,2),"%",sep=''),
         col=c("Red","yellow","Dark Green"),
         main = "Pie Chart of sentiment categories of Vodafone")
     
   }
   else if(input$dataset=="Te"){pie(x=table(Te$category_senti1),
                                    density = 50,
                                    labels = paste(names(table(Te$category_senti1))," ",round(table(Te$category_senti1)/sum(table(Te$category_senti1))*100,2),"%",sep=''),
                                    col=c("Red","yellow","Dark Green"),
                                    main = "Pie Chart of sentiment categories of Tesco")
   }
   else if(input$dataset=="Vi"){pie(x=table(Vi$category_senti1),
                                    density = 50,
                                    labels = paste(names(table(Vi$category_senti1))," ",round(table(Vi$category_senti1)/sum(table(Vi$category_senti1))*100,2),"%",sep=''),
                                    col=c("Red","yellow","Dark Green"),
                                    main = "Pie Chart of sentiment categories of Virgin mobile")
   }
   else if(input$dataset=="EE"){pie(x=table(EE$category_senti1),
                                    density = 50,
                                    labels = paste(names(table(EE$category_senti1))," ",round(table(EE$category_senti1)/sum(table(EE$category_senti1))*100,2),"%",sep=''),
                                    col=c("Red","yellow","Dark Green"),
                                    main = "Pie Chart of sentiment categories of EE")
   }
   else{pie(x=table(OO$category_senti1),
            density = 50,
            labels = paste(names(table(OO$category_senti1))," ",round(table(OO$category_senti1)/sum(table(OO$category_senti1))*100,2),"%",sep=''),
            col=c("Red","yellow","Dark Green"),
            main = "Pie Chart of sentiment categories of O2")
   }
 })
 
 output$word10<-renderPlot({if(input$emotion=="Positive"){
   p11_th<-subset(nth,cat_sent_th=="Positive")
   p11_th<-as.data.frame(p11_th)
   wordcloud(words=p11_th$`wordcloud_bigram_th.ngrams`,freq=p11_th$`wordcloud_bigram_th.freq`,
             min.freq = 1,
             max.words=500, random.order=FALSE, 
             color="Dark Green",
             scale=c(0.8,0.10))
   
 }
   else{
     b11_th<-subset(nth,cat_sent_th=="Negative")
     b11_th<-as.data.frame(b11_th)
     wordcloud(words=b11_th$`wordcloud_bigram_th.ngrams`,freq=b11_th$`wordcloud_bigram_th.freq`,
               min.freq = 2,
               max.words=200, random.order=FALSE, 
               colors="Red")
     }})
   
   output$word11<-renderPlot({if(input$emotion=="Positive"){
     p11_vo<-subset(nvo,cat_sent_vo=="Positive")
     p11_vo<-as.data.frame(p11_vo)
     wordcloud(words=p11_vo$`wordcloud_bigram_vo.ngrams`,freq=p11_vo$`wordcloud_bigram_vo.freq`,
               min.freq = 2,
               max.words=500, random.order=FALSE, 
               color="Dark Green")
   }
   else{
     b11_vo<-subset(nvo,cat_sent_vo=="Negative")
     b11_vo<-as.data.frame(b11_vo)
     wordcloud(words=b11_vo$`wordcloud_bigram_vo.ngrams`,freq=b11_vo$`wordcloud_bigram_vo.freq`,
               min.freq = 2,
               max.words=500, random.order=FALSE, 
               colors="Red")
     
   }})
   
   
   output$word12<-renderPlot({if(input$emotion=="Positive"){
     p11_oo<-subset(noo,cat_sent_oo=="Positive")
     p11_oo<-as.data.frame(p11_oo)
     wordcloud(words=p11_oo$`wordcloud_bigram_oo.ngrams`,freq=p11_oo$`wordcloud_bigram_oo.freq`,
               min.freq = 2,
               max.words=500, random.order=FALSE,
               color="Dark Green")

   }
   else{
     b11_oo<-subset(noo,cat_sent_oo=="Negative")
     b11_oo<-as.data.frame(b11_oo)
     text("title of my cloud")
     wordcloud(words=b11_oo$`wordcloud_bigram_oo.ngrams`,freq=b11_oo$`wordcloud_bigram_oo.freq`,
               min.freq = 2,
               max.words=500, random.order=FALSE,
               colors="Red")
   }})

   output$word13<-renderPlot({if(input$emotion=="Positive"){
     p11_vi<-subset(nvi,cat_sent_vi=="Positive")
     p11_vi<-as.data.frame(p11_vi)
     wordcloud(words=p11_vi$`wordcloud_bigram_vi.ngrams`,freq=p11_vi$`wordcloud_bigram_vi.freq`,
               min.freq = 2,
               max.words=500, random.order=FALSE, 
               color="Dark Green")
   }  
   else{
     b11_vi<-subset(nvi,cat_sent_vi=="Negative")
     b11_vi<-as.data.frame(b11_vi)
     wordcloud(words=b11_vi$`wordcloud_bigram_vi.ngrams`,freq=b11_vi$`wordcloud_bigram_vi.freq`,
               min.freq = 2,
               max.words=500, random.order=FALSE, 
               colors="Red")
   }})
   
   output$word14<-renderPlot({if(input$emotion=="Positive"){
     p11_ee<-subset(nee,cat_sent_ee=="Positive")
     p11_ee<-as.data.frame(p11_ee)
     wordcloud(words=p11_ee$`wordcloud_bigram_ee.ngrams`,freq=p11_ee$`wordcloud_bigram_ee.freq`,
               min.freq = 2,
               max.words=500, random.order=FALSE, 
               color="Dark Green")
   }
   else{
     b11_ee<-subset(nee,cat_sent_ee=="Negative")
     b11_ee<-as.data.frame(b11_ee)
     wordcloud(words=b11_ee$`wordcloud_bigram_ee.ngrams`,freq=b11_ee$`wordcloud_bigram_ee.freq`,
               min.freq = 2,
               max.words=500, random.order=FALSE, 
               colors="Red")
     
   }})
   output$word15<-renderPlot({if(input$emotion=="Positive"){
     p11_te<-subset(nte,cat_sent_te=="Positive")
     p11_te<-as.data.frame(p11_te)
     wordcloud(words=p11_te$`wordcloud_bigram_te.ngrams`,freq=p11_te$`wordcloud_bigram_te.freq`,
               min.freq = 2,
               max.words=500, random.order=FALSE, 
               color="Dark Green")
   }  
   else{
     b11_te<-subset(nte,cat_sent_te=="Negative")
     
     b11_te<-as.data.frame(b11_te)
     wordcloud(words=b11_te$`wordcloud_bigram_te.ngrams`,freq=b11_te$`wordcloud_bigram_te.freq`,
               min.freq = 2,
               max.words=500, random.order=FALSE, 
               colors="Red")
   }})
   
   output$hist3<-renderPlot({if(input$dataset=="Th" & input$inField1=="anger"){
     hist(
       Th$anger[Th$anger >= 1 & Th$anger<=11],
      # breaks = input$bins,
       main = "Histogram of anger for Three",
       xlab = "Anger",
       ylim = c(0, 1000)
     )
     axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     
   }
     else if(input$dataset=="Th" & input$inField1=="anticipation"){
       hist(
         Th$anticipation[Th$anticipation >= 1&Th$anticipation <= 11],
         breaks = input$bins,
         main = "Histogram of anticipation for Three",
         xlab = "Anticipation",
         ylim = c(0, 1000)
       )
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Th" & input$inField1=="disgust"){
       hist(
         Th$disgust[Th$disgust >= 1 & Th$disgust<=11],
         breaks = input$bins,
         main = "Histogram of disgust for Three",
         xlab = "Disgust",
         ylim = c(0, 800)
       )
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Th" & input$inField1=="fear"){
       hist(
         Th$fear[Th$fear >= 1 & Th$fear<=11],
         breaks = input$bins,
         main = "Histogram of fear for Three",
         xlab = "Fear",
         ylim = c(0, 1000)
       )
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Th" & input$inField1=="joy"){
       hist(
         Th$joy[Th$joy >= 1&Th$joy<=11],
         breaks = input$bins,
         main = "Histogram of joy for Three",
         xlab = "Joy",
         ylim = c(0, 1000)
       )
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Th" & input$inField1=="sadness"){
       hist(Th$sadness[Th$sadness>=1&Th$sadness<=11],breaks = input$bins,main="Histogram of sadness for Three",xlab = "Sadness",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Th" & input$inField1=="surprise"){
       hist(Th$surprise[Th$surprise>=1&Th$surprise<=11],breaks = input$bins,main="Histogram of surprise for Three",xlab = "Surprise",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
       }
     else if(input$dataset=="Th" & input$inField1=="trust"){
       hist(Th$trust[Th$trust>=1&Th$trust<=11],breaks = input$bins,main="Histogram of trust for Three",xlab="Trust",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
       }
     else if(input$dataset=="Th" & input$inField1=="negative"){
       hist(Th$negative[Th$negative>=1&Th$negative<=11],breaks = input$bins,main="Histogram of negative for Three",xlab = "Negative",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
       }
     else if(input$dataset=="Th" & input$inField1=="positive"){
       hist(Th$positive[Th$positive>=1&Th$positive<=11],breaks = input$bins,main="Histogram of positive for Three",xlab="Positive",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     
     else if(input$dataset=="Vi" & input$inField1=="anger"){
       hist(Vi$anger[Vi$anger>=1&Vi$anger<=11],breaks = input$bins,main="Histogram of anger for Virgin mobile",xlab = "Anger",ylim = c(0,500))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
       
     }
     else if(input$dataset=="Vi" & input$inField1=="anticipation"){
       hist(Vi$anticipation[Vi$anticipation>=1&Vi$anticipation<=11],breaks = input$bins,main="Histogram of anticipation for Virgin mobile",xlab="Anticipation",ylim = c(0,500))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vi" & input$inField1=="disgust"){
       hist(Vi$disgust[Vi$disgust>=1&Vi$disgust<=11],breaks = input$bins,main="Histogram of disgust for Virgin mobile",xlab = "Disgust",ylim = c(0,500))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vi" & input$inField1=="fear"){
       hist(Vi$fear[Vi$fear>=1&Vi$fear<=11],breaks = input$bins,main="Histogram of fear for Virgin mobile",xlab = "Fear",ylim = c(0,600))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vi" & input$inField1=="joy"){
       hist(Vi$joy[Vi$joy>=1&Vi$joy<=11],breaks = input$bins,main="Histogram of joy for Virgin mobile",xlab = "Joy",ylim = c(0,600))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vi" & input$inField1=="sadness"){
       hist(Vi$sadness[Vi$sadness>=1&Vi$sadness<=11],breaks = input$bins,main="Histogram of sadness for Virgin mobile",xlab = "Sadness",ylim = c(0,600))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vi" & input$inField1=="surprise"){
       hist(Vi$surprise[Vi$surprise>=1&Vi$surprise<=11],breaks = input$bins,main="Histogram of surprise for Virgin mobile",xlab = "Surprise",ylim = c(0,300))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vi" & input$inField1=="trust"){
       hist(Vi$trust[Vi$trust>=1&Vi$trust<=11],breaks = input$bins,main="Histogram of trust for Virgin mobile",xlab="Trust",ylim = c(0,500))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vi" & input$inField1=="negative"){
       hist(Vi$negative[Vi$negative>=1&Vi$negative<=11],breaks = input$bins,main="Histogram of negative for Virgin mobile",xlab = "Negative",ylim = c(0,500))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vi" & input$inField1=="positive"){
       hist(Vi$positive[Vi$positive>=1&Vi$positive<=11],breaks = input$bins,main="Histogram of positive for Virgin mobile",xlab="Positive",ylim = c(0,600))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vo" & input$inField1=="anger"){
       hist(Vo$anger[Vo$anger>=1&Vo$anger<=11],breaks = input$bins,main="Histogram of anger for Vodafone",xlab = "Anger",ylim = c(0,1200))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
       
     }
     else if(input$dataset=="Vo" & input$inField1=="anticipation"){
       hist(Vo$anticipation[Vo$anticipation>=1&Vo$anticipation<=11],breaks = input$bins,main="Histogram of anticipation for Vodafone",xlab="Anticipation",ylim = c(0,1000))
       axis(1, at = 1:20, labels = seq(1, 20, 1))
     }
     else if(input$dataset=="Vo" & input$inField1=="disgust"){
       hist(Vo$disgust[Vo$disgust>=1&Vo$disgust<=11],breaks = input$bins,main="Histogram of disgust for Vodafone",xlab = "Disgust",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vo" & input$inField1=="fear"){
       hist(Vo$fear[Vo$fear>=1&Vo$fear<=11],breaks = input$bins,main="Histogram of fear for Vodafone",xlab = "Fear",ylim = c(0,1200))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vo" & input$inField1=="joy"){
       hist(Vo$joy[Vo$joy>=1&Vo$joy<=11],breaks = input$bins,main="Histogram of joy for Vodafone",xlab = "Joy",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vo" & input$inField1=="sadness"){
       hist(Vo$sadness[Vo$sadness>=1&Vo$sadness<=11],breaks = input$bins,main="Histogram of sadness for Vodafone",xlab = "Sadness",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vo" & input$inField1=="surprise"){
       hist(Vo$surprise[Vo$surprise>=1&Vo$surprise<=11],breaks = input$bins,main="Histogram of surprise for Vodafone",xlab = "Surprise",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vo" & input$inField1=="trust"){
       hist(Vo$trust[Vo$trust>=1&Vo$trust<=11],breaks = input$bins,main="Histogram of trust for Vodafone",xlab="Trust",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vo" & input$inField1=="negative"){
       hist(Vo$negative[Vo$negative>=1&Vo$negative<=11],breaks = input$bins,main="Histogram of negative for Vodafone",xlab = "Negative",ylim = c(0,1500))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vo" & input$inField1=="positive"){
       hist(Vo$positive[Vo$positive>=1&Vo$positive<=11],breaks = input$bins,main="Histogram of positive for Vodafone",xlab="Positive",ylim = c(0,1500))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Oo" & input$inField1=="anger"){
       hist(OO$anger[OO$anger>=1&OO$anger<=11],breaks = input$bins,main="Histogram of anger for O2",xlab = "Anger",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
       
     }
     else if(input$dataset=="Oo" & input$inField1=="anticipation"){
       hist(OO$anticipation[OO$anticipation>=1&OO$anticipation<=11],breaks = input$bins,main="Histogram of anticipation for O2",xlab="Anticipation",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Oo" & input$inField1=="disgust"){
       hist(OO$disgust[OO$disgust>=1&OO$disgust<=11],breaks = input$bins,main="Histogram of disgust for O2",xlab = "Disgust",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Oo" & input$inField1=="fear"){
       hist(OO$fear[OO$fear>=1&OO$fear<=11],breaks = input$bins,main="Histogram of fear for O2",xlab = "Fear",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Oo" & input$inField1=="joy"){
       hist(OO$joy[OO$joy>=1&OO$joy<=11],breaks = input$bins,main="Histogram of joy for O2",xlab = "Joy",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Oo" & input$inField1=="sadness"){
       hist(OO$sadness[OO$sadness>=1&OO$sadness<=11],breaks = input$bins,main="Histogram of sadness for O2",xlab = "Sadness",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Oo" & input$inField1=="surprise"){
       hist(OO$surprise[OO$surprise>=1&OO$surprise<=11],breaks = input$bins,main="Histogram of surprise for O2",xlab = "Surprise",ylim = c(0,800))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Oo" & input$inField1=="trust"){
       hist(OO$trust[OO$trust>=1&OO$trust<=11],breaks = input$bins,main="Histogram of trust for O2",xlab="Trust",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Oo" & input$inField1=="negative"){
       hist(OO$negative[OO$negative>=1&OO$negative<=11],breaks = input$bins,main="Histogram of negative for O2",xlab = "Negative",ylim = c(0,800))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Oo" & input$inField1=="positive"){
       hist(OO$positive[OO$positive>=1&OO$positive<=11],breaks = input$bins,main="Histogram of positive for O2",xlab="Positive",ylim = c(0,1500))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Te" & input$inField1=="anger"){
       hist(Te$anger[Te$anger>=1&Te$anger<=11],breaks = input$bins,main="Histogram of anger for Tesco mobile",xlab = "Anger",ylim = c(0,25))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
       
     }
     else if(input$dataset=="Te" & input$inField1=="anticipation"){
       hist(Te$anticipation[Te$anticipation>=1&Te$anticipation<=11],breaks = input$bins,main="Histogram of anticipation for Tesco mobile",xlab="Anticipation",ylim = c(0,50))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
       }
     else if(input$dataset=="Te" & input$inField1=="disgust"){
       hist(Te$disgust[Te$disgust>=1&Te$disgust<=11],breaks = input$bins,main="Histogram of disgust for Tesco mobile",xlab = "Disgust",ylim = c(0,20))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
       }
     else if(input$dataset=="Te" & input$inField1=="fear"){
       hist(Te$fear[Te$fear>=1&Te$fear<=11],breaks = input$bins,main="Histogram of fear for Tesco mobile",xlab = "Fear",ylim = c(0,20))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Te" & input$inField1=="joy"){
       hist(Te$joy[Te$joy>=1&Te$joy<=11],breaks = input$bins,main="Histogram of joy for Tesco mobile",xlab = "Joy",ylim = c(0,20))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Te" & input$inField1=="sadness"){
       hist(Te$sadness[Te$sadness>=1&Te$sadness<=11],breaks = input$bins,main="Histogram of sadness for Tesco mobile",xlab = "Sadness",ylim = c(0,30))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Te" & input$inField1=="surprise"){
       hist(Te$surprise[Te$surprise>=1&Te$surprise<=11],main="Histogram of surprise for Tesco mobile",xlab = "Surprise",ylim = c(0,20))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Te" & input$inField1=="trust"){
       hist(Te$trust[Te$trust>=1&Te$trust<=11],breaks = input$bins,main="Histogram of trust for Tesco mobile",xlab="Trust",ylim = c(0,30))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Te" & input$inField1=="negative"){
       hist(Te$negative[Te$negative>=1&Te$negative<=11],breaks = input$bins,main="Histogram of negative for Tesco mobile",xlab = "Negative",ylim = c(0,30))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Te" & input$inField1=="positive"){
       hist(Te$positive[Te$positive>=1&Te$positive<=11],breaks = input$bins,main="Histogram of positive for Tesco mobile",xlab="Positive",ylim = c(0,30))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="EE" & input$inField1=="anger"){
       hist(EE$anger[EE$anger>=1&EE$anger<=11],breaks = input$bins,main="Histogram of anger for EE",xlab = "Anger",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
       
     }
     else if(input$dataset=="EE" & input$inField1=="anticipation"){
       hist(EE$anticipation[EE$anticipation>=1&EE$anticipation<=11],breaks = input$bins,main="Histogram of anticipation for EE",xlab="Anticipation",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="EE" & input$inField1=="disgust"){
       hist(EE$disgust[EE$disgust>=1&EE$disgust<=11],breaks = input$bins,main="Histogram of disgust for EE",xlab = "Disgust",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="EE" & input$inField1=="fear"){
       hist(EE$fear[EE$fear>=1&EE$fear<=11],breaks = input$bins,main="Histogram of fear for EE",xlab = "Fear",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="EE" & input$inField1=="joy"){
       hist(EE$joy[EE$joy>=1&EE$joy<=11],breaks = input$bins,main="Histogram of joy for EE",xlab = "Joy",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="EE" & input$inField1=="sadness"){
       hist(EE$sadness[EE$sadness>=1&EE$sadness<=11],breaks = input$bins,main="Histogram of sadness for EE",xlab = "Sadness",ylim = c(0,1200))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="EE" & input$inField1=="surprise"){
       hist(EE$surprise[EE$surprise>=1&EE$surprise<=11],breaks = input$bins,main="Histogram of surprise for EE",xlab = "Surprise",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="EE" & input$inField1=="trust"){
       hist(EE$trust[EE$trust>=1&EE$trust<=11],breaks = input$bins,main="Histogram of trust for EE",xlab="Trust",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="EE" & input$inField1=="negative"){
       hist(EE$negative[EE$negative>=1&EE$negative<=11],breaks = input$bins,main="Histogram of negative for EE",xlab = "Negative",ylim = c(0,1500))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else{
       hist(EE$positive[EE$positive>=1&EE$positive<=11],breaks = input$bins,main="Histogram of positive for EE",xlab="Positive",ylim = c(0,1500))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     
     
   })
   })
  
   
   


 
 



