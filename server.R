shinyServer(function(input,output,session) {
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Three" = Three,
           "Vodafone" = Vodafone,
           "Tesco"=Tesco,
           "Virgin"=Virgin,
           "EE"=EE,
           "O2"=O2)
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
    
    if(input$dataset=="Three"){
      infoBox(title = "No. of reviews", 
              value = nrow(Three)
      ) }
    else if (input$dataset=="Vodafone"){
      infoBox(title = "No. of reviews", 
              value = nrow(Vodafone)
            
      )}
    else if (input$dataset=="Tesco"){
      infoBox(title = "No. of reviews", 
              value = nrow(Tesco)
      )}
    else if (input$dataset=="Virgin"){
      infoBox(title = "No. of reviews", 
              value = nrow(Virgin)
      )}
    else if (input$dataset=="EE"){
      infoBox(title = "No. of reviews", 
              value = nrow(EE)
      )}
    else {infoBox(title = "No. of reviews", 
                  value = nrow(O2))}
    
  })

    output$Positive <- renderInfoBox({
    
    if(input$dataset=="Three"){
      infoBox(title = "", 
              value = as.data.frame(table(Three$category_senti1))[3,],
              icon("arrow-up"),
              subtitle = "",
              color = "green"
      ) }
    else if (input$dataset=="Vodafone"){
      infoBox(title = "", 
              value = as.data.frame(table(Vodafone$category_senti1))[3,],
              icon("arrow-up"),
              subtitle = "",
              color = "green"
      )}
    else if (input$dataset=="Tesco"){
      infoBox(title = "", 
              value = as.data.frame(table(Tesco$category_senti1))[3,],
              icon("arrow-up"),
              subtitle = "",
              color = "green"
      )}
    else if (input$dataset=="Virgin"){
      infoBox(title = "", 
              value = as.data.frame(table(Virgin$category_senti1))[3,],
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
                  value = as.data.frame(table(O2$category_senti1))[3,],
                  icon("arrow-up"),
                  subtitle = "",
                  color = "green")}
    
  })
  output$Neutral <- renderInfoBox({if(input$dataset=="Three"){
    infoBox(title = "", 
            value = as.data.frame(table(Three$category_senti1))[2,],
            icon("arrows-h"),
            subtitle = "",
            color = "yellow"
    ) }
    else if (input$dataset=="Vodafone"){
      infoBox(title = "", 
              value = as.data.frame(table(Vodafone$category_senti1))[2,],
              icon("arrows-h"),
              subtitle = "",
              color = "yellow"
      )}
    else if (input$dataset=="Tesco"){
      infoBox(title = "", 
              value = as.data.frame(table(Tesco$category_senti1))[2,],
              icon("arrows-h"),
              subtitle = "",
              color = "yellow"
      )}
    else if (input$dataset=="Virgin"){
      infoBox(title = "", 
              value = as.data.frame(table(Virgin$category_senti1))[2,],
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
                  value = as.data.frame(table(O2$category_senti1))[2,],
                  icon("arrows-h"),
                  subtitle = "",
                  color = "yellow")}
    
  })
  
  
  output$Negative <- renderInfoBox({
    if(input$dataset=="Three"){
      infoBox(title = "", 
              value = as.data.frame(table(Three$category_senti1))[1,],
              icon("arrow-down"),
              subtitle = "",
              color = "red"
      ) }
    else if (input$dataset=="Vodafone"){
      infoBox(title = "", 
              value = as.data.frame(table(Vodafone$category_senti1))[1,],
              icon("arrow-down"),
              subtitle = "",
              color = "red"
      )}
    else if (input$dataset=="Tesco"){
      infoBox(title = "", 
              value = as.data.frame(table(Tesco$category_senti1))[1,],
              icon("arrow-down"),
              subtitle = "",
              color = "red"
      )}
    else if (input$dataset=="Virgin"){
      infoBox(title = "", 
              value = as.data.frame(table(Virgin$category_senti1))[1,],
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
                  value = as.data.frame(table(O2$category_senti1))[1,],
                  icon("arrow-down"),
                  subtitle = "",
                  color = "red")}
    
  })
  
  
  output$all<-renderPlot({plot(table(Three$Month.and.Year),type = "line",col="Blue",xaxt="n",main="Volume chart for all months",xlab="Months",ylab = "Frequency",ylim=c(0,500))
    lines(table(Tesco$Month.and.Year),type="line",col="Dark Green")
    lines(table(Virgin$Month.and.Year),type="line",col="Maroon")
    lines(table(Vodafone$Month.and.Year),type="line",col="Red")
    lines(table(EE$Month.and.Year),type = "line",col="Orange")
    lines(table(O2$Month.and.Year),type="line",col="Purple")
    axis(1,at=1:19, labels=Three[1:19,37])
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

  output$histogram1<-renderPlot({ifelse(input$dataset=="Three",hist(Three[,input$inField1[input$inField1>=5]], breaks = input$bins,main = paste("Histogram of emotion scores of Three"),xlab = paste("Emotion Score"),ylab = paste("Frequency")),
                                        ifelse(input$dataset=="Vodafone",hist(Vodafone[,input$inField1], breaks = input$bins,main = paste("Histogram of emotion scores of Vodafone"),xlab = paste("Emotion Score"),ylab = paste("Frequency")),
                                               ifelse(input$dataset=="Tesco",hist(Tesco[,input$inField1], breaks = input$bins,main = paste("Histogram of emotion scores of Tesco"),xlab = paste("Emotion Score"),ylab = paste("Frequency")),
                                                      ifelse(input$dataset=="Virgin",hist(Virgin[,input$inField1], breaks = input$bins,main = paste("Histogram of emotion scores of Virgin mobile"),xlab = paste("Emotion Score"),ylab = paste("Frequency")),
                                                             ifelse(input$dataset=="EE",hist(EE[,input$inField1], breaks = input$bins,main = paste("Histogram of emotion scores of EE"),xlab = paste("Emotion Score"),ylab = paste("Frequency")),
                                                                    hist(O2[,input$inField1], breaks = input$bins,main = paste("Histogram of emotion scores of O2"),xlab = paste("Emotion Score"),ylab = paste("Frequency")))))))
    
  })
  
  
  output$histogram2<-renderPlot({ifelse(input$dataset=="Three",hist(Three$sent.val1, breaks = input$bins,main=paste("Histogram of sentiment values of Three"),xlab = paste("Sentiment values"),ylab = paste("Frequency"),ylim = c(0,2000)),
                                        ifelse(input$dataset=="Vodafone",hist(Vodafone$sent.val1, breaks = input$bins,main=paste("Histogram of sentiment values of Vodafone"),xlab = paste("Sentiment values"),ylab = paste("Frequency"),ylim = c(0,2000)),
                                               ifelse(input$dataset=="Tesco",hist(Tesco$sent.val1, breaks = input$bins,main=paste("Histogram of sentiment values of Tesco"),xlab = paste("Sentiment values"),ylab = paste("Frequency"),ylim = c(0,50)),
                                                      ifelse(input$dataset=="Virgin",hist(Virgin$sent.val1, breaks = input$bins,main=paste("Histogram of sentiment values of Virgin mobile"),xlab = paste("Sentiment values"),ylab = paste("Frequency"),ylim = c(0,2000)),
                                                             ifelse(input$dataset=="EE",hist(EE$sent.val1, breaks = input$bins,main=paste("Histogram of sentiment values of EE"),xlab = paste("Sentiment values"),ylab = paste("Frequency"),ylim = c(0,2000)),
                                                                    hist(O2$sent.val1, breaks = input$bins,main=paste("Histogram of sentiment values of O2"),xlab = paste("Sentiment values"),ylab = paste("Frequency"),ylim = c(0,2000)))))))


})
 
 output$line1<-renderPlot({if(input$dataset=="Three"){
   plot(Three$sent.val1, type="line",main = "Line Chart of sentiment values of Three",xlab = "Reviews",ylab = "Sentiment values")
   }
                                else if(input$dataset=="Vodafone"){
                                  plot(Vodafone$sent.val1, type="line",main = "Line Chart of sentiment values of Vodafone",xlab = "Reviews",ylab = "Sentiment values")
                                }
                                         else if(input$dataset=="Tesco"){plot(Tesco$sent.val1, type="line",main = "Line Chart of sentiment values of Tesco",xlab = "Reviews",ylab = "Sentiment values")}
                                                else if(input$dataset=="Virgin"){plot(Virgin$sent.val1, type="line",main = "Line Chart of sentiment values of Virgin mobile",xlab = "Reviews",ylab = "Sentiment values")}
                                                       else if(input$dataset=="EE"){plot(EE$sent.val1, type="line",main = "Line Chart of sentiment values of EE",xlab = "Reviews",ylab = "Sentiment values")}
                                                              else{plot(O2$sent.val1, type="line",main = "Line Chart of sentiment values of O2",xlab = "Reviews",ylab = "Sentiment values")}
})

 output$bar1<-renderPlot({if(input$dataset=="Three"){
   barplot(table(Three$Month.and.Year),main = "Bar Chart of no. of monthly reviews of Three",xlab = "Months",ylab = "Count")
 }
   else if(input$dataset=="Vodafone"){
     barplot(table(Vodafone$Month.and.Year),main = "Bar Chart of no. of monthly reviews of Vodafone",xlab = "Months",ylab = "Count")
   }
   else if(input$dataset=="Tesco"){barplot(table(Tesco$Month.and.Year),main = "Bar Chart of no. of monthly reviews of Tesco",xlab = "Months",ylab = "Count")}
   else if(input$dataset=="Virgin"){barplot(table(Virgin$Month.and.Year),main = "Bar Chart of no. of monthly reviews of Virgin mobile",xlab = "Months",ylab = "Count")}
   else if(input$dataset=="EE"){barplot(table(EE$Month.and.Year),main = "Bar Chart of no. of monthly reviews of EE",xlab = "Months",ylab = "Count")}
   else{barplot(table(O2$Month.and.Year),main = "Bar Chart of no. of monthly reviews of O2",xlab = "Months",ylab = "Count")}})
 
 output$bar2<-renderPlot({if(input$dataset=="Three"){
   barplot(table(Three$category_senti1,sort(Three$Month.and.Year)),main = "Bar Chart of no. of monthly reviews of Three",axisnames=F,ylab = "Count",ylim = c(0,200),col=c("Red","yellow","Dark Green"),legend=rownames(table(Three$category_senti1,Three$Month.and.Year)))
   axis(1,at=1:19, labels=Three[1:19,37],las=2)
 }
   else if(input$dataset=="Vodafone"){
     barplot(table(Vodafone$category_senti1,Vodafone$Month.and.Year),axisnames = F,main = "Bar Chart of no. of monthly reviews of Vodafone",ylab = "Count",ylim = c(0,200),col=c("Red","yellow","Dark Green"),legend=rownames(table(Three$category_senti1,Three$Month.and.Year)))
     axis(1,at=1:15, labels=Vodafone[1:15,37],las=2)
     }
   else if(input$dataset=="Tesco"){barplot(table(Tesco$category_senti1,Tesco$Month.and.Year),main = "Bar Chart of no. of monthly reviews of Tesco",ylim = c(0,10),ylab = "Count",axisnames=F,col=c("Red","yellow","Dark Green"),legend=rownames(table(Three$category_senti1,Three$Month.and.Year)))
     axis(1,at=1:14, labels=Tesco[1:14,37],las=2)}
   else if(input$dataset=="Virgin"){barplot(table(Virgin$category_senti1,Virgin$Month.and.Year),main = "Bar Chart of no. of monthly reviews of Virgin mobile",ylim = c(0,100),axisnames = F,ylab = "Count",col=c("Red","yellow","Dark Green"),legend=rownames(table(Three$category_senti1,Three$Month.and.Year)))
     axis(1,at=1:51, labels=Virgin[1:51,37],las=2)}
   else if(input$dataset=="EE"){barplot(table(EE$category_senti1,EE$Month.and.Year),main = "Bar Chart of no. of monthly reviews of EE",axisnames = F,ylim = c(0,100),ylab = "Count",col=c("Red","yellow","Dark Green"),legend=rownames(table(Three$category_senti1,Three$Month.and.Year)))
     axis(1,at=1:37, labels=EE[1:37,37],las=2)
     
     }
   else{barplot(table(O2$category_senti1,O2$Month.and.Year),main = "Bar Chart of no. of monthly reviews of O2",axisnames = F,xlab = "Months",ylab = "Count",col=c("Red","yellow","Dark Green"),legend=rownames(table(Three$category_senti1,Three$Month.and.Year)))
     axis(1,at=1:81, labels=O2[1:81,37])
   }})
 
 output$line2<-renderPlot({if(input$dataset=="Three"){plot(Three[,input$inField2], type="line", xlim = c(0,200),xlab = "Reviews",ylab = "Sentiment values",main = "Line Chart of monthly sentiment values of Three")}
     else if(input$dataset=="Vodafone"){plot(Vodafone[,input$inField2], type="line", xlim = c(0,180),main = "Line Chart of monthly sentiment values of Vodafone",xlab = "Reviews",ylab = "Sentiment values")}
     else if(input$dataset=="Tesco"){plot(Tesco[,input$inField2], type="line", xlim = c(0,10),main = "Line Chart of monthly sentiment values of Tesco",xlab = "Reviews",ylab = "Sentiment values")}
     else if(input$dataset=="Virgin"){plot(Virgin[,input$inField2], type="line", xlim = c(0,75),main = "Line Chart of monthly sentiment values of Virgin mobile",xlab = "Reviews",ylab = "Sentiment values")}
     else if(input$dataset=="EE"){plot(EE[,input$inField2], type="line", xlim = c(0,110),main = "Line Chart of monthly sentiment values of EE",xlab = "Reviews",ylab = "Sentiment values")}
     else{plot(O2[,input$inField2], type="line", xlim = c(0,75),main = "Line Chart of monthly sentiment values of O2",xlab = "Reviews",ylab = "Sentiment values")}
 })
 
 output$pie1<-renderPlot({if(input$dataset=="Three"){
   pie(x=table(Three$category_senti1),
       labels = paste(names(table(Three$category_senti1))," ",round(table(Three$category_senti1)/sum(table(Three$category_senti1))*100,2),"%",sep=''),
       col=c("Red","yellow","Dark Green"),
       main = "Pie Chart of sentiment categories of Three")
   
 }
   else if(input$dataset=="Vodafone"){
     pie(x=table(Vodafone$category_senti1),
         labels = paste(names(table(Vodafone$category_senti1))," ",round(table(Vodafone$category_senti1)/sum(table(Vodafone$category_senti1))*100,2),"%",sep=''),
         col=c("Red","yellow","Dark Green"),
         main = "Pie Chart of sentiment categories of Vodafone")
     
   }
   else if(input$dataset=="Tesco"){pie(x=table(Tesco$category_senti1),
                                    labels = paste(names(table(Tesco$category_senti1))," ",round(table(Tesco$category_senti1)/sum(table(Tesco$category_senti1))*100,2),"%",sep=''),
                                    col=c("Red","yellow","Dark Green"),
                                    main = "Pie Chart of sentiment categories of Tesco")
   }
   else if(input$dataset=="Virgin"){pie(x=table(Virgin$category_senti1),
                                    labels = paste(names(table(Virgin$category_senti1))," ",round(table(Virgin$category_senti1)/sum(table(Virgin$category_senti1))*100,2),"%",sep=''),
                                    col=c("Red","yellow","Dark Green"),
                                    main = "Pie Chart of sentiment categories of Virgin mobile")
   }
   else if(input$dataset=="EE"){pie(x=table(EE$category_senti1),
                                    labels = paste(names(table(EE$category_senti1))," ",round(table(EE$category_senti1)/sum(table(EE$category_senti1))*100,2),"%",sep=''),
                                    col=c("Red","yellow","Dark Green"),
                                    main = "Pie Chart of sentiment categories of EE")
   }
   else{pie(x=table(O2$category_senti1),
            labels = paste(names(table(O2$category_senti1))," ",round(table(O2$category_senti1)/sum(table(O2$category_senti1))*100,2),"%",sep=''),
            col=c("Red","yellow","Dark Green"),
            main = "Pie Chart of sentiment categories of O2")
   }
 })
 
 output$word10<-renderPlot({if(input$emotion=="Positive"){
   p11_th<-subset(nth,cat_sent_th=="Positive")
   p11_th<-as.data.frame(p11_th)
   wordcloud(words=p11_th$`wordcloud_bigram_th.ngrams`,freq=p11_th$`wordcloud_bigram_th.freq`,
             min.freq = 2,
             max.words=500, random.order=FALSE, 
             color="Dark Green")
   
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
   
   output$hist3<-renderPlot({if(input$dataset=="Three" & input$inField1=="anger"){
     hist(
       Three$anger[Three$anger >= 1 & Three$anger<=11],
      breaks = input$bins,
       main = "Histogram of anger for Three",
       xlab = "Anger",
       ylim = c(0, 1000)
     )
     axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     
   }
     else if(input$dataset=="Three" & input$inField1=="anticipation"){
       hist(
         Three$anticipation[Three$anticipation >= 1&Three$anticipation <= 11],
         breaks = input$bins,
         main = "Histogram of anticipation for Three",
         xlab = "Anticipation",
         ylim = c(0, 1000)
       )
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Three" & input$inField1=="disgust"){
       hist(
         Three$disgust[Three$disgust >= 1 & Three$disgust<=11],
         breaks = input$bins,
         main = "Histogram of disgust for Three",
         xlab = "Disgust",
         ylim = c(0, 800)
       )
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Three" & input$inField1=="fear"){
       hist(
         Three$fear[Three$fear >= 1 & Three$fear<=11],
         breaks = input$bins,
         main = "Histogram of fear for Three",
         xlab = "Fear",
         ylim = c(0, 1000)
       )
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Three" & input$inField1=="joy"){
       hist(
         Three$joy[Three$joy >= 1&Three$joy<=11],
         breaks = input$bins,
         main = "Histogram of joy for Three",
         xlab = "Joy",
         ylim = c(0, 1000)
       )
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Three" & input$inField1=="sadness"){
       hist(Three$sadness[Three$sadness>=1&Three$sadness<=11],breaks = input$bins,main="Histogram of sadness for Three",xlab = "Sadness",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Three" & input$inField1=="surprise"){
       hist(Three$surprise[Three$surprise>=1&Three$surprise<=11],breaks = input$bins,main="Histogram of surprise for Three",xlab = "Surprise",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
       }
     else if(input$dataset=="Three" & input$inField1=="trust"){
       hist(Three$trust[Three$trust>=1&Three$trust<=11],breaks = input$bins,main="Histogram of trust for Three",xlab="Trust",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
       }
     else if(input$dataset=="Three" & input$inField1=="negative"){
       hist(Three$negative[Three$negative>=1&Three$negative<=11],breaks = input$bins,main="Histogram of negative for Three",xlab = "Negative",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
       }
     else if(input$dataset=="Three" & input$inField1=="positive"){
       hist(Three$positive[Three$positive>=1&Three$positive<=11],breaks = input$bins,main="Histogram of positive for Three",xlab="Positive",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     
     else if(input$dataset=="Virgin" & input$inField1=="anger"){
       hist(Virgin$anger[Virgin$anger>=1&Virgin$anger<=11],breaks = input$bins,main="Histogram of anger for Virgin mobile",xlab = "Anger",ylim = c(0,500))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
       
     }
     else if(input$dataset=="Virgin" & input$inField1=="anticipation"){
       hist(Virgin$anticipation[Virgin$anticipation>=1&Virgin$anticipation<=11],breaks = input$bins,main="Histogram of anticipation for Virgin mobile",xlab="Anticipation",ylim = c(0,500))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Virgin" & input$inField1=="disgust"){
       hist(Virgin$disgust[Virgin$disgust>=1&Virgin$disgust<=11],breaks = input$bins,main="Histogram of disgust for Virgin mobile",xlab = "Disgust",ylim = c(0,500))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Virgin" & input$inField1=="fear"){
       hist(Virgin$fear[Virgin$fear>=1&Virgin$fear<=11],breaks = input$bins,main="Histogram of fear for Virgin mobile",xlab = "Fear",ylim = c(0,600))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Virgin" & input$inField1=="joy"){
       hist(Virgin$joy[Virgin$joy>=1&Virgin$joy<=11],breaks = input$bins,main="Histogram of joy for Virgin mobile",xlab = "Joy",ylim = c(0,600))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Virgin" & input$inField1=="sadness"){
       hist(Virgin$sadness[Virgin$sadness>=1&Virgin$sadness<=11],breaks = input$bins,main="Histogram of sadness for Virgin mobile",xlab = "Sadness",ylim = c(0,600))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Virgin" & input$inField1=="surprise"){
       hist(Virgin$surprise[Virgin$surprise>=1&Virgin$surprise<=11],breaks = input$bins,main="Histogram of surprise for Virgin mobile",xlab = "Surprise",ylim = c(0,300))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Virgin" & input$inField1=="trust"){
       hist(Virgin$trust[Virgin$trust>=1&Virgin$trust<=11],breaks = input$bins,main="Histogram of trust for Virgin mobile",xlab="Trust",ylim = c(0,500))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Virgin" & input$inField1=="negative"){
       hist(Virgin$negative[Virgin$negative>=1&Virgin$negative<=11],breaks = input$bins,main="Histogram of negative for Virgin mobile",xlab = "Negative",ylim = c(0,500))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Virgin" & input$inField1=="positive"){
       hist(Virgin$positive[Virgin$positive>=1&Virgin$positive<=11],breaks = input$bins,main="Histogram of positive for Virgin mobile",xlab="Positive",ylim = c(0,600))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vodafone" & input$inField1=="anger"){
       hist(Vodafone$anger[Vodafone$anger>=1&Vodafone$anger<=11],breaks = input$bins,main="Histogram of anger for Vodafone",xlab = "Anger",ylim = c(0,1200))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
       
     }
     else if(input$dataset=="Vodafone" & input$inField1=="anticipation"){
       hist(Vodafone$anticipation[Vodafone$anticipation>=1&Vodafone$anticipation<=11],breaks = input$bins,main="Histogram of anticipation for Vodafone",xlab="Anticipation",ylim = c(0,1000))
       axis(1, at = 1:20, labels = seq(1, 20, 1))
     }
     else if(input$dataset=="Vodafone" & input$inField1=="disgust"){
       hist(Vodafone$disgust[Vodafone$disgust>=1&Vodafone$disgust<=11],breaks = input$bins,main="Histogram of disgust for Vodafone",xlab = "Disgust",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vodafone" & input$inField1=="fear"){
       hist(Vodafone$fear[Vodafone$fear>=1&Vodafone$fear<=11],breaks = input$bins,main="Histogram of fear for Vodafone",xlab = "Fear",ylim = c(0,1200))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vodafone" & input$inField1=="joy"){
       hist(Vodafone$joy[Vodafone$joy>=1&Vodafone$joy<=11],breaks = input$bins,main="Histogram of joy for Vodafone",xlab = "Joy",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vodafone" & input$inField1=="sadness"){
       hist(Vodafone$sadness[Vodafone$sadness>=1&Vodafone$sadness<=11],breaks = input$bins,main="Histogram of sadness for Vodafone",xlab = "Sadness",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vodafone" & input$inField1=="surprise"){
       hist(Vodafone$surprise[Vodafone$surprise>=1&Vodafone$surprise<=11],breaks = input$bins,main="Histogram of surprise for Vodafone",xlab = "Surprise",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vodafone" & input$inField1=="trust"){
       hist(Vodafone$trust[Vodafone$trust>=1&Vodafone$trust<=11],breaks = input$bins,main="Histogram of trust for Vodafone",xlab="Trust",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vodafone" & input$inField1=="negative"){
       hist(Vodafone$negative[Vodafone$negative>=1&Vodafone$negative<=11],breaks = input$bins,main="Histogram of negative for Vodafone",xlab = "Negative",ylim = c(0,1500))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Vodafone" & input$inField1=="positive"){
       hist(Vodafone$positive[Vodafone$positive>=1&Vodafone$positive<=11],breaks = input$bins,main="Histogram of positive for Vodafone",xlab="Positive",ylim = c(0,1500))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="O2" & input$inField1=="anger"){
       hist(O2$anger[O2$anger>=1&O2$anger<=11],breaks = input$bins,main="Histogram of anger for O2",xlab = "Anger",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
       
     }
     else if(input$dataset=="O2" & input$inField1=="anticipation"){
       hist(O2$anticipation[O2$anticipation>=1&O2$anticipation<=11],breaks = input$bins,main="Histogram of anticipation for O2",xlab="Anticipation",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="O2" & input$inField1=="disgust"){
       hist(O2$disgust[O2$disgust>=1&O2$disgust<=11],breaks = input$bins,main="Histogram of disgust for O2",xlab = "Disgust",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="O2" & input$inField1=="fear"){
       hist(O2$fear[O2$fear>=1&O2$fear<=11],breaks = input$bins,main="Histogram of fear for O2",xlab = "Fear",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="O2" & input$inField1=="joy"){
       hist(O2$joy[O2$joy>=1&O2$joy<=11],breaks = input$bins,main="Histogram of joy for O2",xlab = "Joy",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="O2" & input$inField1=="sadness"){
       hist(O2$sadness[O2$sadness>=1&O2$sadness<=11],breaks = input$bins,main="Histogram of sadness for O2",xlab = "Sadness",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="O2" & input$inField1=="surprise"){
       hist(O2$surprise[O2$surprise>=1&O2$surprise<=11],breaks = input$bins,main="Histogram of surprise for O2",xlab = "Surprise",ylim = c(0,800))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="O2" & input$inField1=="trust"){
       hist(O2$trust[O2$trust>=1&O2$trust<=11],breaks = input$bins,main="Histogram of trust for O2",xlab="Trust",ylim = c(0,1000))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="O2" & input$inField1=="negative"){
       hist(O2$negative[O2$negative>=1&O2$negative<=11],breaks = input$bins,main="Histogram of negative for O2",xlab = "Negative",ylim = c(0,800))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="O2" & input$inField1=="positive"){
       hist(O2$positive[O2$positive>=1&O2$positive<=11],breaks = input$bins,main="Histogram of positive for O2",xlab="Positive",ylim = c(0,1500))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Tesco" & input$inField1=="anger"){
       hist(Tesco$anger[Tesco$anger>=1&Tesco$anger<=11],breaks = input$bins,main="Histogram of anger for Tesco mobile",xlab = "Anger",ylim = c(0,25))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
       
     }
     else if(input$dataset=="Tesco" & input$inField1=="anticipation"){
       hist(Tesco$anticipation[Tesco$anticipation>=1&Tesco$anticipation<=11],breaks = input$bins,main="Histogram of anticipation for Tesco mobile",xlab="Anticipation",ylim = c(0,50))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
       }
     else if(input$dataset=="Tesco" & input$inField1=="disgust"){
       hist(Tesco$disgust[Tesco$disgust>=1&Tesco$disgust<=11],breaks = input$bins,main="Histogram of disgust for Tesco mobile",xlab = "Disgust",ylim = c(0,20))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
       }
     else if(input$dataset=="Tesco" & input$inField1=="fear"){
       hist(Tesco$fear[Tesco$fear>=1&Tesco$fear<=11],breaks = input$bins,main="Histogram of fear for Tesco mobile",xlab = "Fear",ylim = c(0,20))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Tesco" & input$inField1=="joy"){
       hist(Tesco$joy[Tesco$joy>=1&Tesco$joy<=11],breaks = input$bins,main="Histogram of joy for Tesco mobile",xlab = "Joy",ylim = c(0,20))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Tesco" & input$inField1=="sadness"){
       hist(Tesco$sadness[Tesco$sadness>=1&Tesco$sadness<=11],breaks = input$bins,main="Histogram of sadness for Tesco mobile",xlab = "Sadness",ylim = c(0,30))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Tesco" & input$inField1=="surprise"){
       hist(Tesco$surprise[Tesco$surprise>=1&Tesco$surprise<=11],main="Histogram of surprise for Tesco mobile",xlab = "Surprise",ylim = c(0,20))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Tesco" & input$inField1=="trust"){
       hist(Tesco$trust[Tesco$trust>=1&Tesco$trust<=11],breaks = input$bins,main="Histogram of trust for Tesco mobile",xlab="Trust",ylim = c(0,30))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Tesco" & input$inField1=="negative"){
       hist(Tesco$negative[Tesco$negative>=1&Tesco$negative<=11],breaks = input$bins,main="Histogram of negative for Tesco mobile",xlab = "Negative",ylim = c(0,30))
       axis(1, at = 1:11, labels = c(seq(1,10, 1),"11+"))
     }
     else if(input$dataset=="Tesco" & input$inField1=="positive"){
       hist(Tesco$positive[Tesco$positive>=1&Tesco$positive<=11],breaks = input$bins,main="Histogram of positive for Tesco mobile",xlab="Positive",ylim = c(0,30))
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
  
   
   


 
 



