
shinyServer(function(input, output) {
  output$gendermap <- renderLeaflet({
    if (input$gendermapid == "Male") {
      return (leaflet_Male)
    } else if (input$gendermapid == "Female") {
      return (leaflet_Female)
    } else if (input$gendermapid == "Both") {
      return (leaflet_Both)
    }
  })
  output$genderChart <- renderPlotly({
    g6= ggplot(data= genderDF, aes(x=sex, y=count/sum(count))) + geom_bar(stat= "identity", aes(fill= sex), position = "dodge") +
      theme(legend.position = "none", plot.margin = margin(10,10,10,10)) + xlab("Gender") + ylab("Percentage of Total Users")
    ggplotly(g6)  %>% layout(height  = 300)
  })
  output$Top10Gender <- renderDataTable(
    if (input$gendermapid == "Male") {
      return (neighborhoodTop10_Male)
    } else if (input$gendermapid == "Female") {
      return (neighborhoodTop10_Female)
    } else if (input$gendermapid == "Both") {
      return (neighborhoodTop10_Both)
    }
  )
  output$ageRangeChart <- renderPlotly({
    g7= ggplot(data= ageRangeDF, aes(x=ageRange, y=count/sum(count))) + geom_bar(stat="identity", aes(fill= ageRange)) +
      theme(legend.position = "none", plot.margin = margin(10,10,10,10))+
      theme(axis.text.x =element_text(angle=45), legend.position = "none", plot.margin = margin(10,10,40,10))+ xlab("Age Range") + ylab("Percentage of Total Users")
    ggplotly(g7)  %>% layout(height  = 300)
  })
  output$Top10Age <- renderDataTable(
    if (input$agerangeid == "All") {
      return (neighborhoodTop10_All)
    } else if (input$agerangeid == "13 to 18") {
      return (neighborhoodTop10_13)
    } else if (input$agerangeid == "19 to 24") {
      return (neighborhoodTop10_19)
    } else if (input$agerangeid == "25 to 30") {
      return (neighborhoodTop10_25)
    } else if (input$agerangeid == "31 to 36") {
      return (neighborhoodTop10_31)
    } else if (input$agerangeid == "37 to 42") {
      return (neighborhoodTop10_37)
    } else if (input$agerangeid == "43 to 48") {
      return (neighborhoodTop10_43)
    } else if (input$agerangeid == "49 to 54") {
      return (neighborhoodTop10_49)
    } else if (input$agerangeid == "55 to 60") {
      return (neighborhoodTop10_55)
    } else if (input$agerangeid == "61 to 66") {
      return (neighborhoodTop10_61)
    } else if (input$agerangeid == "67 and over") {
      return (neighborhoodTop10_67)
    }
  )
  output$agemap <- renderLeaflet({
    if (input$agerangeid == "All") {
      return (leaflet_All)
    } else if (input$agerangeid == "13 to 18") {
      return (leaflet_13)
    } else if (input$agerangeid == "19 to 24") {
      return (leaflet_19)
    } else if (input$agerangeid == "25 to 30") {
      return (leaflet_25)
    } else if (input$agerangeid == "31 to 36") {
      return (leaflet_31)
    } else if (input$agerangeid == "37 to 42") {
      return (leaflet_37)
    } else if (input$agerangeid == "43 to 48") {
      return (leaflet_43)
    } else if (input$agerangeid == "49 to 54") {
      return (leaflet_49)
    } else if (input$agerangeid == "55 to 60") {
      return (leaflet_55)
    } else if (input$agerangeid == "61 to 66") {
      return (leaflet_61)
    } else if (input$agerangeid == "67 and over") {
      return (leaflet_67)
    }
    
  })
  output$bestHourBox <- renderInfoBox({
    infoBox(
      "Best Hour", count(julDFbyHour[julDFbyHour$sex==input$genderid & julDFbyHour$age>=input$ageid[1] &  julDFbyHour$age<=input$ageid[2], ])[count(julDFbyHour[julDFbyHour$sex==input$genderid & julDFbyHour$age>=input$ageid[1] &  julDFbyHour$age<=input$ageid[2], ])$n==max(count(julDFbyHour[julDFbyHour$sex==input$genderid & julDFbyHour$age>=input$ageid[1] &  julDFbyHour$age<=input$ageid[2], ])$n),]$hour[1], 
      icon = icon("clock-o"),
      color = "purple", fill =TRUE
    )
  })
  output$bestDayBox <- renderInfoBox({
    infoBox(
      "Best Day", count(julDFbyDay[julDFbyDay$sex==input$genderid & julDFbyDay$age>=input$ageid[1] &  julDFbyDay$age<=input$ageid[2], ])[count(julDFbyDay[julDFbyDay$sex==input$genderid & julDFbyDay$age>=input$ageid[1] &  julDFbyDay$age<=input$ageid[2], ])$n==max(count(julDFbyDay[julDFbyDay$sex==input$genderid & julDFbyDay$age>=input$ageid[1] &  julDFbyDay$age<=input$ageid[2], ])$n),]$day[1],
      icon = icon("calendar"),
      color = "yellow", fill =TRUE
    )
  })
  output$hourChart <- renderPlotly({
    g4= ggplot(data= jul17dfClean %>% filter(sex== input$genderid)%>% filter(age >= input$ageid[1] & age <= input$ageid[2]), aes(x=hour, fill = hour)) + 
      geom_bar(colour="white", size=.3)+ guides(fill=FALSE) +
      xlab("Hour Of The Day")+ylab("Count")+ ggtitle("Riders By Hour")+
      theme(axis.text.x =element_text(angle=45), legend.position = "none", plot.margin = margin(10,10,40,10))
    ggplotly(g4) %>% layout(height  = 300)
  })
  output$dayChart <- renderPlotly({
    g5= ggplot(data= jul17dfClean %>% filter(sex== input$genderid)%>% filter(age >= input$ageid[1] & age <= input$ageid[2]), aes(x=day)) + 
      geom_bar(colour="black", size =.7)+ guides(fill=FALSE) +
      xlab("Day Of The Week")+ylab("Count")+ ggtitle("Most Popular Day of the Week")+
      theme_bw()+ theme(plot.margin = margin(10,10,40,20))
    ggplotly(g5) %>% layout(height  = 300)
  })
})
