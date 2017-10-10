
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
    g4= ggplot(data= jul17dfClean %>% filter(sex== input$genderid)%>% filter(age >= input$ageid[1] & age <= input$ageid[2]), aes(x=hour)) + geom_bar()
    ggplotly(g4) %>% layout (height  = 300)
  })
  output$dayChart <- renderPlotly({
    g5= ggplot(data= jul17dfClean %>% filter(sex== input$genderid)%>% filter(age >= input$ageid[1] & age <= input$ageid[2]), aes(x=day)) + geom_bar()
    ggplotly(g5) %>% layout (height  = 300)
  })
})
