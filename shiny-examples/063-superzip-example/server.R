# %load shiny-examples/063-superzip-example/server.R
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$centile),]

function(input, output, session) {

  ## Interactive Map ###########################################
  #v<-reactiveValues(data=NULL)
  
  #map.on("load",function(e){
  #    print("map.on.load")
  #})
    
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 22, lat = 52, zoom = 7) #%>%
      #addPolygons(data=powiatyOGR) 
      #, color = "#444444", weight = 5, smoothFactor = 0.5,
      #opacity = 1.0, fillOpacity = 0.5,
      #highlightOptions = highlightOptions(color = "white", weight = 2,
      #=ringToFront = TRUE))
  })
    


  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(zipdata,
      latitude >= latRng[1] & latitude <= latRng[2] &
        longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  

  # Precalculate the breaks we'll need for the two histograms
  centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks

  output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)

    hist(zipsInBounds()$centile,
      breaks = centileBreaks,
      main = "SuperZIP score (visible zips)",
      xlab = "Percentile",
      xlim = range(allzips$centile),
      col = '#00DD00',
      border = 'white')
  })

  output$scatterCollegeIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)

    print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
#  observe({
#    colorBy <- input$color
#    sizeBy <- ""#input$size
#
#    if (colorBy == "superzip") {
#      # Color and palette are treated specially in the "superzip" case, because
#      # the values are categorical instead of continuous.
#      colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
#      pal <- colorFactor("viridis", colorData)
#    } else {
#      colorData <- zipdata[[colorBy]]
#      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
#    }

#    if (sizeBy == "superzip") {
#      # Radius is treated specially in the "superzip" case.
#      radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
#    } else {
#      radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
#    }

#    leafletProxy("map", data = zipdata) %>%
#      clearShapes() %>%
#      addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
#        stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
#      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
#        layerId="colorLegend")
#  })

 # Show a popup at the given location
  showZipcodePopup <- function(zipcode, lat, lng) {
    selectedZip <- allzips[allzips$zipcode == zipcode,]
      
    cnames<-as.vector(matrix(rep(colnames(cleantable)[4:NCOL(cleantable)],2),2,byrow=TRUE))
    idx<-rep(1:2,length(4:NCOL(cleantable)))==2
    cnames[idx]<-NA
    content <- as.character(
      do.call(tagList,
      c(list(
      tags$h4("Powiat ", cleantable[zipcode,1]),#as.integer(zipcode)),
      tags$strong(HTML(sprintf("%s, %s %s",
        selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
      ))), 
      tags$br()),
      lapply(cnames,function(cn){
           if (is.na(cn)) return(tags$br())
           sprintf("%s: %s", cn, cleantable[zipcode,cn])
          }
        )
      )
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event)) return()
    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
  
  showLatLng <- function(lat,lng) {
    output$coord_lat <- renderText({
      paste("Latitude:",lat)
    })
    output$coord_lng <- renderText({
      paste("Longitude:",lng)
    })
  }
  
  observe({
    proxy <- leafletProxy("map")
    proxy %>% clearPopups()
    event <- input$map_click
    isolate({
      showLatLng(event$lat,event$lng)
    })
  })


  ## Data Explorer ###########################################

  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
      selected = stillSelected)
  })

  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
          is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
      selected = stillSelected)
  })

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
    
    
   ################################3
  
  printTest<-function(s){
    output$testText <- renderText({s})    
  }
    
  color_all<-function(dataCol=powiatyData[,6]){
     mnDC<-min(dataCol,na.rm=TRUE)
     mxDC<-max(dataCol,na.rm=TRUE)
     dataCol<-(dataCol-mnDC)/max(0.000000001,mxDC-mnDC)
     dataCol<-0.3+dataCol*0.7
     dataCol[is.na(dataCol)]<-0
     proxy <- leafletProxy("map")
     proxy %>% clearShapes()
     for (powiati in 1:length(powiatyOGR$NAZWA)) {
     selPols<-powiatyOGR@polygons[powiati]
     if (length(selPols)>=1) {         
        regLngLat<-selPols[[1]]@labpt
        
        print(regLngLat)
        proxy %>% addPolygons(data = powiatyOGR@polygons[powiati][[1]], 
                              fillColor = "green",
                              fillOpacity = dataCol[powiati], 
                              color = "red",
                              weight = 2, 
                              stroke = TRUE,
                              layerId = powiati)
        }
     }
  }
    
  do_shape_onclick <-function(click) {
    
    # add new circle
    #proxy %>% addCircles(lng=click$lng, lat=click$lat,radius=(1609.344*10),color='red')
      
    #proxy %>% addPolygons(data=powiatyOGR, color = "#444444", weight = 1, smoothFactor = 0.5)
      #opacity = 1.0, fillOpacity = 0.5,
      #highlightOptions = highlightOptions(color = "white", weight = 2,
      #=ringToFront = TRUE))
      
      clickGlob<<-click
      
      #pulls lat and lon from shiny click event
      lat <- click$lat
      lon <- click$lng

      #puts lat and lon for click point into its own data frame
      coords <- as.data.frame(cbind(lon, lat))

      #converts click point coordinate data frame into SP object, sets CRS
      #point <- SpatialPoints(coords)
      #proj4string(point) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      #printTest(paste("shape_onclick point"))  
      #retrieves country in which the click point resides, set CRS for country
      #selectedReg <- powiatyOGR[point,]
      #proj4string(selectedReg) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
      
      if (is.integer(click$id)) {
        printTest(paste("SelectedID:",click$id)) 
           
        printTest(paste("Selected:",as.character(powiatyOGR$NAZWA[click$id])))
           
      selPols<-powiatyOGR@polygons[click$id]
      if (length(selPols)>=1) {         
        regLngLat<-selPols[[1]]@labpt
        #printTest(paste("regLngLat:",regLngLat)) 
      
       proxy <- leafletProxy("map")
       # remove all markers and popups
       proxy %>% clearMarkers()
     #proxy %>% clearShapes()
    
     # add new marker around the center
     #proxy %>% addMarkers(lng=click$lng,lat = click$lat,popup='Your Starting Point')
      proxy %>% addMarkers(lng=regLngLat[1],lat = regLngLat[2],popup='Your Starting Point')
    
      proxy %>% removeShape(layerId = click$id)
      
      #leafletProxy("map")<-proxy
      
      markUnmark<-runif(1)>0.5
      if (markUnmark) {
         proxy %>% addPolygons(data = selPols[[1]], 
                              fillColor = "gray",
                              fillOpacity = 0.5, 
                              color = "red",
                              weight = 3, 
                              stroke = TRUE,
                              layerId = click$id)
      } else {
          do.call(addPolygons,c(list(map=proxy,data=selPols[[1]], layerId=click$id),
                           defaultRegionPolVals))
      }
           
      } # if selPols              
     } # if is.integer
  }
    
    
  #observeEvent(input$map_click, {
  #observe({
    #proxy <- leafletProxy("map")
    #proxy %>% clearPopups()
    #event <- input$map_click
 
 #  click<-input$map_click; if (is.null(click)) return()
  #  isolate({
  #  output$clickInfo <- renderPrint(click)
 #   text <-paste("lat ",click$lat," lon ",click$lng)
 #   print(text)
 #   #do_onclick(click)
 #   })
 # })
  
  #  #### what to do on shape click
  #observeEvent(input$map_shape_click, {
  observe({    
    click<-input$map_shape_click; if (is.null(click))  return()
    #if(!v$click) return()
    isolate({ 
    output$clickInfo <- renderPrint(click)
    text <-paste("map_shape_click lat ",click$lat," lon ",click$lng)
    printTest(text)
    do_shape_onclick(click)
    })
    
  }) 
  
  #testRes<-eventReactive(input$testButton, {
  #   print("testButton pressed") #input$testButton)    
    
  #})
 
  defaultRegionPolVals<-list(color = "#444444", weight = 1, smoothFactor = 0.5)
    
  prepareMap<-function() {
     proxy <- leafletProxy("map")
     do.call(addPolygons,c(list(map=proxy,data=powiatyOGR, layerId=~seq_along(NAZWA)),
                           defaultRegionPolVals))                 
            
  }        
    
  observe({     
     obj<-input$testButton; if (is.null(obj)) return()
     if(input$testButton==0) isolate({
         prepareMap()
         color_all(powiatyData[,as.numeric(input$dane)])
     }) else {
         daneGlob<<-input$dane
         #print(paste("input$dane",input$dane))
         color_all(powiatyData[,as.numeric(input$dane)])
     }
     #printTest(paste("LoadButton counter:",input$testButton)) 
     #print(input$testButton)
  })
    
     #if (!input$powiaty.loaded) isolate({
     #      leafletProxy("map") %>% 
     #       addPolygons(data=powiatyOGR, color = "#444444", weight = 1, smoothFactor = 0.5)  
     #})
    
     #if (v$testButton) print("testButton clicked!")
     #isolate({
        #leafletProxy("map") %>% 
        #addPolygons(data=powiatyOGR, color = "#444444", weight = 1, smoothFactor = 0.5)
        #opacity = 1.0, fillOpacity = 0.5,
        #highlightOptions = highlightOptions(color = "white", weight = 2,
      #=ringToFront = TRUE))
     #})   
  #})
        

  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
    #  filter(
    #    Score >= input$minScore,
    #    Score <= input$maxScore,
    #    is.null(input$states) | State %in% input$states,
    #    is.null(input$cities) | City %in% input$cities,
    #    is.null(input$zipcodes) | Zipcode %in% input$zipcodes
    #  ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', 1:NROW(cleantable), '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}