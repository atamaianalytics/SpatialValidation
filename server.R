

library(shiny)
library(leaflet)
library(leaflet.extras)
library(sp)
library(utf8)
library(gtools)
library(rgdal)
library(plotly)


server <- function(input, output, session) {
  
#Project selection
project_sql <- paste0("select distinct(PROJECTCODE) from HOLELOCATION hl
                      WHERE HOLETYPE = 'DRILLHOLE' and exists (select distinct(PROJECTCODE) from HOLECOORD hc
                      WHERE hc.HOLEID = hl.HOLEID and hc.PROJECTCODE = hl.PROJECTCODE)
                      order by PROJECTCODE")
  
  
  project <- reactive({
    ### Testing locally ###
    if(Sys.info()["nodename"]=="DESKTOP-@@@@") {
    dbhandle <- odbcDriverConnect('driver={SQL Server};server=<server>;database=<database>;uid=<user>;pwd=<password>')
    df <- sqlQuery(dbhandle,project_sql)
    ### shinyapps.io connection ##
    } else {
    dbhandle <- DBI::dbConnect(odbc::odbc(),Driver = "SQLServer",Server = "<server>",Database = "<database>",UID = "<user>",PWD = "<password>",Port = 1433)
    df <- dbGetQuery(dbhandle,project_sql)
  }
  })
  
  output$project <- renderUI({
    data <- project()
    choice <-  unique(data$PROJECTCODE)
    selectInput("project","Project", choices = choice, selected = choice[1])
  })

#Element selection
  element_sql <- paste0("select distinct(ELEMENT) from ASSAYTYPE 
                      where ELEMENT in ('Li','Be','B','C','N','O','F','Ne','Na','Mg','Al','Si','P','S','Cl','Ar','K','Ca','Sc'
                                        ,'Ti','V','Cr','Mn','Fe','Co','Ni','Cu','Zn','Ga','Ge','As','Se','Br','Kr','Rb','Sr','Y'
                                        ,'Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','In','Sn','Sb','Te','I','Xe','Cs','Ba','La'
                                        ,'Ce','Pr','Nd','Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb','Lu','Hf','Ta','W','Re'
                                        ,'Os','Ir','Pt','Au','Hg','Tl','Pb','Bi','Po','At','Rn','Fr','Ra','Ac','Th','Pa','U')
                      and UNITS in ('ppm','pct','ppb') and ELEMENT is not null order by ELEMENT")
  
  element <- reactive({
    ### Testing locally ###
    if(Sys.info()["nodename"]=="DESKTOP-@@@@") {
    dbhandle <- odbcDriverConnect('driver={SQL Server};server=<server>;database=<database>;uid=<user>;pwd=<password>')
    df <- sqlQuery(dbhandle,element_sql)
    ### shinyapps.io connection ##
    } else {
    dbhandle <- DBI::dbConnect(odbc::odbc(),Driver = "SQLServer",Server = "<server>",Database = "<database>",UID = "<user>",PWD = "<password>",Port = 1433)
    df <- dbGetQuery(dbhandle,element_sql)
    }
  })
  
  output$element <- renderUI({
    data <- element()
    choice <-  unique(data$ELEMENT)
    selectInput("element","Element", choices = choice, selected = choice[1])
  })
  
#Geological attribute selection
  geoattrib_sql <- paste0("select distinct(NAME) from GEOLOGYCODESECONDARY where DATATYPE = 'Text'")
  
  geoattrib <- reactive({
    ### Testing locally ###
    if(Sys.info()["nodename"]=="DESKTOP-@@@@") {
    dbhandle <- odbcDriverConnect('driver={SQL Server};server=<server>;database=<database>;uid=<user>;pwd=<password>')
    df <- sqlQuery(dbhandle,geoattrib_sql)
    } else {
    ### shinyapps.io connection ## 
    dbhandle <- DBI::dbConnect(odbc::odbc(),Driver = "SQLServer",Server = "<server>",Database = "<database>",UID = "<user>",PWD = "<password>",Port = 1433)
    df <- dbGetQuery(dbhandle,geoattrib_sql)
    }
  })
  
  output$geoattrib <- renderUI({
    data <- geoattrib()
    choice <-  unique(data$NAME)
    selectInput("geoattrib","Geology Attribute", choices = choice, selected = choice[1])
  })
  
 # Generate map 
  mapdata <- reactive({
    sql <- paste0("select top 15
                  PROJECTCODE,
                  HOLEID as HoleID,
                  X as Longitude,
                  Y as Latitude
                  from HOLECOORD where COORDINATESET = 'LL_WGS84_Calculated'
                  and PROJECTCODE = '",input$project,"'")
    
  })
  
  MapData <- eventReactive(input$getdata,{
    ### Testing locally ###
    if(Sys.info()["nodename"]=="DESKTOP-@@@@") {
    dbhandle <- odbcDriverConnect('driver={SQL Server};server=<server>;database=<database>;uid=<user>;pwd=<password>')
    df <- sqlQuery(dbhandle,mapdata())
    ### shinyapps.io connection ## 
    } else {
    dbhandle <- DBI::dbConnect(odbc::odbc(),Driver = "SQLServer",Server = "<server>",Database = "<database>",UID = "<user>",PWD = "<password>",Port = 1433)
    df <- dbGetQuery(dbhandle,mapdata())
    }
  })
  
  output$mymap <- renderLeaflet({
    collars <- MapData()
    collars$HoleID <- as.character(collars$HoleID)
    collar_coordinates <- SpatialPointsDataFrame(collars[,c("Longitude","Latitude")],collars)
    leaflet() %>%
      #setView(0,0,2) %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addMarkers(data=collars,lat=~Latitude,lng=~Longitude,label=~HoleID) %>%
      addDrawToolbar(
        targetGroup='draw',
        polylineOptions=FALSE,
        markerOptions = FALSE,
        circleOptions = TRUE)  %>%
      addLayersControl(overlayGroups = c('draw'), options =
                         layersControlOptions(collapsed=FALSE)) 
  })
  
# Collar selection from map
  selected_collars <- reactive({
    collars <- MapData()
    collars$HoleID <- as.character(collars$HoleID)
    collar_coordinates <- SpatialPointsDataFrame(collars[,c("Longitude","Latitude")],collars)
    #use the draw_stop event to detect when users finished drawing
    req(input$mymap_draw_stop)
    print(input$mymap_draw_new_feature)
    feature_type <- input$mymap_draw_new_feature$properties$feature_type
    
    if(feature_type %in% c("rectangle","polygon")) {
      
      #get the coordinates of the polygon
      polygon_coordinates <- input$mymap_draw_new_feature$geometry$coordinates[[1]]
      
      #transform them to an sp Polygon
      drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
      
      #use over from the sp package to identify selected drillholes
      selected_collars <- collar_coordinates %over% SpatialPolygons(list(Polygons(list(drawn_polygon),"drawn_polygon")))
      
      #print the name of the drillholes
      collars[which(!is.na(selected_collars)),"HoleID"]
    } else if(feature_type=="circle") {
      #get the coordinates of the center of the cirle
      center_coords <- matrix(c(input$mymap_draw_new_feature$geometry$coordinates[[1]],input$mymap_draw_new_feature$geometry$coordinates[[2]]),ncol=2)
      
      #calculate the distance of the drillholes to the center
      dist_to_center <- spDistsN1(collar_coordinates,center_coords,longlat=TRUE)
      
      #select the drillholes that are closer to the center than the radius of the circle
      collars[dist_to_center < input$mymap_draw_new_feature$properties$radius/1000,"HoleID"]
    }
    
    
  })
  
# survey selection for desurveying 
  survey_sql <- reactive({
  selected_drillholes <- toString(paste0(chr(39),selected_collars(),chr(39)))
          sql <- paste0("select
                         hs.HOLEID
                        ,hs.PROJECTCODE
                        ,hs.DEPTH
                        ,hs.SURVTYPE
                        ,sa.GDAZIMUTH as AZIMUTH
                        ,'Geographic' as AZIMUTHTYPE
                        ,hs.DIP
                        ,hs.PRIORITY
                        ,sa.GRIDNAME as AZIMUTHGRIDNAME
                        from HOLESURVEY hs 
                        inner join SURVEYAZIMUTH sa
                        on
                        hs.PROJECTCODE = sa.PROJECTCODE
                        and hs.HOLEID = sa.HOLEID
                        and hs.DEPTH = sa.DEPTH
                        and hs.SURVTYPE = sa.SURVTYPE
                        where hs.PRIORITY = 1
                        and sa.GRIDNAME = 'LL_WGS84'
                        and hs.HOLEID in (",selected_drillholes,")")
  })
  
# sample selection for desurveying 
  sample_sql <- reactive({
   selected_drillholes <- toString(paste0(chr(39),selected_collars(),chr(39)))
         sql <- paste0("select
                        SAMPLEID
                        ,HOLEID
                        ,PROJECTCODE
                        ,SAMPFROM
                        ,SAMPTO
                        ,SAMPLETYPE
                        ,PRIORITY
                        ,[dbo].[QFN_MAJ_GEOCODE](HOLEID,PROJECTCODE,SAMPFROM,SAMPTO, 1, '",input$geoattrib,"', 1) as GeoAttribute
                        ,[dbo].[QFN_CORPBESTASSAY_VALUE](SAMPLEID,'",input$element,"','",input$Unit,"',1,3) AssayValue
                        from SAMPLE
                        where PRIORITY = 1
                        and HOLEID in (",selected_drillholes,")")
  })
  
# drillhole coordinate selection for desurveying
  holecoord_sql <- reactive({
    selected_drillholes <- toString(paste0(chr(39),selected_collars(),chr(39)))
              sql <- paste0("select hc.*, hl.DEPTH as TD from HOLECOORD hc
                           inner join HOLELOCATION hl
                           on hc.HOLEID = hl.HOLEID and hc.PROJECTCODE = hl.PROJECTCODE
                           where hc.HOLEID in (",selected_drillholes,") 
                           and hc.COORDINATESET = 'LL_WGS84_Calculated'")
   
  })
  
# fetch survey selection from database
   df_survey <- eventReactive(input$desurvey,{
   ### Testing locally ###
  if(Sys.info()["nodename"]=="DESKTOP-@@@@") {
   dbhandle <- odbcDriverConnect('driver={SQL Server};server=<server>;database=<database>;uid=<user>;pwd=<password>')
   df <- sqlQuery(dbhandle,survey_sql())
   ### shinyapps.io connection ##
  } else {
   dbhandle <- DBI::dbConnect(odbc::odbc(),Driver = "SQLServer",Server = "<server>",Database = "<database>",UID = "<user>",PWD = "<password>",Port = 1433)
   df_survey <- dbGetQuery(dbhandle,survey_sql())
  }
   })
  
# fetch sample selection from database
   df_sample <- eventReactive(input$desurvey,{
   ### Testing locally ###
  if(Sys.info()["nodename"]=="DESKTOP-@@@@") {
   dbhandle <- odbcDriverConnect('driver={SQL Server};server=<server>;database=<database>;uid=<user>;pwd=<password>')
   df <- sqlQuery(dbhandle,sample_sql())
   ### shinyapps.io connection ##
  } else {
   dbhandle <- DBI::dbConnect(odbc::odbc(),Driver = "SQLServer",Server = "<server>",Database = "<database>",UID = "<user>",PWD = "<password>",Port = 1433)
   df_sample <- dbGetQuery(dbhandle,sample_sql())
  }
   })
   
# fetch drillhole coordinate selection from database
   df_holecoord <- eventReactive(input$desurvey,{
   ### Testing locally ###
  if(Sys.info()["nodename"]=="DESKTOP-@@@@") {
   dbhandle <- odbcDriverConnect('driver={SQL Server};server=<server>;database=<database>;uid=<user>;pwd=<password>')
   df <- sqlQuery(dbhandle,holecoord_sql())
   ### shinyapps.io connection ##
  } else {
   dbhandle <- DBI::dbConnect(odbc::odbc(),Driver = "SQLServer",Server = "<server>",Database = "<database>",UID = "<user>",PWD = "<password>",Port = 1433)
   df_sample <- dbGetQuery(dbhandle,holecoord_sql())
  }
   })
   
# desurvey drillholes
   desurvey <- reactive({
     
     interval <- df_sample() %>% arrange(HOLEID,SAMPFROM)
     
     interval <- interval %>% 
       mutate(midpoint = (SAMPFROM+SAMPTO)/2) %>%
       select(SAMPLEID,HOLEID,PROJECTCODE,SAMPFROM,SAMPTO,SAMPLETYPE,PRIORITY,midpoint,GeoAttribute,AssayValue)
     
     df_survey <- df_survey() %>% arrange(HOLEID,DEPTH)
     
     collar <- df_holecoord() %>% arrange(HOLEID)
     
     ########## Convert Coordinates ########################
     drillhole_coordinates <- collar %>% select(X,Y)
     coordinates(drillhole_coordinates) <- c("X", "Y")
     proj4string(drillhole_coordinates) <- CRS("+proj=longlat +datum=WGS84")  
     
     
     CollarCoordsUTM <- spTransform(drillhole_coordinates, CRS("+init=epsg:28350"))
     
     CollarCoordsUTM <- cbind(collar$HOLEID,as.data.frame(CollarCoordsUTM),collar$Z)
     
     holeid <- data.frame(unique((CollarCoordsUTM[,1])))
     
     holeid <- holeid %>% mutate_if(is.factor, as.character)
     
     fin_surv_all <- data.frame()
     
     
     for (k in 1:nrow(holeid)) {
       east <- CollarCoordsUTM[k,2]
       north <- CollarCoordsUTM[k,3]
       elev <- CollarCoordsUTM[k,4]
       
if(nrow(interval[interval$HOLEID==holeid[k,1],])>0 & nrow(df_survey[df_survey$HOLEID==holeid[k,1],])>0) {       
      
       cdip <- df_survey$DIP[df_survey$HOLEID==holeid[k,1]][1]
       cazi <- df_survey$AZIMUTH[df_survey$HOLEID==holeid[k,1]][1]

       int <- interval[interval$HOLEID==holeid[k,1],]
       
       surv_fil <- df_survey[df_survey$HOLEID==holeid[k,1],] %>% filter(DEPTH<=max(int$SAMPFROM[1]))
       
       
       if (int$SAMPFROM[1]==0) {
         survey <-  df_survey[df_survey$HOLEID==holeid[k,1],]
       } else {
         survey <-  df_survey[df_survey$HOLEID==holeid[k,1],] %>% filter(DEPTH>=max(surv_fil$DEPTH)) 
       }
       
       surv_td_pt <- tail(survey,1) %>% mutate(DEPTH=collar[collar$HOLEID==holeid[k,1],]$TD)
       
       if(max(df_survey[df_survey$HOLEID==holeid[k,1],]$DEPTH)==collar[collar$HOLEID==holeid[k,1],]$TD) {
         survey <- survey
       } else {
         survey <- rbind(survey,surv_td_pt)
       }
       
       
       survey <- survey %>% arrange(DEPTH)
       
       icounter = 1
       scounter = 1
       df_x <- data.frame()
       df_y <- data.frame()
       df_z <- data.frame()
       df_zz <- data.frame()
       for (i in 1:nrow(int)) {
         if (int[icounter,8]>=survey[scounter,3] & int[icounter,8]<=survey[scounter+1,3]) {
           x <- int[icounter,] %>% 
             mutate(lsurv=survey[scounter,3]) %>% 
             mutate(usurv = survey[scounter+1,3]) %>%
             mutate(ldip = survey[scounter,7]) %>%
             mutate(udip = survey[scounter+1,7]) %>%
             mutate(lazi = survey[scounter,5]) %>%
             mutate(uazi = survey[scounter+1,5])
           icounter = icounter + 1
           df_x <- rbind(df_x,x)
         } else if (int[icounter,8]>=survey[scounter+1,3] & int[icounter,8]<=survey[scounter+2,3]) {
           y <- int[icounter,] %>% 
             mutate(lsurv=survey[scounter+1,3]) %>% 
             mutate(usurv = survey[scounter+2,3]) %>%
             mutate(ldip = survey[scounter+1,7]) %>%
             mutate(udip = survey[scounter+2,7]) %>%
             mutate(lazi = survey[scounter+1,5]) %>%
             mutate(uazi = survey[scounter+2,5])
           icounter = icounter + 1
           df_y <- rbind(df_y,y)
           scounter <- scounter + 1
         } else if (int[icounter,8]>=survey[scounter+1,3] & int[icounter,8]>survey[scounter+2,3]) {
           prev_depth <- max(survey[survey$DEPTH<=int[icounter,8],3])
           next_depth <- min(survey[survey$DEPTH>=int[icounter,8],3])
           prev_dip <- survey[survey$DEPTH==prev_depth,7]
           next_dip <- survey[survey$DEPTH==next_depth,7]
           prev_azi <- survey[survey$DEPTH==prev_depth,5]
           next_azi <- survey[survey$DEPTH==next_depth,5]
           ncounter <- which(survey$DEPTH==next_depth)
           z <- int[icounter,] %>% 
             mutate(lsurv = prev_depth) %>% 
             mutate(usurv = next_depth) %>%
             mutate(ldip = prev_dip) %>%
             mutate(udip = next_dip) %>%
             mutate(lazi = prev_azi) %>%
             mutate(uazi = next_azi)
           icounter = icounter + 1
           df_z <- rbind(df_z,z)
           scounter <- scounter + 1
         } else if (int[icounter,8]<survey[scounter,3] ){
           prev_depth <- min(survey[survey$DEPTH>=int[icounter,8],3])
           next_depth <- min(survey[survey$DEPTH>=int[icounter,8],3])
           prev_dip <- survey[survey$DEPTH==prev_depth,7]
           next_dip <- survey[survey$DEPTH==next_depth,7]
           prev_azi <- survey[survey$DEPTH==prev_depth,5]
           next_azi <- survey[survey$DEPTH==next_depth,5]
           ncounter <- which(survey$DEPTH==next_depth)
           zz <- int[icounter,] %>% 
             mutate(lsurv = prev_depth) %>% 
             mutate(usurv = next_depth) %>%
             mutate(ldip = prev_dip) %>%
             mutate(udip = next_dip) %>%
             mutate(lazi = prev_azi) %>%
             mutate(uazi = next_azi)
           icounter = icounter + 1
           df_zz <- rbind(df_zz,zz)
         }

       }
       
       
       df_s <- rbind(df_x,df_y,df_z)
       
       df_s <- df_s %>% arrange(SAMPFROM)
       
       df_s <- df_s %>% 
         mutate(mp_l=midpoint-lsurv) %>%
         mutate(mp_u=(usurv-midpoint)+0.001) %>%
         mutate(wt_dip=((ldip*mp_u)+(udip*mp_l))/(mp_l+mp_u)) %>%
         mutate(wt_azi=((lazi*mp_u)+(uazi*mp_l))/(mp_l+mp_u))
       
       
       csurv <- head(survey,1)
       csurv <- csurv %>% 
         mutate(SAMPLEID="Collar") %>%
         mutate(midpoint=0) %>%
         mutate(wt_azi=AZIMUTH) %>%
         mutate(wt_dip=DIP) %>%
         mutate(X = east) %>%
         mutate(Y = north) %>%
         mutate(Z = elev) %>%
         mutate(GeoAttribute="Collar") %>%
         mutate(AssayValue=0) %>%
         select(HOLEID,SAMPLEID,midpoint,wt_dip,wt_azi,X,Y,Z,GeoAttribute,AssayValue)
       
       df_s <- df_s %>% 
         mutate(X = as.numeric("")) %>%
         mutate(Y = as.numeric("")) %>%
         mutate(Z = as.numeric("")) %>%
         select(HOLEID,SAMPLEID,midpoint,wt_dip,wt_azi,X,Y,Z,GeoAttribute,AssayValue)
       
       fin_surv <- rbind(csurv,df_s)
       
       fin_surv <- fin_surv %>% arrange(midpoint)
       
       fscounter = 2
       for (j in 2:nrow(fin_surv)) {
         p1_depth <- fin_surv[fscounter-1,3]
         p2_depth <- fin_surv[fscounter,3]
         p1_inc <- 90-abs(fin_surv[fscounter-1,4])
         p2_inc <- 90-abs(fin_surv[fscounter,4])
         p1_azi <- abs(fin_surv[fscounter-1,5])
         p2_azi <- abs(fin_surv[fscounter,5])
         x1 <- fin_surv[fscounter-1,6]
         y1 <- fin_surv[fscounter-1,7]
         z1 <- fin_surv[fscounter-1,8]
         brad <- acos(cos((p2_inc*pi/180)-(p1_inc*pi/180))-(sin(p1_inc*pi/180)*sin(p2_inc*pi/180)*(1-cos((p2_azi*pi/180)-(p1_azi*pi/180)))))
         bdeg <- brad*(180/pi)
         if(brad==0 & bdeg==0) {
           RF <- 1
         } else RF <- 2/brad*tan(brad/2)
         n <- (p2_depth-p1_depth)/2*(sin(p1_inc*pi/180)*cos(p1_azi*pi/180)+sin(p2_inc*pi/180)*cos(p2_azi*pi/180))*RF
         e <- (p2_depth-p1_depth)/2*(sin(p1_inc*pi/180)*sin(p1_azi*pi/180)+sin(p2_inc*pi/180)*sin(p2_azi*pi/180))*RF
         tvd <- (p2_depth-p1_depth)/2*(cos(p1_inc*pi/180)+cos(p2_inc*pi/180))*RF
         fin_surv[fscounter,6] <- fin_surv[fscounter-1,6]+e
         fin_surv[fscounter,7] <- fin_surv[fscounter-1,7]+n 
         fin_surv[fscounter,8] <- fin_surv[fscounter-1,8]-tvd
         fscounter <- fscounter + 1
       }
       
       
       fin_surv_all <- rbind(fin_surv_all,fin_surv)
       
     } 
     }
     fin_surv_all
   })
   
########## Plans and Sections #####

   output$planview <- renderPlot({
     
     dataset <- desurvey() %>% filter(SAMPLEID!="Collar")
     collar <- desurvey() %>% filter(SAMPLEID=="Collar")
     
     if(input$Trace=="Assay") {
       p <- ggplot() +
         geom_point(data=dataset,aes(x=X,y=Y,col=AssayValue),size=3,shape=19,alpha=0.75) + 
         scale_colour_gradientn(colours = topo.colors(10)) +
         labs(title="Plan View",x="Easting",y="Northing",col=paste0(input$element,"_",input$Unit))
     } else if(input$Trace=="Geology") {
       p <- ggplot() +
          geom_point(data=dataset,aes(x=X,y=Y,col=GeoAttribute),size=3,shape=19,alpha=0.75) +
          labs(title="Plan View",x="Easting",y="Northing",col=input$geoattrib)
     }
     
      p <- p + 
        geom_point(data=collar,aes(x=X,y=Y,label=HOLEID)) +
        geom_text(data=collar,aes(x=X,y=Y,label=HOLEID),hjust=0, vjust=0,angle=315) +
        theme(axis.text.x=element_text(size = 12),
             axis.text.y=element_text(size = 12),
             axis.title.x=element_text(size = 12),
             axis.title.y=element_text(size = 12),
             plot.title = element_text(hjust = 0.5,face="bold"),
             legend.key = element_blank(), 
             legend.text =element_text(size = 12),
             legend.position = "right",
             panel.background = element_rect(fill = "white"),
             panel.border = element_rect(linetype = "solid", fill=NA, colour = "black"),
             panel.grid.major = element_line(colour = "grey",size=0.2),
             plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))
     return(p)
     
   })
   
   output$looknorth <- renderPlot({
     
     dataset <- desurvey() %>% filter(SAMPLEID!="Collar")
     collar <- desurvey() %>% filter(SAMPLEID=="Collar")
     
     if(input$Trace=="Assay") {
       p <- ggplot() +
         geom_point(data=dataset,aes(x=X,y=Z,col=AssayValue),size=3,shape=19,alpha=0.75) +
         scale_colour_gradientn(colours = topo.colors(10)) +
         labs(title="Looking North",x="Easting",y="Elevation",col=paste0(input$element,"_",input$Unit))
     } else if(input$Trace=="Geology") {
       p <- ggplot() +
         geom_point(data=dataset,aes(x=X,y=Z,col=GeoAttribute),size=3,shape=19,alpha=0.75) +
         labs(title="Looking North",x="Easting",y="Elevation",col=input$geoattrib)
     }
     
    p <- p + 
      geom_point(data=collar,aes(x=X,y=Z,label=HOLEID)) +
      geom_text(data=collar,aes(x=X,y=Z,label=HOLEID),hjust=0, vjust=0,angle=315) +
      theme(axis.text.x=element_text(size = 12),
             axis.text.y=element_text(size = 12),
             axis.title.x=element_text(size = 12),
             axis.title.y=element_text(size = 12),
             plot.title = element_text(hjust = 0.5,face="bold"),
             legend.key = element_blank(), 
             legend.text =element_text(size = 12),
             legend.position = "right",
             panel.background = element_rect(fill = "white"),
             panel.border = element_rect(linetype = "solid", fill=NA, colour = "black"),
             panel.grid.major = element_line(colour = "grey",size=0.2),
             plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))
     return(p)
     
   })
   
   output$lookeast <- renderPlot({
     
     dataset <- desurvey() %>% filter(SAMPLEID!="Collar")
     collar <- desurvey() %>% filter(SAMPLEID=="Collar")
     
     if(input$Trace=="Assay") {
       p <- ggplot() +
         geom_point(data=dataset,aes(x=Y,y=Z,col=AssayValue),size=3,shape=19,alpha=0.75) +
         scale_colour_gradientn(colours = topo.colors(10)) +
         labs(title="Looking East",x="Northing",y="Elevation",col=paste0(input$element,"_",input$Unit))
     } else if(input$Trace=="Geology") {
       p <- ggplot() +
         geom_point(data=dataset,aes(x=Y,y=Z,col=GeoAttribute),size=3,shape=19,alpha=0.75) +
         labs(title="Looking East",x="Northing",y="Elevation",col=input$geoattrib)
     }
     
     p <- p +
       geom_point(data=collar,aes(x=Y,y=Z,label=HOLEID)) +
       geom_text(data=collar,aes(x=Y,y=Z,label=HOLEID),hjust=0, vjust=0,angle=315) +
       scale_x_reverse() +
       theme(axis.text.x=element_text(size = 12),
             axis.text.y=element_text(size = 12),
             axis.title.x=element_text(size = 12),
             axis.title.y=element_text(size = 12),
             plot.title = element_text(hjust = 0.5,face="bold"),
             legend.key = element_blank(), 
             legend.text =element_text(size = 12),
             legend.position = "right",
             panel.background = element_rect(fill = "white"),
             panel.border = element_rect(linetype = "solid", fill=NA, colour = "black"),
             panel.grid.major = element_line(colour = "grey",size=0.2),
             plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))
     return(p)
     
   })
   
   
   
   output$threeD <- renderPlotly({
     dataset <- desurvey() 
     if(input$Trace=="Assay") {
     p <- plot_ly(dataset) %>%
          
       add_markers(x = ~dataset[dataset$SAMPLEID!="Collar",]$X, #X 
                   y = ~dataset[dataset$SAMPLEID!="Collar",]$Y, #Y
                   z = ~dataset[dataset$SAMPLEID!="Collar",]$Z, #Z
                   marker = list(color = ~dataset[dataset$SAMPLEID!="Collar",]$AssayValue, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
                   
       add_markers(x = ~dataset[dataset$SAMPLEID=="Collar",]$X, #X 
                   y = ~dataset[dataset$SAMPLEID=="Collar",]$Y, #Y
                   z = ~dataset[dataset$SAMPLEID=="Collar",]$Z, #Z
                   marker = list(color = 'blue', symbol = 'cross'),
                   showlegend = FALSE
                   
                   ) %>%
       add_text(x = ~dataset[dataset$SAMPLEID=="Collar",]$X, #X 
                y = ~dataset[dataset$SAMPLEID=="Collar",]$Y, #Y
                z = ~dataset[dataset$SAMPLEID=="Collar",]$Z, #Z
                text = ~dataset[dataset$SAMPLEID=="Collar",]$HOLEID,
                showlegend = FALSE,
                textposition = "top right") %>%
       layout(scene = list(xaxis = list(title = 'Easting',tickformat = '0.0f'),
                           yaxis = list(title = 'Northing',tickformat = '0.0f'),
                           zaxis = list(title = 'RL')))#,

   } else if (input$Trace=="Geology") {
     p <- plot_ly(dataset) %>%
       #plot_ly(collar, x = ~X, y = ~Y, z = ~Z) %>%
       add_markers(x = ~dataset[dataset$SAMPLEID!="Collar",]$X, #X 
                   y = ~dataset[dataset$SAMPLEID!="Collar",]$Y, #Y
                   z = ~dataset[dataset$SAMPLEID!="Collar",]$Z, #Z
                   color = ~dataset[dataset$SAMPLEID!="Collar",]$GeoAttribute) %>%
                   add_markers(x = ~dataset[dataset$SAMPLEID=="Collar",]$X, #X 
                               y = ~dataset[dataset$SAMPLEID=="Collar",]$Y, #Y
                               z = ~dataset[dataset$SAMPLEID=="Collar",]$Z, #Z
                               marker = list(color = 'blue', symbol = 'cross'),
                               showlegend = FALSE
                               
                   ) %>%
                     add_text(x = ~dataset[dataset$SAMPLEID=="Collar",]$X, #X 
                              y = ~dataset[dataset$SAMPLEID=="Collar",]$Y, #Y
                              z = ~dataset[dataset$SAMPLEID=="Collar",]$Z, #Z
                              text = ~dataset[dataset$SAMPLEID=="Collar",]$HOLEID,
                              showlegend = FALSE,
                              textposition = "top right") %>%
                     layout(scene = list(xaxis = list(title = 'Easting',tickformat = '0.0f'),
                                         yaxis = list(title = 'Northing',tickformat = '0.0f'),
                                         zaxis = list(title = 'RL')))
   }
     return(p)
   })
   
   output$planviewPlot <- renderUI({
     withSpinner(
       plotOutput("planview"),type=4,color="#35e51d"
     )
   })

   output$looknorthPlot <- renderUI({
     withSpinner(
       plotOutput("looknorth"),type=4,color="#35e51d"
     ) 
   })
   
   output$lookeastPlot <- renderUI({
     withSpinner(
       plotOutput("lookeast"),type=4,color="#35e51d"
     )
   })
   
   output$threeDPlot <- renderUI({
       plotOutput("threeD")
   })
   

  
}