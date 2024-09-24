
#-----------------
# Sources of data :
# API Fulel Price : https://data.economie.gouv.fr/explore/dataset/prix-carburants-fichier-instantane-test-ods-copie/api/?rows=10000&refine.prix_maj=2022
# Explore this API : https://data.economie.gouv.fr/api/v2/console
# Console API : https://data.economie.gouv.fr/api/v1/console/datasets/1.0/search/


#***** Links Fuel Price Tracking : ********
# https://plein-moins-cher.fr/index.html
# https://9to5google.com/2022/06/07/find-gas-prices-near-you-in-waze/

# Pushing Shiny App in GitHub 
# https://www.youtube.com/watch?v=bUoN85QvC10

# My Working directory : /Users/daya/Documents/energy_tracking
#-------------------------------------------------------------


#------------------
# Loading Packages
#------------------
# scraping libs
library(httr) 
library(jsonlite)
library(tidyverse)
library(writexl) 

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinybusy)
library(DT)
library(reactable)
library(leaflet)

#-------------------------------------------daya entr
#           USER INTERFACE AREA
#-------------------------------------------
ui <- shinyUI(
  
  dashboardPage(skin="blue",   # skin: for changing the dashboard body
                
                dashboardHeader(title = "Fuel prices in France"),
                dashboardSidebar(
                  tags$style(type = 'text/css',".badge{min-width: 200px;}"),
                  sidebarMenu(
                    menuItem("Dev. in progress !", tabName = 'dashboard'),
                    menuItem(actionButton("btn_decretiz", "Discretization"))
                  )),
                
                ### Application Body ###
                dashboardBody(
                  navbarPage(
                    titlePanel(" "),
                    
                    # Page1 --------------------------------------------------------------------------------------------------------------------------------
                    tabPanel("Home",icon = icon("database"),
                             add_busy_spinner(spin = "fading-circle"),
                             fluidPage(
                               fluidRow(reactableOutput("table1"))
                             )
                    ),
                    
                    # Page2 -------------------------------------------------------------------------------------------------------------------------------
                    tabPanel("Map view",
                             fluidRow(
                               leafletOutput("map"),   
                               # uiOutput('map_dep_code'),
                               #--------------------------------------------------
                               pickerInput("dep_code", label = "Select a Dep Code:",
                                           # choices = list("All dep_code", `Dep_code :` = uiOutput('moreControls')),
                                           choices = list("All dep_code", `Dep_code :` =c("34","74", "14")),
                                           options = list(`live-search` = TRUE)
                               ),
                               #--------------------------------------------------
                               
                             ) #End Fluidrow
                             
                    ), #End tabPanel
                    
                    # Page3 --------------------------------------------------------------------------------------------------------------------------------
                    tabPanel("Overview",icon = icon("database"),
                             
                             
                    )
                    
                    
                    
                    
                    #---------------------------------------------------------------
                  )#End navbarPage 
                )#End dashboardBody
  ) #End dashboardPage
  
) #End shinyUI


#-------------------------------------------
#               SERVER AREA                     
#-------------------------------------------
server <- function(input, output, session) {
  
  # GETTING DATA FROM API SOURCE #############################################
  base_url <-GET("https://data.economie.gouv.fr/api/records/1.0/search/?dataset=prix-carburants-fichier-instantane-test-ods-copie&q=&rows=10000&facet=id&facet=adresse&facet=ville&facet=prix_maj&facet=prix_nom&facet=com_arm_name&facet=epci_name&facet=dep_name&facet=reg_name&facet=services_service&facet=horaires_automate_24_24&refine.prix_maj=2022")
  httr::http_status(base_url)    
  get_url_text <- httr::content(base_url,as = "text")         # content formatting
  get_data_json<-jsonlite::fromJSON(get_url_text,flatten = T) # content retrieval
  
  
  # DATA MANAGEMENT ##########################################################
  
  # Managing Data tables
  get_data_base<-as.data.frame(get_data_json$records, na.string = c("", " ", "NA", "N/A"))
  get_data_group <- as.data.frame(get_data_json$facet_groups)
  get_data_param <- as.data.frame(get_data_json$parameters)
  get_data_nrows <- as.data.frame(get_data_json$nhits)
  
  
  # Constitution of the database
  names(get_data_base)
  data_base = subset(get_data_base, select = - c(1:2))  # Drop the 2 first columns (:= datasetid, recordsetid)
  # L_var <- list(names(data_base))   # we have 25 colums in total 'All'
  # data_base = subset(data_base, select = - c(10, 14))  # Drop the 2 first columns (:= horaires , services_service)
  
  # Rename columns
  colnames(data_base) = c("id", "prix_id", "pop", "reg_code", "reg_name",
                          "automate_24h_24", "com_arm_name", "adresse", "cp",
                          "horaires", "dep_code", "ville", "epci_code", "services_service", "dep_name",
                          "com_arm_code", "epci_name", "geom", "prix_valeur", "prix_nom",
                          "prix_maj", "geometry.type","geometry.coordinates")
  
  # 'NA' Management #
  apply(data_base, MARGIN = 2, FUN = function(x){x%>%is.na%>%sum})
  sum(is.na(data_base))
  
  
  ## Testing for the output ##
  # data_base <- data_base %>%
  #   select("id", "prix_id", "pop", "reg_code", "reg_name")
  
  # NA's Replacement ------------------------------------------------------------------------------
  # horaires, services_service
  # data_base <- mutate_at(data_base, c("horaires", "services_service"), ~replace(., is.na(.), 0))
  
  # data_base$horaires[is.na(data_base$horaires)] <-"X"
  # data_base$services_service[is.na(data_base$services_service)] <- "X"
  #-------------------------------------------------------------------------------------------------
  
  
  # Renaming step of the columns-------------------------------#
  data_base$id = as.numeric(data_base$id)
  data_base$prix_id = as.numeric(data_base$prix_id) 
  data_base$pop = as.factor(data_base$pop)
  data_base$reg_code = as.factor(data_base$reg_code) 
  data_base$reg_name = as.factor(data_base$reg_name)
  
  data_base$automate_24h_24 = as.factor(data_base$automate_24h_24)
  data_base$com_arm_name = as.factor(data_base$com_arm_name)
  data_base$adresse = as.factor(data_base$adresse)
  data_base$cp = as.factor(data_base$cp)
  
  data_base$horaires = as.factor(data_base$horaires)  #***
  data_base$dep_code = as.factor(data_base$dep_code)
  data_base$ville = as.factor(data_base$ville)
  data_base$epci_code = as.numeric(data_base$epci_code)
  data_base$services_service = as.factor(data_base$services_service) #***
  data_base$dep_name = as.factor(data_base$dep_name)
  
  data_base$com_arm_code = as.factor(data_base$com_arm_code)
  data_base$epci_name = as.factor(data_base$epci_name)
  # data_base$geom : 'geom' variable is a List
  
  data_base$prix_valeur = as.numeric(data_base$prix_valeur)
  data_base$prix_nom = as.factor(data_base$prix_nom)
  data_base$prix_maj = as.Date(data_base$prix_maj)
  data_base$geometry.type = as.factor(data_base$geometry.type)
  # data_base$geometry.coordinates : 'geometry.coordinates' variable is a List
  
  
  # Splitting variables 'lists'***  into multiple columns --------------------
  df_geom <- lapply(data_base$geom, \(x) data.frame(
    geom_lngs = I(list(x[1:(length(x) / 2)])), 
    geom_lats = I(list(x[(length(x) / 2 + 1):length(x)]))))
  df_geom <- do.call('rbind', df_geom)        
  
  ##
  df_gcoord <- lapply(data_base$geometry.coordinates, \(x) data.frame(
    gcoord_lngs = I(list(x[1:(length(x) / 2)])), 
    gcoord_lats = I(list(x[(length(x) / 2 + 1):length(x)]))))
  df_gcoord <- do.call('rbind', df_gcoord) 
  
  # Drop 'lists' and ddding new_splits 'columns' to the dataframe 'data_base'
  data_base <- select(data_base, -c(18,22,23))   # Droping list  & columns : 'geom', 'geometry.points', 'geometry.coordinates'
  
  # Adding the new coordinates columns to 'data_base' --> It seems that these 2 tables give the same informations
  data_base = cbind(data_base, df_geom)       # terms  ---> ("geom_lngs","geom_lats")
  data_base = cbind(data_base, df_gcoord)     # terms  ---> ("gcoord_lngs","gcoord_lats")
  
  # formating coordinates
  data_base$gcoord_lngs = as.numeric(data_base$gcoord_lngs)
  data_base$gcoord_lats = as.numeric(data_base$gcoord_lats)
  
  data_base$geom_lngs_lngs = as.numeric(data_base$geom_lngs)
  data_base$geom_lngs_lats = as.numeric(data_base$geom_lats)
  
  #------------------------------------#
  # Here for testing for Outputs in App
  #------------------------------------#
  
  #  Selecting Columns to Display #
  L_dep_code = list(as.factor(as.vector(data_base$dep_code)))
  
  data_base <- data_base %>%
    select("reg_code", "reg_name", "cp", "dep_code", "dep_name", "prix_valeur", "prix_nom", "gcoord_lngs" , "gcoord_lats")
  
  
  ### Suppose to hide the columns with goegraphic coordinates
  
  
  # select("reg_code", "reg_name","automate_24h_24", "com_arm_name", "cp", "dep_code", "ville", "epci_code", 
  #        "dep_name","com_arm_code", "prix_valeur", "prix_nom","prix_maj", "geom_lngs","geom_lats")
  
  
  
  # Exporting data test 
  write_xlsx(data_base, "data_base.xlsx")     # Exporting dataframe into 'Excel file'
  
  #---------------------------------------------------------------------------
  # L_dep_code = list(data_base$dep_code)
  # L_dep_code = as.factor(as.vector(data_base$dep_code))
  # L_dep_code = as.factor(L_dep_code)
  
  #---------------------------------------------------------------------------
  # Type of stations and fuel types
  stations_types = as.data.frame(levels(data_base$prix_nom))
  colnames(stations_types) = "gas_type"
  
  
  
  ##################### Displaying Data Tables ########################
  output$table1 <- renderReactable({ #sortie table df1 en mode rectable
    reactable(data_base,
              compact = TRUE,resizable = TRUE,searchable = TRUE,defaultPageSize = 10, 
              
              # Columns formats
              columns = list(
                # Date = colDef(format = colFormat(date = TRUE, locales = "en-GB")),
                Designation = colDef(footer = "Total"),
                prix_valeur = colDef(footer = JS("function(colInfo) {
                                     var total = 0
                                     colInfo.data.forEach(function(row) {
                                     total += row[colInfo.column.id]})
                                     return total.toFixed(2)+' EUR'}")
                                     
                                     ,format = colFormat(currency = "EUR"))
                
              ),
              
              #theme et sortie du tableau et de la fenetre de recherche
              theme = reactableTheme(searchInputStyle = list(width = "100%"),
                                     headerStyle = list(
                                       "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
                                       "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),borderColor = "#555"
                                     )
              ),
              
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")) # formating footer
    )
    
  }) # Ending renderReactable
  
  
  ##################### Displaying Widgets ####################
  # Displays 'PickerInput'
  filteredData <- reactive({
    if (input$dep_code == "All dep_code") {
      data_base
    } 
    else {
      filter(data_base, dep_code == input$dep_code)
    }
  })
  
  #-----------------------------------------------------------
  # # # displays 'dep_code'
  # output$map_dep_code<- renderUI({
  #   pickerInput("dep_code", label = "Select a Dep Code:",
  #               choices = list("All dep_code", `Dep_code :` = c("23","14")),
  #               options = list(`live-search` = TRUE)
  #   )
  # 
  # 
  # })
  
  # L_dep_code = as.vector(data_base$dep_code)
  # L_dep_code = as.factor(L_dep_code)
  #-------------------------------------------
  # MAP : Displays
  output$map <- renderLeaflet({
    leaflet(filteredData()) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addMarkers(~as.numeric(gcoord_lngs), ~as.numeric(gcoord_lats), 
                 labelOptions = labelOptions(textsize = "12px"),popup =~ dep_code)
  })
  
  #INTERPRETATION : Information with Map during display
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addMarkers(~as.numeric(gcoord_lngs), ~as.numeric(gcoord_lats), 
                 labelOptions = labelOptions(textsize = "12px"),popup = ~dep_code)
  })
  
  
  
  
  
  ##################### Displaying Plots #######################
  
  
  
  ##########################
} # Ending server portion


#----------------------
#     Launching APP
#----------------------
shinyApp(ui = ui, server = server)
