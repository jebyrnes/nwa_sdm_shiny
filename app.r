# 1. Preamble
library(shiny)
library(shinythemes)
library(leafem)
library(glue)

library(leaflet)
library(terra)
library(sf)

source("constants.R")

# 2. Define a User Interface
ui <- fluidPage(
  title = "Northwest Atlantic Kelp Species Distribution Models",
  theme = shinytheme("journal"),
  titlePanel("Kelp Species Distribution Models in the Northwest Atlantic"),
  
  tabsetPanel(
    tabPanel("About", "text here"),
    
    # Show raw data
    # Nope - not everyone wants their raw data out there
    #tabPanel("Data Points", leafletOutput("Data")),
   
    # Show predictions
    tabPanel("Distributions", 
             selectInput("species",
                         "Select Species to See Modeled Distribution",
                         c("Alaria esculenta",
                           "Agarum clathratum",
                           "Laminaria digitata",
                           "Saccharina latissima"),
                         selected = "Saccharina latissima"),
             leafletOutput("dist")),
     
    # Show what went in
    tabPanel("Environmental Layers", 
             selectInput("env_layers_input",
                         "Select an Environmental Layer to Explore",
                         choices = names(env_layers),
                         selected = "Longterm Bottom Temp Maximum"),
             
             leafletOutput("env_layers")),
    
 

   # tabPanel("Future Kelp Distribution", leafletOutput("predictions")),
    
    # tabPanel("Distribution Change", 
    #          
    #          selectInput("change_scenario",
    #                      "Select A  Change Scenario",
    #                      c("SSP 126",
    #                        "SSP 245",
    #                        "SSP 370")),
    #          
    #          selectInput("species_change",
    #                      "Select Species to See Its Change",
    #                      c("Alaria esculenta",
    #                        "Agarum clathratum",
    #                        "Laminaria digitata",
    #                        "Saccharina latissima"),
    #                      selected = "Saccharina latissima"),
    #          
    #          
    #          leafletOutput("community_change"))
    
  )
)

# 3. define a server
server <- function(input, output) {
  
  ## for the kelp distribution
  kelp_rast <- reactive({
    kelp_filname <- glue("{toplevel_dir}/{current_dist_dir}/{kelp_layers[[input$species]]}") 
    
    #debug
    print(glue("Loading {kelp_filname}"))
    print(list.files(glue("{toplevel_dir}/{current_dist_dir}")))
    
   r<-  rast(kelp_filname)
   print("Raster Loaded")
   print(r)
   print("Sampling...")
  r <- r|>    spatSample(size = maxcell, 
                 as.raster=TRUE, 
                 warn=TRUE,
                 method = "regular")
    print("success!")
    
    r
  })
  
  output$dist <- renderLeaflet({
    leaflet() |>
     # addProviderTiles(base_tileset) |>
      addTiles() |>
      fitBounds(bbox_4326[1], bbox_4326[2], 
                bbox_4326[3], bbox_4326[4]) |>
      addLegend("bottomright",
                colors = c("Green", "Orange"),
                values = c(0,1),
                labels = c("Present", "Absent"),
                title = "Presence/Abscence",
                opacity = 1
      ) |> #initialize
      addRasterImage(x = kelp_rast(), 
                     opacity = 0.5,
                     method = "ngb")
  })
  
  ## An observe statement to update the dist map
  observe({
    print("updating")
                 
    # here we use leafletProxy()
    leafletProxy(mapId = "dist") |>
      clearShapes() |>
      addRasterImage(x = kelp_rast(), 
                     opacity = 0.5,
                     method = "ngb")
    
  })
  
  # ### Environmental Layers
  env_rast <- reactive({
   r <-  load_resample_rast(
      toplevel_dir,
      env_dist_dir,
      env_layers,
      "env_layers_input",
      input
    )
   r
  })
  
  env_values <- reactive({values(env_rast())})
  

#env_pal <- reactive({
  # make a palette
#  print("Making env palette")
  # env_pal <- colorNumeric(palette = "RdYlBu",
  #                         domain = values(env_rast()),
  #                         na.color = "transparent",
  #                         reverse = TRUE)

 # env_pal
#})
  # 
  # env_values <- reactive({
  #   print("Getting env values...")
  #   values(env_rast())
  #   })
  # 
  # base env layer map
   output$env_layers <- renderLeaflet(
     leaflet() |>
      # addProviderTiles(base_tileset) |>
       addTiles(base_tileset) |>
       fitBounds(bbox_4326[1], bbox_4326[2],
                 bbox_4326[3], bbox_4326[4])
   )

   # An observe statement to update the env map
   observe({
     print("updating env map")
     env_values <- values(env_rast())

     env_pal <- colorNumeric(palette = "RdYlBu",
                             domain = env_values,
                             na.color = "transparent",
                             reverse = TRUE)


     print("Plotting env map")

     # here we use leafletProxy()
     leafletProxy(mapId = "env_layers") |>
       clearShapes() |>
       clearControls() |>
       addRasterImage(x = env_rast(),
                      colors = env_pal,
                      opacity = 0.8,
                      method = "ngb") |>
     addLegend(position = "bottomright",
               values = env_values,
               pal = env_pal,
               title = input$env_layers_input,
               opacity = 1
     )
   })

}

# 4. Call shinyApp() to run your app
shinyApp(ui = ui, server = server)