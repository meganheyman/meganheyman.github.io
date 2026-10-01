##R packages that house some functions we use
library(ggplot2)  #to create graphs
library(dplyr)    #to summarize and filter data
library(gridExtra)#to place two ggplot graphs in the same space
library(maps)     #to create a map
library(ggthemes) #colorblind color scales




###ENSURE YOU HAVE LOADED THE DATA TO YOUR COMPUTING ENVIRONMENT FIRST!





###--- Presence of microplastics in oceans over time ---###
## Basic scatterplot
ggplot(data = Microplastics,
       mapping = aes(x = Year, y = Microplastics.Measurement)) +
  geom_point()


## More aesthetic scatterplot
## -updated quantitative variable to be on square-root scale
## -jittered scatter since year is discrete
## -added descriptive graph labels
## -added a smoother to visualize trending
## -remove gray background
Microplastics <- mutate(Microplastics,
                        sqrtMeas = sqrt(Microplastics.Measurement))

ggplot(data = Microplastics,
       mapping = aes(x = Year, y = sqrtMeas)) +   
  geom_jitter(size = 0.5, alpha = 0.5) +
  labs(y = "Square Root (Microplastics (pieces/m^3))",
       title = "Presence of microplastics in oceans") +    
  geom_smooth(se = FALSE, 
              color="purple") +
  theme_bw() 


## Time series graph
avgMicro <- Microplastics |>
  group_by(Year) |>
  summarize(avgMicroplastics = mean(Microplastics.Measurement),
            medMicroplastics = median(Microplastics.Measurement))

## Basic side-by-side compare
## - saves average connected scatter in p1; median in p2
p1 <- ggplot(data = avgMicro,
             mapping = aes(x = Year, y = avgMicroplastics)) +
        geom_line() +
        geom_point()

p2 <- ggplot(data = avgMicro,
             mapping = aes(x = Year, y = medMicroplastics)) +
        geom_line() +
        geom_point()

grid.arrange(p1, p2, ncol = 2)


## Updated side-by-side compare
## - Update the vertical axis range to match across graphs
## - update graph labeling
## - remove gray background
p1 <- ggplot(data = avgMicro,
             mapping = aes(x = Year, y = avgMicroplastics)) +
        geom_line() +
        geom_point() +
        scale_y_continuous(limits = c(0, 2.75)) +
        labs(title = "Presence of microplastics in the ocean:  Average",
             y = "Average Microplastic pieces per m^3") +
        theme_bw()

p2 <- ggplot(data = avgMicro,
             mapping = aes(x = Year, y = medMicroplastics)) +
        geom_line() +
        geom_point() + 
        labs(title = "Presence of microplastics in the ocean:  Median",
             y = "Median Microplastic pieces per m^3") +
        scale_y_continuous(limits = c(0, 2.75)) +
        theme_bw()

grid.arrange(p1, p2, ncol = 2)




###--- Presence of Microplastics in Atlantic Ocean, 2024 ---###

#Look at how many observations exist for each ocean
#Could also create a table(Ocean, Year)
with(Microplastics, table(Ocean))

#Create dataset that just has observations from Atlantic Ocean, 2024
Atlantic24 <- Microplastics |>
  filter(Ocean == "Atlantic Ocean", Year == 2024)

#Get map shapefile info for the world
globe <- map_data("world")

##Basic map
##-creates a graph space displaying land outlines
##-overlays microplastics measurements where they were obtained
##  via latitude and longitude.  Size of point indicates how much microplastic
##-expand_limits() tells R to show the entire globe on map space
ggplot() +
  geom_map(data = globe, 
           mapping = aes(map_id = region),
           map = globe) +
  geom_point(data = Atlantic24,
             aes(x = Longitude..degree.,
                 y = Latitude..degree.,
                 size = Microplastics.Measurement)) +
  expand_limits(x = globe$long, y = globe$lat) 


##Updated Map
##-change coloring of water and land
##-change coloring of points and add transparency
##-set graph limits to "zoom in" on relevant Atlantic Ocean
##-coord_fixed() sets the map to not be deformed
##-updated graph labeling & removed gridlines
ggplot() +
  geom_map(data = globe, 
           mapping = aes(map_id = region),
           map = globe,
           fill = "antiquewhite") +
  geom_point(data = Atlantic24,
             aes(x = Longitude..degree.,
                 y = Latitude..degree.,
                 size = Microplastics.Measurement),
             color="red",
             alpha = 0.25) +
  expand_limits(x = c(-75, 25), y = c(0, 60)) +
  coord_fixed(1.3) +
  labs(size = "Microplastics/m^3",
       title="2024 Microplastic Presence in the Atlantic Ocean") +
  theme_void() +
  theme(panel.background = element_rect("lightskyblue1"),
        legend.key = element_rect(fill = "white"))



  

###--- Presence of Microplastics across oceans --###
#Basic overlaid density plots
ggplot(data = Microplastics,
       mapping = aes(x = Microplastics.Measurement,
                     color = Ocean,
                     fill = Ocean)) +
  geom_density(alpha = 0.25) 



#Updated overlaid denisty plots
##-add line type to not rely on color for distinction
##-update axis limits to "zoom in" on behavior
##-update color scale to be color blind friendly
##-update graph labeling & remove default gray background
ggplot(data = Microplastics,
       mapping = aes(x = Microplastics.Measurement,
                     color = Ocean,
                     fill = Ocean,
                     linetype = Ocean)) +
  geom_density(alpha = 0.25)  +
  scale_x_continuous(limits = c(0, 3)) +
  scale_y_continuous(limits = c(0, 12)) +
  scale_color_colorblind() +
  scale_fill_colorblind() +
  labs(x = "microplastics per m^3",
       title = "Microplastic presence across Oceans",
       subtitle = "Zoomed in view of densities") +
  theme_classic()
  
