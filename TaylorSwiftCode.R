
library(ggplot2) #for graphing

###ENSURE YOU HAVE LOADED THE DATA TO YOUR COMPUTING ENVIRONMENT FIRST!


################################
### Side-by-side boxplots
################################
#Default graphic
ggplot(TaylorSwiftSpotify, aes(x = danceability, y = album_name)) + 
  geom_boxplot()


#album order by original release (not Taylor's Version release)
album_order <- rev(c("Taylor Swift", 
                 "Fearless (Taylor's Version)",
                 "Speak Now (Taylor's Version)", 
                 "Red (Taylor's Version)",
                 "1989 (Taylor's Version)",
                 "reputation", 
                 "Lover",
                 "folklore",
                 "evermore",
                 "Midnights",
                 "THE TORTURED POETS DEPARTMENT",
                 "The Life of a Showgirl"))

#set the dataset variable to be an ordered factor
TaylorSwiftSpotify$album_name <- factor(TaylorSwiftSpotify$album_name, 
                                        levels=album_order)

#Updated, aesthetic graph
ggplot(TaylorSwiftSpotify, aes(x = danceability, y = album_name)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(
    color = "slateblue", 
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
    size = 1,
    alpha = 0.75
  ) +
  xlim(c(0, 1)) +
  theme_classic() +
  theme(axis.title.y = element_blank())


################################
### Overlaid density graphs
################################
#Default graph
ggplot(TaylorSwiftSpotify, aes(x = danceability, fill = mode_name,
                   color=mode_name)) + 
  geom_density(alpha = 0.4, linewidth = 0.8)

#Updated, aesthetic graph
ggplot(TaylorSwiftSpotify, aes(x = danceability, fill = mode_name,
                   color=mode_name)) + 
  geom_density(alpha = 0.4, linewidth = 0.8) +
  scale_fill_manual(values = c("major" = "slateblue4", "minor" = "gray")) +
  scale_color_manual(values = c("major" = "slateblue", "minor" = "lightgray")) +
  xlim(c(0, 1)) +
  theme_classic() +
  theme(legend.title = element_blank())





################################
### Scatterplot
################################

#default graph
ggplot(TaylorSwiftSpotify, aes(y = danceability, x=track_number)) + 
  geom_point() 

#updated, aesthetic graph
ggplot(TaylorSwiftSpotify, aes(y = danceability, x=track_number)) + 
  geom_point(color="darkgray") +
  geom_jitter(color="darkgray") +
  labs(x = "Track Number") +
  scale_x_continuous(breaks=seq(1, 31, by=3)) +
  geom_smooth(method="lm", se=FALSE, color="slateblue") +
  theme_bw() 



 
