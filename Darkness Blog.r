# libraries
library(Cairo)
library(geniusr)
library(ggplot2)
library(tidyverse) 
library(tidytext) 

# Genius API stuff
# https://genius.com/api-clients
# geniusID <- # REDACTED
# geniusSecret <- # REDACTED
# geniusToken <- # REDACTED

genius_token(TRUE)


# I believe in a thing called love
# findable using search
tclSearch <- search_genius(search_term = "I Believe in a thing called love")
tclSearch[[1]][[1]][6]

# Lyrics
thingCalledLoveLyrics <- get_lyrics_id(song_id = 81524)


# Get all songs
search_artist("The Darkness")
songs <- get_artist_songs_df(22667) #22667 is band id


ids <- c(as.character(songs$song_id))

allLyrics <- data.frame()


### hacky solution by god, but somehow it works
### https://stackoverflow.com/a/14749552/10575353
while (length(ids) > 0) {
  for (id in ids) {
    tryCatch({
      allLyrics <- rbind(get_lyrics_id(id), allLyrics)
      successful <- unique(allLyrics$song_id)
      ids <- ids[!ids %in% successful]
      print(paste("done - ", id))
      print(paste("New length is ", length(ids)))
    }, error = function(e){})
  }
}


# Backing up 
allLyrics2 <- allLyrics
# write.csv(file = "allLyrics.csv", allLyrics)
allLyrics <- read.csv("allLyrics.csv", header = T, stringsAsFactors = F)[,-1]


# Add album for each track
allIds <- data.frame(song_id = unique(allLyrics$song_id))
allIds$album <- ""
# cannot do multiple at once, need to do loop again
for (song in allIds$song_id) {
  allIds[match(song,allIds$song_id),2] <- get_song_df(song)[12]
  print(allIds[match(song,allIds$song_id),])
}


# Some NAs - songs with no offical release
head(allIds)
allIds$album[is.na(allIds$album)] <- "Single Only"
head(allIds)

allLyrics2 <- full_join(allLyrics, allIds)


# Analysis ----

# Split every word into one line
allLyricsTokenised <- allLyrics2 %>%
  #word is the new column, line the column to retrieve the information from
  unnest_tokens(word, line)


# Count each word - I guarantee love is top
allLyricsTokenised %>%
  count(word, sort = TRUE)
# Okay it isn't top but let's remove stopwords


tidyLyrics <- allLyricsTokenised %>%
  anti_join(stop_words)


tidyLyrics %>%
  count(word, sort = TRUE)
# Yup, Love is WAYYY out in front 302 - second is "gonna" with 96



# Plot? Plot.
topFew <- tidyLyrics %>%
  group_by(album, word) %>%
  mutate(n = row_number()) %>%
  ungroup()


# Remove extra cols
topFew <- topFew[,c("album", "word", "n")]


topFew <- topFew %>%
  group_by(album, word) %>%
  summarise(n = max(n))%>%
  ungroup()


topFew <- topFew %>% 
  group_by(word) %>%
  mutate(total = sum(n)) %>%
  filter(total >= 40,
         word != "ooh") %>%
  ungroup()


# colours for each album
albumCol <- c("#394887",      # PTL
              "#9e5a47",      # OWT
              "#f9c784",      # Hot cakes
              "#cf57d4",      # Last
              "#e8b0a5",      # PINE
              "#d18943",      # Easter
              "#4C1A57")      # singles
names(albumCol) <- c("Permission to Land", "One Way Ticket to Hell... and Back",
                     "Hot Cakes", "Last of Our Kind", "Pinewood Smile", "Easter Is Cancelled",
                     "Single Only")

# This ensures bars are stacked in order of release date
# https://stackoverflow.com/questions/33538059/how-to-change-stacking-order-in-stacked-bar-chart-in-r
topFew$album <- factor(topFew$album, levels = c("Single Only",
                                                "Easter Is Cancelled",
                                                "Pinewood Smile", 
                                                "Last of Our Kind", 
                                                "Hot Cakes", 
                                                "One Way Ticket to Hell... and Back",
                                                "Permission to Land"
))

# New christmas thing out since first made this. Just remove it
topFew <- topFew[topFew$album != "Xmas In Rock",]

# ggplot ----
wordsPlot <- ggplot(topFew) +
  
  geom_bar(aes(x = reorder(word, total), 
               y = n,
               fill = as.factor(album)),
           colour = "black",
           stat = "identity") +
  
  coord_flip() +
  
  labs(title = "The Darkness' most used words",
       subtitle = "The words that appear more than 40 times in The Darkness' catalogue",
       caption = "Source: genius.com | by @Statnamara",
       y = "Number of appearances",
       x = "Word",
       fill = "Album")+
  
  scale_fill_manual(values = albumCol) +
  
  theme(title = element_text(face = "italic", size = 12), 
        
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.background = element_rect(colour = "black", fill = "white"),
        panel.grid.major.x = element_line(colour="grey90",size = 1, linetype = 4),
        
        axis.title = element_text(face = "italic",size = 11, colour = "black"),
        axis.ticks.length = unit(5, units = "pt"),
        
        legend.background = NULL,
        legend.position = "top",
        legend.key.size = unit(12,"pt"),
        legend.box.spacing = unit(5,"pt"),
        legend.text = element_text(size = 12),
        
        axis.text.y = element_text(size = 12))

wordsPlot

# ggsave(filename = "DarknessWords.png", plot = wordsPlot, width = 30, height = 24, units = "cm",
       # type = "cairo")


# Add Darkness Logo to the plot
# https://themockup.blog/posts/2019-01-09-add-a-logo-to-your-plot/
library(magick)
logo <- image_read("darkness.png")
logo <- logo %>%
  image_resize("1000x1000")


wordsPic <- image_read("DarknessWords.png")


withLogo <- wordsPic %>%
  image_composite(logo, offset = "+2400+1800")

# image_write(withLogo, "wordsWithLogo.png")



# Sentiment analysis
darknessSentiments <- tidyLyrics %>%
  inner_join(get_sentiments("bing"))%>% 
  count(album, song_name, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

darknessSentiments <- darknessSentiments[darknessSentiments$album != "Xmas In Rock",]
darknessSentiments <- darknessSentiments[!grepl("The Darkness", darknessSentiments$song_name),] # remove the two that contain "By the darkness" in their title




darknessSentiments$album <- factor(darknessSentiments$album, 
                                   levels = c("Permission to Land",
                                              "One Way Ticket to Hell... and Back",
                                              "Hot Cakes", 
                                              "Last of Our Kind", 
                                              "Pinewood Smile", 
                                              "Easter Is Cancelled",
                                              "Single Only"
                                   ))


# sent plot
sentPlot <- ggplot(darknessSentiments,
                   aes(x = reorder(song_name, sentiment), 
                       y = sentiment, 
                       fill = album)) +
  
  geom_col(show.legend = FALSE) +
  
  facet_wrap(~album, 
             ncol = 3, 
             scales = "free")+
  
  scale_fill_manual(values = albumCol)+
  
  labs(title = "The Darkness' songs ranked by sentiment",
       caption = "Source: genius.com | by @Statnamara",
       y = "Sentiment score",
       fill = "Album")+
  
  theme(title = element_text(face = "italic", size = 12), 
        
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.background = element_rect(colour = "black", fill = "white"),
        panel.grid.major.x = element_line(colour="grey90",size = 1, linetype = 4),
        
        axis.title.x = element_text(face = "italic",size = 11, colour = "black"),
        axis.title.y = element_blank(),
        axis.ticks.length = unit(5, units = "pt"),
        
        legend.background = NULL,
        legend.position = "top",
        legend.key.size = unit(12,"pt"),
        legend.box.spacing = unit(5,"pt")) +
  
  coord_flip()

sentPlot

# ggsave(filename = "DarknessSentiment.png", plot = sentPlot, width = 36, height = 24, units = "cm",
       # type = "cairo")


# Add logo
sentPic <- image_read("DarknessSentiment.png")


sentWithLogo <- sentPic %>%
  image_composite(logo, offset = "+2400+1800")
sentWithLogo

# image_write(sentWithLogo, "sentWithLogo.png")

