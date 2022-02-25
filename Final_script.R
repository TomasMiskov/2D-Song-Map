## ---------------------------
## Script Name: Spotify_song_map.R
## Author: Tomas Miskov
## Date Created: 2022-02-11
## Purpose: Cluster images using unsupervised machine learning methods
## --------------------------------------------------------------------

#--------
# SET UP |
#--------
rm(list=ls())                                         # clean the environment
if (!require("pacman")) install.packages("pacman")    # install pacman
pacman::p_load(ggplot2, smacof, stats, 
               plotly, cluster)                       # pre-load packages

#-----
# EDA |
#-----
spotifyTrends <- read.csv("spotify_trendsGoogle.csv")
spotifyTrends <- data.frame(rownames(spotifyTrends), spotifyTrends)
spotifyTrends <- spotifyTrends[-1,]
colnames(spotifyTrends) <- c("week", "search_index")
spotifyTrends$week <- as.Date(spotifyTrends$week)
spotifyTrends$search_index <- strtoi(spotifyTrends$search_index)

ggplot(spotifyTrends, aes(x = week, y = search_index)) + 
  geom_line() + scale_x_date(date_breaks = "6 month", date_labels = "%b %Y") + 
  theme_minimal() + theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ylab("Search index") + xlab("Week")

#------
# DATA |
#------
data <- read.csv("top50NL.csv")
data <- data[,-1]
songs <- data$name
songs[c(1,11,7,22,34)] <- c("AMSTERDAM", "L'enfer", "Enemy ft. JID", 
                            "Moth To A Flame", "STAY ft. Bieber")
features <- data[,1:12]
features <- apply(features, 2, function(x) (x - min(x)) / (max(x) - min(x)))
distance <- dist(features, method = "euclidean")

#-----
# MDS |
#-----
set.seed(2022)
iN <- nrow(features)
p <- 2
iter <- 100

# CLASSIC
vStressClassic <- rep(Inf, iter)
mdsModelClassic <- NA
for(i in seq(iter)){
  mdsConf <- mds(distance, ndim = p, init = "random")
  vStressClassic[i] <- mdsConf$stress
  if(vStressClassic[i] == min(vStressClassic)){
    mdsModelClassic <- mdsConf
  }
}
classicShepard <- plot(mdsModelClassic, plot.type = "Shepard")
classicStressHist <- hist(vStressClassic)
classicStressMin <- mdsModelClassic$stress

# ORDINAL
vStressOrdinal <- rep(Inf, iter)
mdsModelOrdinal <- NA
for(i in seq(iter)){
  mdsConf <- mds(distance, ndim = p, init = "random", type = 'ordinal')
  vStressOrdinal[i] <- mdsConf$stress
  if(vStressOrdinal[i] == min(vStressOrdinal)){
    mdsModelOrdinal <- mdsConf
  }
}
ordinalShepard <- plot(mdsModelOrdinal, plot.type = "Shepard")
ordinalStressHist <- hist(vStressOrdinal)
ordinalStressMin <- mdsModelOrdinal$stress

# SPLINE
vStressSpline <- rep(Inf, iter)
mdsModelSpline <- NA
for(i in seq(iter)){
  mdsConf <- mds(distance, ndim = p, init = "random", type = "mspline",
                 spline.degree = 2, spline.intKnots = 2)
  vStressSpline[i] <- mdsConf$stress
  if(vStressSpline[i] == min(vStressSpline)){
    mdsModelSpline <- mdsConf
  }
}
splineShepard <- plot(mdsModelSpline, plot.type = "Shepard")
splineStressHist <- hist(vStressSpline)
splineStressMin <- mdsModelSpline$stress

# COMPARISON PLOT
par(mfrow=c(3,2))
hist(mdsModelClassic$dhat, main = "Classical MDS", 
     xlab = expression(hat(d)))
plot(mdsModelClassic, plot.type = "Shepard", main = "Shepard Diagram (Classical)",
     xlab = paste("Dissimilarities\n Minimal Stress:", round(classicStressMin, 3)))
hist(mdsModelSpline$dhat, main = "Spline MDS", 
     xlab = expression(hat(d)))
plot(mdsModelSpline, plot.type = "Shepard", main = "Shepard Diagram (Spline)",
     xlab = paste("Dissimilarities\n Minimal Stress:", round(splineStressMin, 3)))
hist(mdsModelOrdinal$dhat, main = "Ordinal MDS", 
     xlab = expression(hat(d)))
plot(mdsModelOrdinal, plot.type = "Shepard", main = "Shepard Diagram (Ordinal)",
     xlab = paste("Dissimilarities\n Minimal Stress:", round(ordinalStressMin, 3)))

par(mfrow = c(1,1)) # reset plotting environment

# ORDINAL MDS CONFIGURSTION PLOT
conf <- as.data.frame(cbind(data, mdsModelOrdinal$conf))
ggplot(aes(x = D1, y = D2) , data = conf) +
  geom_point() + 
  geom_text(label = songs, nudge_y = -0.05, check_overlap = T, size = 3) +
  theme_classic() + xlab("") + ylab("")

# SAVE INITIALIZATION FOR THE REPORT
mInit <- mdsModelOrdinal$init
save(mInit, file = "C:/Users/misko/OneDrive/Desktop/BDS/Block 3/Unsupervised Machine Learning/Week 7/UML_Final_Project/init.Rdata")

#------------
# CLUSTERING |
#------------
colors <- c("#4b917d", "#f037a5", "#cdf264", "#121212", "#ffffff")
hcluster <- hclust(distance)
plot(hcluster)
conf["cluster"] <- cutree(hcluster, k = 4)
ggplot(aes(x = D1, y = D2, color = factor(cluster)) , data = conf) +
  geom_point() + 
  geom_text(label = songs, nudge_y = -0.05, check_overlap = T, 
            size = 4, family = "sans", fontface = "bold") +
  theme_classic() + xlab("") + ylab("") + theme(legend.position="bottom") +
  scale_color_manual(labels = c("", "", "", "", ""), 
                     values = colors) + labs(title = "Track Map: Top 50 - Netherlands") +
  theme(plot.title = element_text(color = colors[1], size = 18, face = "bold"),
        panel.background = element_rect(fill = colors[5]),
        plot.background = element_rect(fill = colors[5]),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA))

