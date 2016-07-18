#---------------------------------------------------------------------
# Author: Nina Chaichi
# Subject: Cluseter analysis of drone features in the successful and
#          unsuccessful campaign on kickstarter.
#---------------------------------------------------------------------

library(rvest)
library(tm)
library(SnowballC)
library(ggplot2)
library(fpc)
library(cluster)

# successful campaign links.
links <- c("http://www.kickstarter.com/projects/solidifi/streamline-the-ultimate-streaming-drone/description",
           "http://www.kickstarter.com/projects/torquing/zano-autonomous-intelligent-swarming-nano-drone/description",
           "http://www.kickstarter.com/projects/sqdr/hexo-your-autonomous-aerial-camera?ref=category",
           "http://www.kickstarter.com/projects/728836843/codrone-learn-to-code-with-programmable-drone/description",
           "http://www.kickstarter.com/projects/airdog/airdog-worlds-first-auto-follow-action-sports-dron/description",
           "http://www.kickstarter.com/projects/1719668770/cyphy-lvl-1-drone-reinvented-for-performance-and-c/description",
           "http://www.kickstarter.com/projects/137596013/x-plusone-your-ultimate-hover-speed-aerial-camera/description",
           "http://www.kickstarter.com/projects/wiggly/easy-drone-the-first-modular-plug-and-fly-aerial-s/description",
           "http://www.kickstarter.com/projects/wiggly/easy-drone-xl-pro-longest-flying-quadcopter-on-the/description",
           "http://www.kickstarter.com/projects/skyeintel/orbit-the-social-sharing-drone-with-precise-auto-f/description",
           "http://www.kickstarter.com/projects/258964655/ir-lock-infrared-target-tracking-for-drones-and-di/description",
           "http://www.kickstarter.com/projects/2023861406/drone-school-basic-course/description",
           "http://www.kickstarter.com/projects/1950977397/mapping-with-drones/description",
           "http://www.kickstarter.com/projects/1762962374/lego-drone/description"
           )
setwd("D:/Job & Scholarship application/Incubator Data Fellowship/challenge/Kickstarter/texts")

# write the campaign content to text file.
for(i in 1:length(links)){
  a <- read_html(links[i]) %>% html_nodes(".responsive-media p") %>% html_text()
  filename <- paste("outfile_", i, ".txt",sep = "")
  writeLines(a, filename)
}

# clean the text files.
cname <- file.path("D:/Job & Scholarship application/Incubator Data Fellowship/challenge/Kickstarter", "texts")
docs <- Corpus(DirSource(cname))
docs <- tm_map(docs, removePunctuation) 
docs <- tm_map(docs, removeNumbers) 
docs <- tm_map(docs, tolower) 
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, "kickstarter") 
docs <- tm_map(docs, stemDocument) 
docs <- tm_map(docs, stripWhitespace) 
docs <- tm_map(docs, PlainTextDocument)

# create the matrix of desired features.
feature <- inspect(DocumentTermMatrix(docs, 
           list (dictionary = c("water", "camera", "small", "racing",
                        "flying","smart", "control", "phone", "speed"))))

# plot hierarchical cluster
d <- dist(t(feature), method="euclidian")
fit <- hclust(d=d, method="ward")
plot(fit, hang=-1)

# plot K-means Cluster
kfit <- kmeans(d, 3)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0,
         main = "Cluster of Successful Drone Campaign's Features")

# Unsuccessful campaign links.
links <- c("http://www.kickstarter.com/projects/215849561/airstring-gsm-telemetry-for-your-drone?ref=category",
           "http://www.kickstarter.com/projects/2040441468/phenox-2-a-programmable-drone-and-platform?ref=category",
           "http://www.kickstarter.com/projects/951267349/quadcopter-drone-the-tank-quadcopter?ref=category",
           "http://www.kickstarter.com/projects/1435350214/menu-drone-a-menu-search-platform?ref=category",
           "http://www.kickstarter.com/projects/946052725/telaero-drone-flight-planner-for-precision-imagery?ref=category",
           "http://www.kickstarter.com/projects/1406575060/drone-pad-uas-landing-platform-delivery-business-h?ref=category",
           "http://www.kickstarter.com/projects/dronewarfare/how-uav-drones-and-collateral-damage-impact-intern?ref=category",
           "http://www.kickstarter.com/projects/1723580316/affordable-3d-laser-scanner-for-drones-and-ground?ref=category",
           "http://www.kickstarter.com/projects/422909497/lorian-burner-the-adjustable-adaptable-racing-dron?ref=category",
           "http://www.kickstarter.com/projects/spaceonefpv/fpv-racing-drones-by-space-one-fpv?ref=category",
           "http://www.kickstarter.com/projects/1329423847/mind4-worlds-first-android-based-smart-drone?ref=category")

setwd("D:/Job & Scholarship application/Incubator Data Fellowship/challenge/Kickstarter/texts-f")

# write the campaign content to text file.
for(i in 1:length(links)){
  a <- read_html(links[i]) %>% html_nodes(".responsive-media p") %>% html_text()
  filename <- paste("outfile_", i, ".txt",sep = "")
  writeLines(a, filename)
}

# create the matrix of desired features.
cname <- file.path("D:/Job & Scholarship application/Incubator Data Fellowship/challenge/Kickstarter", "texts-f")
docs <- Corpus(DirSource(cname))
docs <- tm_map(docs, removePunctuation) 
docs <- tm_map(docs, removeNumbers) 
docs <- tm_map(docs, tolower) 
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, "kickstarter") 
docs <- tm_map(docs, stemDocument) 
docs <- tm_map(docs, stripWhitespace) 
docs <- tm_map(docs, PlainTextDocument)

# create the matrix of desired features.
feature <- inspect(DocumentTermMatrix(docs, 
           list (dictionary = c("water", "camera", "small", 
            "racing", "flying", "smart", "control", "phone", "speed"))))

# plot hierarchical cluster
d <- dist(t(feature), method="euclidian")
fit <- hclust(d=d, method="ward")
plot(fit, hang=-1)

# plot K-means Cluster
kfit <- kmeans(d, 3)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0,
         main = "Cluster of Unsuccessful Drone Campaign's Features")

