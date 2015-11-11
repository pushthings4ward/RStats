library(ggplot2)
library(grid)

# load data
DWP <- read.csv("YOURDATA.csv", stringsAsFactors=FALSE)

# remove duplicates
DWP <- unique( DWP[ , ] )

# Factor --> Numeric
DWP$impressions <-as.numeric(as.character(DWP[,5]))
DWP$engagements <-as.numeric(as.character(DWP[,6]))
DWP$retweets <-as.numeric(as.character(DWP[,8]))
DWP$url.clicks <-as.numeric(as.character(DWP[,12]))

# select data
dvtv <- subset(DWP, retweets > 0 & retweets < 40 & impressions < 9000)

# grepl query
dvtv <- subset(dvtv, grepl("^2015", time))

# render scatterplot
vis <- ggplot(dvtv, aes(x=retweets, y=impressions)) + 
  geom_point(size=2.5) 

vis +
  theme_bw() +
  
  # Set the entire chart region to a light gray color
  theme(panel.background=element_rect(fill="#F0F0F0")) +
  theme(plot.background=element_rect(fill="#F0F0F0")) +
  theme(panel.border=element_rect(colour="#F0F0F0")) +
  
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  scale_x_continuous(minor_breaks=0,breaks=seq(0,40,2),limits=c(0,40)) +
  scale_y_continuous(minor_breaks=0,breaks=seq(0,10000,1000),limits=c(0,10000)) +
  theme(axis.ticks=element_blank())+
  
  # Big bold line at y=0
  geom_hline(yintercept=0,size=1.2,colour="#535353")+
  
  # Set title and axis labels, and format these and tick marks
  ggtitle("Title Here") +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=16)) +
  ylab("Impressions") +
  xlab("Retweets") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
  
  # Plot margins
  theme(plot.margin = unit(c(1, 1, .5, .7), "cm")) +
  
  # zone description
  annotate("text", x=5, y=9500, label="Zone 1", size=6, family="DIN Alternate", fontface="bold.italic", colour="black") +
  annotate("text", x=15, y=9500, label="Zone 2", size=6, family="DIN Alternate", fontface="bold.italic", colour="black") +
  annotate("text", x=25, y=9500, label="Zone 3", size=6, family="DIN Alternate", fontface="bold.italic", colour="black") +
  annotate("text", x=35, y=9500, label="Zone 4", size=6, family="DIN Alternate", fontface="bold.italic", colour="black") +
  
  # zone
  annotate("rect", xmin=0, xmax=10, ymin=0, ymax=10000, alpha=.1, fill="red") +
  annotate("rect", xmin=10, xmax=20, ymin=0, ymax=10000, alpha=.1, fill="yellow") +
  annotate("rect", xmin=20, xmax=30, ymin=0, ymax=10000, alpha=.1, fill="blue") +
  annotate("rect", xmin=30, xmax=40, ymin=0, ymax=10000, alpha=.1, fill="green") 

# export graphic
ggsave(filename="graphic.png", dpi=300)