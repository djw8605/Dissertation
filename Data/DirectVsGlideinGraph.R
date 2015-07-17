
library("ggplot2")
#library("ggthemes")

setwd("~/Dropbox/Dissertation/Document/Data")

  


dir_data = read.csv("Direct-data.csv")
dir_data$type <- "Direct"

bit_data = read.csv("bittorrent-data3.csv")
bit_data$type <- "BitTorrent"
colnames(bit_data)[1] = "Run"
colnames(bit_data)[2] = "Parents"
bit_data$std <- NULL
bit_data$stderr <- NULL

merged = rbind(dir_data, bit_data )

merged <- transform(merged, hour = Average / 3600)

#merged <- transform(merged, mbps = (15273396463/1025^2) / Average)

p <- ggplot(merged, aes(x = Parents, y = hour, group=type, shape=type, colour=type))

#colnames(data)[2] <- "Distinct"
p <- p + geom_point()


#colnames(data)[2] <- "Distinct"
p <- p + geom_point() + geom_smooth(se=FALSE, method="lm") +
  scale_colour_discrete(name="Transfer Method") + 
  scale_shape_discrete(name="Transfer Method") 

#p <- geom_point( alpha = 1, size = 2) +
#geom_smooth() +

p <- p + ggtitle("Transfer Speed vs. Number of Distinct Nodes") + 
ylab("Transfer Speed (Mbps)") +
xlab("Number of Distinct Nodes")


p <- p + theme_bw() + theme(legend.position=c(0.91,0.5), 
                            legend.background = element_rect(fill="gray97", 
                                                             size=.5, 
                                                             linetype="dotted"))
ggsave("CombinedPlot.pdf", plot = p, width=8, height=3.5)
p


