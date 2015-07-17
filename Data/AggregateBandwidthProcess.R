
library(XLConnect)

setwd("~/Dropbox/Dissertation/Document/Data")

wk = loadWorkbook("AggregateBandwidth.xls")

campusdirect = readWorksheet(wk, sheet="Campus Direct")
campusdirect$type <- "Direct"
campusbt = readWorksheet(wk, sheet="Campus BitTorrent")
campusbt$type <- "BitTorrent"

osghttp = readWorksheet(wk, sheet="OSG HTTP Caching")
osghttp$type <- "HTTP Caching"
osgbt = readWorksheet(wk, sheet="OSG BitTorrent")
osgbt$type <- "BitTorrent"

campusmerged <- rbind(campusdirect, campusbt)
osgmerged <- rbind(osghttp, osgbt)


hour_format <- function() {
  function(x) format(x/3600, digits=2)
}

ticmarks <- c(0, 0.5, 1, 1.5, 2.0, 2.5, 3.0) * 3600

p <- ggplot(campusmerged, aes(x=Time, y=Bandwidth, group=type, colour=type)) +
 geom_line(size=1.5) +
  ggtitle("Campus Aggregate Transfer Speed") +
  ylab("Transfer Speed (Gbps)") +
  xlab("Time (hours)") +
  scale_colour_discrete(name="Transfer Method") +
  theme_bw() + 
  scale_x_continuous(labels = hour_format(), breaks=ticmarks) +
  theme(legend.position=c(0.85,0.5), 
        legend.background = element_rect(fill="gray97", 
                                         size=.5, 
                                         linetype="dotted")) +
  geom_abline(intercept = 1, slope = 0)

p
ggsave("~/Dropbox/Dissertation/Document/data/CampusAggregate.pdf", width=5, height=6)

osgp <- ggplot(osgmerged, aes(x=Time, y=Bandwidth, group=type, colour=type)) +
  geom_line(size=1.5) +
  ggtitle("OSG Aggregate Transfer Speed") +
  ylab("Transfer Speed (Gbps)") +
  xlab("Time (s)") +
  theme_bw() + 
  scale_colour_discrete(name="Transfer Method") +
  scale_x_continuous(labels = hour_format(), breaks=ticmarks) +
  theme(legend.position=c(0.85,0.5), 
        legend.background = element_rect(fill="gray97", 
                                         size=.5, 
                                         linetype="dotted")) +
  geom_abline(intercept = 1, slope = 0)

osgp
ggsave("~/Dropbox/Dissertation/Document/data/OSGAggregate.pdf", width=5, height=6)

  #scale_x_continuous(labels = minute_format(), breaks=xbreaks) +
  #scale_y_continuous(labels = minute_format(), breaks=ybreaks) + 
  #scale_y_continuous(breaks=newybreaks) + 
  #ylim(0,10) +
  

#scale_colour_grey(start = .3, end = .7, name="Submission\nMethod") 
#scale_colour_manual(name="Submission\nMethods", values=c("red", "blue"))







