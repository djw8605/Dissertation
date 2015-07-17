library(ggplot2)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

directlong = "~/Dropbox/Dissertation/Document/Data/output.1800.csv"
glideinlong = "~/Dropbox/Dissertation/Document/Data/output.glidein.1800.csv"

directshort = "~/Dropbox/Dissertation/Document/Data/output.100.csv"
glideinshort = "~/Dropbox/Dissertation/Document/Data/output.glidein.100.csv"

directlongdata = read.csv(directlong)
glideinlongdata = read.csv(glideinlong)

directshortdata = read.csv(directshort)
glideinshortdata = read.csv(glideinshort)

# Long data
directlongdata = directlongdata[, c('time', 'running')]
glideinlongdata = glideinlongdata[, c('time', 'running')]
directlongdata = cbind(method='Direct', directlongdata)
glideinlongdata = cbind(method='Glidein', glideinlongdata)
longdata = rbind(directlongdata, glideinlongdata)

# Short data
directshortdata = directshortdata[, c('time', 'running')]
glideinshortdata = glideinshortdata[, c('time', 'running')]
directshortdata = cbind(method='Direct', directshortdata)
glideinshortdata = cbind(method='Glidein', glideinshortdata)
shortdata = rbind(directshortdata, glideinshortdata)

minute_format <- function() {
  function(x) format(x/60, digits=2)
}

min30times <- c(0, 25, 50, 75, 100) * 60
sec100times <- c(0.0, 5, 10, 15, 20) * 60

p1 <- ggplot(longdata, aes(x=time, y=running, colour=method, group=method)) +
  ggtitle("30-Minute Jobs") +
  geom_line(size=1.5) +
  theme_bw() +
  scale_x_continuous(labels = minute_format(), breaks=min30times) +
  #scale_colour_grey(start = .3, end = .7, name="Submission\nMethod") +
  scale_colour_manual(values = c("red", "blue")) +
  theme(text = element_text(size=15)) +
  xlab("Time (min.)") +
  ylab("Running")

p2 <- ggplot(shortdata, aes(x=time, y=running, colour=method, group=method)) +
  ggtitle("100-Second Jobs") +
  geom_line(size=1.5) +
  theme_bw() +
  #scale_colour_grey(start = .3, end = .7, name="Submission\nMethod") +
  scale_colour_manual(values = c("red", "blue")) + 
  scale_x_continuous(limits = c(0, 1200), labels = minute_format(), breaks=sec100times) +
  theme(text = element_text(size=15)) +
  xlab("Time (min.)") +
  ylab("Running")
  

multiplot(p2, p1)
#library(grid)
#library(gridExtra)
#grid.arrange(p1, p2, ncol=1)

ggsave("~/Dropbox/Dissertation/Document/images/NumberRunning-color.pdf")

