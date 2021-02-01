library(ggplot2)
library(dplyr)
library(ggthemes)

#ECONOMIST DATA VISUALIZATION PROJECT
df <- read.csv("F:/GitHub/Economist_Assignment_Data.csv", header=T)
df$Region <- as.factor(df$Region)
plot1 <- ggplot(df,aes(x=CPI,y=HDI))
print(plot1)
plot2 <- plot1 + geom_point(size=5,aes(color=Region),shape=1) 
print(plot2)
plot3 <- plot2 + geom_smooth(aes(group=1),method='lm', formula=y ~log(x), se=F, color='red')
print(plot3)

pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")
plot4 <- plot3 + geom_text(aes(label=Country), color = "gray20", 
                           data = subset(df, Country %in% pointsToLabel),check_overlap = TRUE) #check_overlap: it will check if the point labels overlap
print(plot4)
plot5 <- plot4 + theme_economist_white()
print(plot5)
plot6 <- plot5 + scale_x_continuous(name="Corruption Perceptions Index, 2011 (10=least corrupt)",
                                    limits=c(1,10), breaks=1:10)
print(plot6)
plot7 <- plot6 + scale_y_continuous(name="Human Development Index, 2011 (1=best)", 
                                    limits=c(0.2,1),
                                    breaks=seq(from=0.2, to = 1, by = 0.1))
print(plot7)
plot8 <- plot7 + ggtitle("Corruption and Human development") 
print(plot8)

ggsave(file = "F:/GitHub/Economist data visualization.jpg",
       plot = plot8,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 600)
