library(ggplot2)
library(plyr)
library(dplyr)
library(extrafont)
library(ggthemes)
library(ggpubr)
library(gridExtra)
loadfonts(device = "win")
windowsFonts(Times = windowsFont("TT Times New Roman"))


mv <- read.csv(file = "K:/GitHub/matevalue3895.csv",h=T, sep = ";",dec = ",")
summary(mv$Age)
is.factor(mv$Sex)
mv$sex <- as.factor(mv$Sex)
mv$sexNames <- revalue(mv$sex, c("1"="Men", "2"="Women"))
is.factor(mv$sexNames)
mv$relship <- as.factor(mv$RelStat)
mv$relshipNames <- revalue(mv$relship, c("1"="Single", "2"="In a relationship"))


#Across sex: short-term mate value
mv.plot.shraw <- mv %>% 
  ggplot(aes(x=Age,
             y=STDESIR,
             group=sexNames)) +
  geom_smooth(aes(linetype = sexNames), color = 'black', method = 'gam', formula = y ~ s(x, bs = "cs"), se=T, size=1.5, alpha=0.5) +
  geom_point(shape=1,size =3, alpha=0.5) +
  xlab("Age") + ylab("Short-term desirability") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(18, 70, 10), limits = c(18, 70)) +
  scale_linetype_manual(values=c("solid","dotted"), name = "Sex") +
  theme_minimal() +
  theme(legend.position="bottom") +
  theme(text=element_text(family="Times", face="bold", size=14),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.title.x=element_blank())

print(mv.plot.shraw)


#Across sex: long-term mate value
mv.plot.loraw <- mv %>% 
  ggplot(aes(x=Age,
             y=LTDESIR)) +
  geom_smooth(aes(linetype = sexNames), color = 'black', method = 'gam', formula = y ~ s(x, bs = "cs", k = 4), se=T, size=1.5, alpha=0.5) +
  geom_point(shape=1,size =3, alpha=0.5) +
  xlab("Age") + ylab("Long-term desirability") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(18, 70, 10), limits = c(18, 70)) +
  scale_linetype_manual(values=c("solid","dotted"), name = "Sex") +
  theme_minimal() +
  theme(legend.position="bottom") +
  theme(text=element_text(family="Times", face="bold", size=14),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.title.x=element_blank())

print(mv.plot.loraw)



#Across relship status: short-term mate value, men
mv.menshort <- mv %>% filter(sex=="1") %>% 
  ggplot(aes(x=Age,
             y=STDESIR,
             group=relshipNames)) +
  geom_smooth(aes(linetype = relshipNames), color = 'black', method = 'gam', formula = y ~ s(x, bs = "cs"), se=T, size=1.5, alpha=0.5) +
  geom_point(shape=1,size =3, alpha=0.5) + 
  xlab("Age") + ylab("Men's short-term desirability") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(18, 70, 10), limits = c(18, 70)) +
  scale_linetype_manual(values=c("solid","dotted"), name = "Relationship status") +
  theme_minimal() +
  theme(legend.position="bottom") +
  theme(text=element_text(family="Times", face="bold", size=14),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.title.x=element_blank())
print(mv.menshort)


#Across relship status: short-term mate value, women
mv.womenshort <- mv %>% filter(sex=="2") %>% 
  ggplot(aes(x=Age,
             y=STDESIR,
             group=relshipNames)) +
  geom_smooth(aes(linetype = relshipNames), color = 'black', method = 'gam', formula = y ~ s(x, bs = "cs"), se=T, size=1.5, alpha=0.5) +
  geom_point(shape=1,size =3, alpha=0.5) + 
  xlab("Age") + ylab("Women's short-term desirability") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(18, 70, 10), limits = c(18, 70)) +
  scale_linetype_manual(values=c("solid","dotted"), name = "Relationship status") +
  theme_minimal() +
  theme(legend.position="bottom") +
  theme(text=element_text(family="Times", face="bold", size=14),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.title.x=element_blank())
print(mv.womenshort)


#Across relship status: Long-term mate value, men
mv.menlong <- mv %>% filter(sex=="1") %>% 
  ggplot(aes(x=Age,
             y=LTDESIR,
             group=relshipNames)) +
  geom_smooth(aes(linetype = relshipNames), color = 'black', method = 'gam', formula = y ~ s(x, bs = "cs"), se=T, size=1.5, alpha=0.5) +
  geom_point(shape=1,size =3, alpha=0.5) + 
  xlab("Age") + ylab("Men's long-term desirability") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(18, 70, 10), limits = c(18, 70)) +
  scale_linetype_manual(values=c("solid","dotted"), name = "Relationship status") +
  theme_minimal() +
  theme(legend.position="bottom") +
  theme(text=element_text(family="Times", face="bold", size=14),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.title.x=element_blank())
print(mv.menlong)


#Across relship status: Long-term mate value, women
mv.womenlong <- mv %>% filter(sex=="2") %>% 
  ggplot(aes(x=Age,
             y=LTDESIR,
             group=relshipNames)) +
  geom_smooth(aes(linetype = relshipNames), color = 'black', method = 'gam', formula = y ~ s(x, bs = "cs"), se=T, size=1.5, alpha=0.5) +
  geom_point(shape=1,size =3, alpha=0.5) + 
  xlab("Age") + ylab("Women's long-term desirability") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(18, 70, 10), limits = c(18, 70)) +
  scale_linetype_manual(values=c("solid","dotted"), name = "Relationship status") +
  theme_minimal() +
  theme(legend.position="bottom") +
  theme(text=element_text(family="Times", face="bold", size=14),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.title.x=element_blank())
print(mv.womenlong)


blankPlot.sex <- ggplot()+geom_blank(aes(1,1)) + 
  cowplot::theme_nothing()
mv.plot.shraw.nl = mv.plot.shraw + theme(legend.position="none")
mv.plot.loraw.nl = mv.plot.loraw + theme(legend.position="none")
legend.sex = get_legend(mv.plot.loraw + theme(legend.position = "bottom",
                                              legend.direction = "horizontal"))
all_sex = grid.arrange(mv.plot.shraw.nl, 
                       mv.plot.loraw.nl,
                       legend.sex,
                       ncol=2, nrow=2,
                       layout_matrix = rbind(c(1, 2), c(3, 3)),
                       widths = c(2.7, 2.7), heights = c(2.5, 0.2))


blankPlot.relship <- ggplot()+geom_blank(aes(1,1)) + 
  cowplot::theme_nothing()
mv.menshort.nl = mv.menshort + theme(legend.position="none")
mv.womenshort.nl = mv.womenshort + theme(legend.position="none")
mv.menlong.nl = mv.menlong + theme(legend.position="none")
mv.womenlong.nl = mv.womenlong + theme(legend.position="none")
legend.relship = get_legend(mv.menshort + theme(legend.position = "bottom",
                                              legend.direction = "horizontal"))
all_relship = grid.arrange(mv.menshort.nl, 
                           mv.womenshort.nl,
                           mv.menlong.nl,
                           mv.womenlong.nl,
                           legend.relship,
                           ncol=2, nrow=3,
                           layout_matrix = rbind(c(1, 2),
                                                 c(3, 4),
                                                 c(5,5)),
                           widths = c(2.7, 2.7), heights = c(2.5, 2.5, 0.2))

ggsave(file = "K:/GitHub/Rplot_shortlong_sex_BW.jpg",
       plot = all_sex,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 600)


ggsave(file = "K:/GitHub/Rplot_shortlong_sexrelship_BW.jpg",
       plot = all_relship,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 600)
