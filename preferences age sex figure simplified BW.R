library(ggplot2)
library(plyr)
library(dplyr)
library(extrafont)
library(ggthemes)
library(ggpubr)
library(gridExtra)
loadfonts(device = "win")
windowsFonts(Times = windowsFont("TT Times New Roman"))

w1 <- read.csv(file = "K:/GitHub/repre_age_2399.csv",h=T, sep = ";",dec = ",")
summary(w1$age)
is.factor(w1$gender)
w1$sex <- as.factor(w1$gender)
w1$sexNames <- revalue(w1$sex, c("1"="Men", "0"="Women"))
is.factor(w1$sexNames)
w1$relship <- as.factor(w1$in_relationship)
w1$relshipNames <- revalue(w1$relship, c("0"="Single", "1"="In a relationship"))


#Across sex: Loving, caring
w1.plot.warmth.sex <- w1 %>% 
  ggplot(aes(x=age,
             y=idealpartner_loving_caring,
             group=sexNames)) +
  stat_smooth(aes(linetype=sexNames),method = 'lm', formula = y ~ x  + I(x^2), se=T, size=1.5, alpha=0.5, color="black") +
  xlab("Age") + ylab("Warm") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(18, 50, 10), limits = c(18, 50)) +
  scale_linetype_manual(values=c("solid","dotted"), name = "Sex") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(text=element_text(family="Times", face="bold", size=14),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.title.x=element_blank())
  
print(w1.plot.warmth.sex)



#Across sex: Attractive, physically attractive
w1.plot.attractiveness.sex <- w1 %>% 
  ggplot(aes(x=age,
             y=idealpartner_attractive_physicallyattracitve,
             group=sexNames)) +
  stat_smooth(aes(linetype=sexNames),method = 'lm', formula = y ~ x  + I(x^2), se=T, size=1.5, alpha=0.5, color="black") +
  xlab("Age") + ylab("Attractive") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(18, 50, 10), limits = c(18, 50)) +
  scale_linetype_manual(values=c("solid","dotted"), name = "Sex") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(text=element_text(family="Times", face="bold", size=14),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.title.x=element_blank())

print(w1.plot.attractiveness.sex)


#Across sex: Good financial, good social position
w1.plot.status.sex <- w1 %>% 
  ggplot(aes(x=age,
             y=idealpartner_goodfinancial_goodsocialposition,
             group=sexNames,
             colour=sexNames)) +
  stat_smooth(aes(linetype=sexNames),method = 'lm', formula = y ~ x  + I(x^2), se=T, size=1.5, alpha=0.5, color="black") +
  xlab("Age") + ylab("Good status") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(18, 50, 10), limits = c(18, 50)) +
  scale_linetype_manual(values=c("solid","dotted"), name = "Sex") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(text=element_text(family="Times", face="bold", size=14),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.title.x=element_blank())

print(w1.plot.status.sex)


#Across sex: Intelligent, educated
w1.plot.intellect.sex <- w1 %>% 
  ggplot(aes(x=age,
             y=idealpartner_intelligent_educated,
             group=sexNames,
             colour=sexNames)) +
  stat_smooth(aes(linetype=sexNames),method = 'lm', formula = y ~ x  + I(x^2), se=T, size=1.5, alpha=0.5, color="black") +
  xlab("Age") + ylab("Intelligent") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(18, 50, 10), limits = c(18, 50)) +
  scale_linetype_manual(values=c("solid","dotted"), name = "Sex") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(text=element_text(family="Times", face="bold", size=14),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.title.x=element_blank())

print(w1.plot.intellect.sex)


#Across sex: Passionate, good in bed
w1.plot.passion.sex <- w1 %>% 
  ggplot(aes(x=age,
             y=idealpartner_passionate_goodinbed,
             group=sexNames,
             colour=sexNames)) +
  stat_smooth(aes(linetype=sexNames),method = 'lm', formula = y ~ x  + I(x^2), se=T, size=1.5, alpha=0.5, color="black") +
  xlab("Age") + ylab("Passionate") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(18, 50, 10), limits = c(18, 50)) +
  scale_linetype_manual(values=c("solid","dotted"), name = "Sex") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(text=element_text(family="Times", face="bold", size=14),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.title.x=element_blank())

print(w1.plot.passion.sex)


#Across sex: Calm, patient
w1.plot.stability.sex <- w1 %>% 
  ggplot(aes(x=age,
             y=idealpartner_calm_patient,
             group=sexNames,
             colour=sexNames)) +
  stat_smooth(aes(linetype=sexNames),method = 'lm', formula = y ~ x  + I(x^2), se=T, size=1.5, alpha=0.5, color="black") +
  xlab("Age") + ylab("Stable") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(18, 50, 10), limits = c(18, 50)) +
  scale_linetype_manual(values=c("solid","dotted"), name = "Sex") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(text=element_text(family="Times", face="bold", size=14),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.title.x=element_blank())

print(w1.plot.stability.sex)


#Across sex: Purposeful, confident
w1.plot.dominant.sex <- w1 %>% 
  ggplot(aes(x=age,
             y=idealpartner_purposeful_confident,
             group=sexNames,
             colour=sexNames)) +
  stat_smooth(aes(linetype=sexNames),method = 'lm', formula = y ~ x  + I(x^2), se=T, size=1.5, alpha=0.5, color="black") +
  xlab("Age") + ylab("Dominant") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(18, 50, 10), limits = c(18, 50)) +
  scale_linetype_manual(values=c("solid","dotted"), name = "Sex") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(text=element_text(family="Times", face="bold", size=14),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.title.x=element_blank())

print(w1.plot.dominant.sex)


#Across sex: Indecisive, unambitious
w1.plot.unambitious.sex <- w1 %>% 
  ggplot(aes(x=age,
             y=idealpartner_indecisive_withoutambition,
             group=sexNames,
             colour=sexNames)) +
  stat_smooth(aes(linetype=sexNames),method = 'lm', formula = y ~ x  + I(x^2), se=T, size=1.5, alpha=0.5, color="black") +
  xlab("Age") + ylab("Unambitious") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(18, 50, 10), limits = c(18, 50)) +
  scale_linetype_manual(values=c("solid","dotted"), name = "Sex") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(text=element_text(family="Times", face="bold", size=14),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.title.x=element_blank())

print(w1.plot.unambitious.sex)


#Across sex: Bad, rude
w1.plot.hostile.sex <- w1 %>% 
  ggplot(aes(x=age,
             y=idealpartner_bad_rude,
             group=sexNames,
             colour=sexNames)) +
  stat_smooth(aes(linetype=sexNames),method = 'lm', formula = y ~ x  + I(x^2), se=T, size=1.5, alpha=0.5, color="black") +
  xlab("Age") + ylab("Hostile") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(18, 50, 10), limits = c(18, 50)) +
  scale_linetype_manual(values=c("solid","dotted"), name = "Sex") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(text=element_text(family="Times", face="bold", size=14),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.title.x=element_blank())

print(w1.plot.hostile.sex)


#Across sex: Messy, careless of hygiene
w1.plot.filthy.sex <- w1 %>% 
  ggplot(aes(x=age,
             y=idealpartner_messy_carelessofhygiene,
             group=sexNames,
             colour=sexNames)) +
  stat_smooth(aes(linetype=sexNames),method = 'lm', formula = y ~ x  + I(x^2), se=T, size=1.5, alpha=0.5, color="black") +
  xlab("Age") + ylab("Filthy") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(18, 50, 10), limits = c(18, 50)) +
  scale_linetype_manual(values=c("solid","dotted"), name = "Sex") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(text=element_text(family="Times", face="bold", size=14),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.title.x=element_blank())

print(w1.plot.filthy.sex)


#Across sex: Selfish, arrogant
w1.plot.arrogant.sex <- w1 %>% 
  ggplot(aes(x=age,
             y=idealpartner_selfish_arrogant,
             group=sexNames,
             colour=sexNames)) +
  stat_smooth(aes(linetype=sexNames),method = 'lm', formula = y ~ x  + I(x^2), se=T, size=1.5, alpha=0.5, color="black") +
  xlab("Age") + ylab("Arrogant") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(18, 50, 10), limits = c(18, 50)) +
  scale_linetype_manual(values=c("solid","dotted"), name = "Sex") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(text=element_text(family="Times", face="bold", size=14),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(size = 13, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 13, face = "bold"),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5))

print(w1.plot.arrogant.sex)


#Across sex: Unattractive, physically unattractive
w1.plot.unattractive.sex <- w1 %>% 
  ggplot(aes(x=age,
             y=idealpartner_unattractive_physicallyunattractive,
             group=sexNames,
             colour=sexNames)) +
  stat_smooth(aes(linetype=sexNames),method = 'lm', formula = y ~ x  + I(x^2), se=T, size=1.5, alpha=0.5, color="black") +
  xlab("Age") + ylab("Unattractive") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(18, 50, 10), limits = c(18, 50)) +
  scale_linetype_manual(values=c("solid","dotted"), name = "Sex") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(text=element_text(family="Times", face="bold", size=14),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(size = 13, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 13, face = "bold"),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5))

print(w1.plot.unattractive.sex)


#Across sex: Emotionally dependent, demanding commitment
w1.plot.clingy.sex <- w1 %>% 
  ggplot(aes(x=age,
             y=idealpartner_emotionallydependent_demandingcommitment,
             group=sexNames,
             colour=sexNames)) +
  stat_smooth(aes(linetype=sexNames),method = 'lm', formula = y ~ x  + I(x^2), se=T, size=1.5, alpha=0.5, color="black") +
  xlab("Age") + ylab("Clingy") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(18, 50, 10), limits = c(18, 50)) +
  scale_linetype_manual(values=c("solid","dotted"), name = "Sex") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(text=element_text(family="Times", face="bold", size=14),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(size = 13, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 13, face = "bold"),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5))

print(w1.plot.clingy.sex)


#Across sex: Aggressive, violent
w1.plot.abusive.sex <- w1 %>% 
  ggplot(aes(x=age,
             y=idealpartner_agressive_violent,
             group=sexNames,
             colour=sexNames)) +
  stat_smooth(aes(linetype=sexNames),method = 'lm', formula = y ~ x  + I(x^2), se=T, size=1.5, alpha=0.5, color="black") +
  xlab("Age") + ylab("Abusive") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(18, 50, 10), limits = c(18, 50)) +
  scale_linetype_manual(values=c("solid","dotted"), name = "Sex") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(text=element_text(family="Times", face="bold", size=14),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(size = 13, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 13, face = "bold"),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5))

print(w1.plot.abusive.sex)


#Across sex: Pessimistic, depressed
w1.plot.depressive.sex <- w1 %>% 
  ggplot(aes(x=age,
             y=idealpartner_pessimistic_depressed,
             group=sexNames,
             colour=sexNames)) +
  stat_smooth(aes(linetype=sexNames),method = 'lm', formula = y ~ x  + I(x^2), se=T, size=1.5, alpha=0.5, color="black") +
  xlab("Age") + ylab("Depressive") +
  scale_y_continuous(breaks = seq(1, 7, 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(18, 50, 10), limits = c(18, 50)) +
  scale_linetype_manual(values=c("solid","dotted"), name = "Sex") +
  theme_minimal() +
  theme(legend.position="bottom") +
  theme(text=element_text(family="Times", face="bold", size=14),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(size = 13, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 13, face = "bold"),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5))

print(w1.plot.depressive.sex)

blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
  cowplot::theme_nothing()
legend = get_legend(w1.plot.depressive.sex)
w1.plot.depressive.sex.nl = w1.plot.depressive.sex + theme(legend.position="none")
all_plots = grid.arrange(w1.plot.warmth.sex, 
                         w1.plot.attractiveness.sex, 
                         w1.plot.status.sex,
                         w1.plot.intellect.sex,
                         w1.plot.passion.sex,
                         w1.plot.stability.sex,
                         w1.plot.dominant.sex,
                         w1.plot.unambitious.sex,
                         w1.plot.hostile.sex,
                         w1.plot.filthy.sex,
                         w1.plot.arrogant.sex,
                         w1.plot.unattractive.sex,
                         w1.plot.clingy.sex,
                         w1.plot.abusive.sex,
                         w1.plot.depressive.sex.nl,
                         blankPlot, blankPlot, legend, ncol=5, nrow=4, heights=c(3,3,3,0.5))



ggsave(file = "K:/GitHub/Rplot_ideal_sex_all15_BW.jpg",
       plot = all_plots,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 600)

