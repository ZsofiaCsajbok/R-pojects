library(ggplot2)
library(tidyr)
library(dplyr)
library(lessR)
library(plyr)

couples <- read.csv(file = "F:/A Python/Couples Depression/R figure/couples_depr_classes_centered.csv",h=T, sep = ";",dec = ",")
couples_short <- couples %>% select(simple_ID, C_4classes, w1_p1_eurod_computed_1, w2_p1_eurod_computed_2, w4_p1_eurod_computed_4, w5_p1_eurod_computed_5,w6_p1_eurod_computed_6,w7_p1_eurod_computed_7,w1_p2_eurod_computed_1, w2_p2_eurod_computed_2, w4_p2_eurod_computed_4, w5_p2_eurod_computed_5,w6_p2_eurod_computed_6,w7_p2_eurod_computed_7)
couples_short_longformat <- gather(couples_short, wavesex, eurod, w1_p1_eurod_computed_1:w7_p2_eurod_computed_7, factor_key = T)

str(couples_short_longformat)

couples_short_longformat_separated <- separate(couples_short_longformat, col=wavesex, into=c("wave", "sex", "extra1", "extra2", "extra3"), sep='_')
couples_short_longformat_separated_shortened <- couples_short_longformat_separated %>% select(simple_ID, C_4classes, wave, sex, eurod)
str(couples_short_longformat_separated_shortened)
couples_short_longformat_separated_shortened$wave <- as.factor(couples_short_longformat_separated_shortened$wave)
couples_short_longformat_separated_shortened$sex <- as.factor(couples_short_longformat_separated_shortened$sex)

couples_recoded <- couples_short_longformat_separated_shortened
str(couples_recoded)
couples_recoded$sex <- recode(couples_recoded$sex, p1="Male", p2="Female")
couples_recoded$waves <- recode(couples_recoded$wave, w1="Wave 1",w2="Wave 2",w4="Wave 4",w5="Wave 5",w6="Wave 6",w7="Wave 7")
couples_recoded$class <- recode(couples_recoded$C_4classes, `1`="Constantly low depressive symptoms", `2`="Decreasing depressive symptoms", `3`="Only the woman has depressive symptoms", `4`="Increasing depressive symptoms")
couples_recoded$class_sex <- paste(couples_recoded$class," (", couples_recoded$sex,")")


plot_couples <- couples_recoded %>%
  group_by(class_sex=as.character(class_sex), waves=as.character(waves)) %>%
  summarise(eurodM = mean(eurod, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = waves,
             y = eurodM,
             group = class_sex,
             colour = class_sex)) + 
  geom_line(size = 1.5) 

plot_couples

plot_couples2  <- plot_couples + xlab("Waves") +
  ylab("Mean depressive symptoms score") +
  scale_y_continuous(breaks = seq(0, 7, 1), limits = c(0, 7))

plot_couples2

plot_couples3  <- plot_couples2 +  
  scale_colour_manual(values = c("#FFCC00",
                                 "#FFFF66",
                                 "#339900",
                                 "#66CC00",
                                 "#000066",
                                 "#3366FF",
                                 "#CC0000",
                                 "#FF0000"), 
                      name = "Depressive trajectories")
plot_couples3 


plot_couples4 <- plot_couples3 +
  theme(axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 13, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(size = 13, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 13, face = "bold"),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5))

plot_couples4

ggsave(file = "F:/GitHub/Couples_Depressive_symptoms_trajectories_per_wave.jpg",
       plot = plot_couples4,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 600)


