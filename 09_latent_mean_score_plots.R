##########################################
###########09_LATENT_MEANS_Plot###########
##########################################
library('tidyverse')
library('ggplot2')

df2 <- data.frame(retirement_status = c("Long-time workers",
                                        "Soon retired",
                                        "Bridge employment",
                                        "Recently retired",
                                        "Long-time retired"),
                  cm = c(3.74,3.84,3.91,3.7,3.63),
                  ic = c(3.45,3.56,3.64,3.47,3.39),
                  re = c(2.01,1.78,1.77,1.82,1.70),
                  eb = c(3.34,3.42,3.33,3.21,3),
                  ed = c(2.21,2.18,2.22,2.14,2.16))

df2 <- df2 %>%
  mutate(retirement_status = fct_relevel(retirement_status,
                                         "Long-time workers",
                                         "Soon retired",
                                         "Bridge employment",
                                         "Recently retired",
                                         "Long-time retired"))



#### Line plot

ggplot(data=df2, aes(x=retirement_status)) +

  geom_line(aes(y=cm), color = "#438ABA",size=2, alpha=0.9,  group = 1)  +
  geom_point(aes(y=cm), color = "#438ABA", group = 1, size = 4) +
  geom_line(aes(y=ic), color = "#439A7A",size=2, alpha=0.9,  group = 1)  +
  geom_point(aes(y=ic), color = "#439A7A", group = 1, size = 4) +
  geom_line(aes(y=re), color = "#9176BA",size=2, alpha=0.9,  group = 1)  +
  geom_point(aes(y=re), color = "#9176BA", group = 1, size = 4) +
  geom_line(aes(y=eb), color = "#FF9ABA",size=2, alpha=0.9,  group = 1)  +
  geom_point(aes(y=eb), color = "#FF9ABA", group = 1, size = 4) +
  geom_line(aes(y=ed), color = "#DFB246",size=2, alpha=0.9,  group = 1)  +
  geom_point(aes(y=ed), color = "#DFB246", group = 1, size = 4)+
  geom_label( x=5.7, y=1.7, label="Ruminative exploration", size=5, color="#9176BA", label.size = 0, hjust = 0, family = "serif")+
  geom_label( x=5.7, y=2.16, label="Exploration in depth", size=5, color="#DFB246", label.size = 0, hjust = 0, family = "serif")+
  geom_label( x=5.7, y=3.0, label="Exploration in breadth", size=5, color="#FF9ABA", label.size = 0,  hjust = 0, family = "serif")+
  geom_label( x=5.7, y=3.63, label="Commitment making", size=5, color="#438ABA", label.size = 0, hjust = 0, family = "serif")+
  geom_label( x=5.7, y=3.4, label="Identification with commitment", size=5, color="#439A7A", label.size = 0, hjust = 0, family = "serif")+
  xlab("")+
  ylab("")+
  ylim(1,4)+
  theme(
    panel.background = element_rect(fill='transparent'),
    panel.grid.major.y  = element_line(color = "grey",
                                       size = 0.1, linetype = "dotted" ),
    plot.margin =  margin(1,8,1,1, "cm"),
    axis.ticks = element_blank(),
    text=element_text(family="serif")
  )+
  coord_cartesian(clip = "off")



# Clear up environment
rm(list = ls())


