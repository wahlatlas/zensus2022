library(tidyverse)
library(scales)
library(ggtext)
library(gridExtra)
library(grid)
library(gridtext)


# https://ergebnisse.zensus2022.de/datenbank/online/table/1000X-2012/
daten2011 <- read.csv2("1000X-2012_de_flat.csv")
daten2011 <- daten2011 |> filter(X1_variable_attribute_label != "Deutschland")

# https://ergebnisse.zensus2022.de/datenbank/online/table/1000A-2027/
daten2022 <- read.csv2("1000A-2027_de_flat.csv")
daten2022 <- daten2022 |> filter(X1_variable_attribute_label != "Deutschland")

df <- rbind(
  daten2011, daten2022
)

daten <- df[,c("X2_variable_attribute_label",
               "X3_variable_attribute_code",
               "value","time")]


data_wide <- pivot_wider(daten, names_from = c("time","X2_variable_attribute_label"),
                          values_from = "value")

# create numeric age column
data_wide <- data_wide |> 
  mutate(tmp = sub("ALTERU01", "0", X3_variable_attribute_code),
         tmp = sub("ALTER100UM", "100", tmp),
         age = as.numeric(sub("ALTER", "", tmp))) |> 
  select(-tmp, -X3_variable_attribute_code)

names(data_wide) <- c("m2011","w2011","m2022","w2022","age")



pyramid_half <- function(data, leftRight, agecol, agebar, agestep, scale, max) {
  
  direction <- ifelse(leftRight=="left", -1, 1)
  dirTickLab <- ifelse(leftRight=="left", 0, 1)
  m_right <- ifelse(leftRight=="left", -2.5, 1)
  m_left  <-  ifelse(leftRight=="left", 1, -2.5)
  age_sym <- ensym(agecol)
  bar_sym <- ensym(agebar)
  step_sym <- ensym(agestep)
  
  ggplot(data=data) + 
  geom_bar( aes(x=!!age_sym, y=direction*!!bar_sym), stat = "identity", 
            width=1, fill="#9dceff") + 
  geom_step(aes(x=!!age_sym, y=direction*!!step_sym), stat = "identity",
            direction="mid", color="red", linewidth=.7) +
  coord_flip() + 
  scale_x_continuous(expand=c(0,0), breaks=seq(0,100,10), 
                     position = ifelse(leftRight=="left", "top", "bottom")) +
  scale_y_continuous(expand=c(0,0), labels=function(x){x/(direction*scale)}, 
                     breaks=seq(0,direction*max,by=direction*max/5), 
                     limits=c(0,direction*max*1.05)) +
  
  annotate("text", y=direction*max/2, x=95, 
           label= ifelse(leftRight=="right", "Frauen", "Männer"), 
           hjust=0.5, size=3.25) + 
  labs(y="Tausend Personen", x=NULL) +
  
  theme_minimal(base_family = "Verdana", base_size = 9) + 
  theme(
    plot.margin=unit(c(1,m_right,1,m_left), "mm"),
    axis.title.x = element_text(hjust=dirTickLab, size=7),
    axis.text.y = element_text(hjust=.5),
    axis.text.y.right = element_text(color="white"),
    axis.ticks.x = element_line(color = "#dedede", linewidth = 0.5),
    
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_line(color = "#000", linewidth = 0.25),
    axis.ticks.y = ggplot2::element_blank()
  )
}


right_plot <- pyramid_half(data_wide, "right", "age", "w2022", "w2011", 1000, 10000)
left_plot  <- pyramid_half(data_wide, "left",  "age", "m2022", "m2011", 1000, 10000) +
                annotate("text", y=-9000, x=63, label= "2022", 
                  hjust=0.5, size=3.25) +
                annotate("text", y=-9000, x=43, label= "2011", 
                  hjust=0.5, size=3.25)

title_grob <- richtext_grob("Altersaufbau der Bevölkerung im Saarland<br><span style='color: #9dceff;'>2022</span> im Vergleich zu <span style='color: red;'>2011</span>",
                            hjust=0, x=0, margin = unit(c(6,6,6,6), "pt"),
                            gp = gpar(fontsize = 9, fontfamily = "Verdana",
                                      color = "#484848",
                                      lineheight = 1.4))

mygridplot <- grid.arrange(grobs = list(left_plot,right_plot), 
                           top = title_grob, 
                           ncol=2)

ggsave("sl2022vgl2011.svg", 
       plot=mygridplot, width=150, height=150, units="mm")

