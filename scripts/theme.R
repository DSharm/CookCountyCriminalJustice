my_theme <- theme(plot.background = element_rect(fill="#F8F8F8"),
                  plot.title = element_text(family="Tahoma", face="bold", size=14),
                  plot.subtitle = element_text(family="Tahoma",size=10),
                  plot.margin=unit(c(1,1,1,1),"cm"),
                  plot.caption = element_text(family="Tahoma", size=8),
              
                  panel.background = element_rect(fill = "#F8F8F8"),
                  panel.grid.major.y = element_line(color="#717175", size=0.25),
                  panel.grid.minor.y = element_line(color="#717175", size=0.25),
                  panel.grid.minor.x=element_blank(),
                  panel.grid.major.x=element_blank(),
                  
                  legend.background = element_rect(fill="#F8F8F8"),
                  legend.key = element_blank(),
                  legend.title = element_text(family="Tahoma", size=10),
                  legend.text = element_text(family="Tahoma", size=8),
                  
                  axis.ticks.y = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text = element_text(family="Tahoma", size=8),
                  axis.title = element_text(family="Tahoma", size=9),
                  axis.title.x = element_text(margin=margin(t=15)),
                  axis.title.y = element_text(margin=margin(r=15)),
                  
                  text = element_text(family = "Tahoma", size = 7))
