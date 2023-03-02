library(tidyverse)
dta <- readRDS("C:/Users/mgreen/gits/foraging-for-open-science-talk/001-00-e1-data.RDS")
#dta.plot = dta %>% filter(te%in%c("p:10", "d:10"),pp==2)
dta.plot=dta %>% 
  filter(
    (pp==2 & tb %in% c(4,10) & rr=="patchy") | 
    (pp==2 & tb %in% c(5,2) & rr=="dispersed")
    
     )%>% 
  group_by(tb,rr) %>% 
  mutate(plot_group = paste(sep="_", tb,rr),
         xfirst=first(xx),yfirst=first(yy),
         xlast=last(xx),  ylast=last(yy),
         xend=lead(xx),   yend=lead(yy),
         fruitx=ifelse(ll=="fruit",xx,NA),
         fruity=ifelse(ll=="fruit",yy,NA))  %>% 
  mutate(tb=case_when(
    tb==5 ~ 1,
    tb==2 ~ 2,
    tb==10 ~ 1,
    tb==4~ 2
  ))
ggplot(
  data = dta.plot,
  aes(y=yy, 
      x=xx,
      xend=xend,
      yend=yend)
) + 
  theme_bw()+theme(panel.grid=element_blank())+
  xlim(-1920/2,1920/2)+
  ylim(-1080/2,1080/2)+
  coord_fixed()+
  facet_grid(rr~tb)+
  geom_point(aes(x=fruitx, y=fruity, color=rr), size=2)+
  geom_segment(aes(color=rr))+
  scale_colour_manual(values = c("red", "blue"))+
  geom_point(aes(y=yfirst, x=xfirst), size=3, pch=23, color='green', fill='green')
#  geom_point(aes(y=ylast, x=xlast), size=3, color='red')

# 
#   geom_segment(
#     aes(x=xx, 
#         xend=(xx+lead(xx, order_by(plot_group)))/2,
#         y=yy,
#         yend=(yy+lead(yy, order_by(plot_group)))/2),
#     color='blue',
#     arrow=arrow(angle=30, length=unit(8, "points"))
#   ) +
#   geom_segment(
#     aes(x=(xx+lead(xx, order_by(plot_group)))/2, 
#         xend=lead(xx, order_by(plot_group)),
#         y=(yy+lead(yy, order_by(plot_group)))/2,
#         yend=lead(yy, order_by(plot_group))),
#     color='red'
#     ) +
#   geom_point(aes(x=xinit,y=yinit), cex=5, color="green")+
#   geom_point(aes(x=xlast,y=ylast), cex=5, color="yellow")
# #  labs(title="Scanpath for a patchy trial")
# 
# # ,
# #         arrow=arrow(angle = 30, length = unit(0.15, "inches"),
# #                ends = "last", type = "closed"))
