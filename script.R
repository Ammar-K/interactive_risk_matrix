# Load libraries

library(ggplot2)
library(plotly)
library(RColorBrewer)

# Load the data

risk <- read.csv("~/interactive_risk_matrix/risk.csv")

# Creating heatmap background for Risk Matrix

Likelihood <- rep(c(1:5),5)
Consequence <- rep(c(1:5),each=5)
df <- data.frame(Likelihood,Consequence)
df <- mutate(df, Risk = Consequence + Likelihood)


# Create the Color Pallete
myPalette <- colorRampPalette(rev(brewer.pal(11, "RdYlGn")))
risk_p<- ggplot(df,aes(x =Likelihood, y =Consequence, fill=Risk))+
  geom_tile()+
  scale_fill_gradientn(colours = rev(myPalette(10)),guide=FALSE)+
  scale_x_continuous(trans = "reverse",name= "Frequency",breaks = 0:5, expand = c(0, 0))+
  scale_y_continuous(trans = "reverse",name = "Consequence",breaks = 0:5, expand = c(0, 0))+
  coord_fixed()+
  theme_bw()+
  #ggtitle("SAFER per Risk Assessment")+
  theme(legend.position="bottom")+
  guides(color=guide_legend(title="Selected Plants"))+
  geom_jitter(data = risk,
              # position = "jitter",
              inherit.aes = FALSE, width= 0.3,height = 0.3,
              aes(y = Consequence,
                  x = Likelihood, 
                  col = Type,
                  text = paste("<b>ID#:</b>",ID,"<br>",
                               "<b>Risk:</b>",Risk,"<br>",
                               "<b>Type:</b>",Type,"<br>",
                               "<b>Interim Action:</b>",Interim)))+
  scale_color_manual(values = c("#ffa300","#009fdf","#aaaaaa")
  )

config(ggplotly(risk_p,tooltip = "text",width = 650,height = 700), displayModeBar=FALSE, collaborate = FALSE) %>%
  layout(margin=list(l=90,b=50),
         legend = list(orientation = "h",y = -0.15, x = 0))

```
