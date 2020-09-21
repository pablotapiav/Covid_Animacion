library(tidyverse)
library(readr)
library(ggplot2)
library(gganimate)
library(babynames)
library(hrbrthemes)
library(gifski)
library(lubridate)


data <- read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto1/Covid-19_std.csv")

data$Fecha <- as.Date(ymd(data$Fecha))

colnames(data)[7] <- "Casos" 


# Aqui seleccionas las columnas que te interesan
don <- data %>% 
  filter(Comuna %in% c("Arauco", "Los Alamos", "Curanilahue", "Canete", "Contulmo", "Lebu"))

# Plot
p <- don %>%
  ggplot( aes(x=Fecha, y= Casos , group = Comuna ,color=Comuna)) +
  geom_line(size = 1.5) +
  geom_point(size = 2) +
  #scale_color_viridis(discrete = TRUE) +
  labs(title ="Casos COVID Acumulados en Provincia de Arauco", 
       subtitle = 'Fecha: {frame_along}', caption = "Realizado el 12-09-20 por @pabloandrestv, Datos extraidos desde Datos-COVID19") +
  geom_text(aes(x = Fecha, label = Comuna), hjust = 0,  size = 3) + 
  theme_ipsum() +
  ylab("Casos COnfirmados") +
  transition_reveal(Fecha)


# Resolucion optimizada para Iphone

animate(p, duration = 25, fps = 5, width = 1792, height = 828, renderer = gifski_renderer(), res = 150)
anim_save("covid_1209_provArauco.gif")




###### Grafico Estatico


data_group <- don %>% 
    group_by(Comuna, Fecha) %>% summarise(Casos = sum(Casos))

ggplot( data_group, aes(x= Fecha, y= Casos, colour= Comuna)) + geom_line(size = 1.5) +
  labs(title ="Casos COVID Acumulados en Provincia de Arauco", 
       subtitle = 'Acumulados al 7-09-2020', caption = "Realizado el 12-09-20 por @pabloandrestv, Datos extraidos desde Datos-COVID19")
  
