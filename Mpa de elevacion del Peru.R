#  Cargamos las Librerias ----------------------------------------------------------
library(sf)
library(ggplot2)
library(tidyverse)
library(ggnewscale)
library(raster)
library(extrafont)      # custom font
library(hrbrthemes)     # to use import_roboto_condensed()
library(ggthemes)
library(elevatr)
library(ggspatial)
library(tmap)
library(ggpubr)
# Cargammos los SHp del Peru ---------------------------------------------------------------
Bolivia           <- getData('GADM', country='Bolivia', level=0) %>% st_as_sf()
Brazil             <- getData('GADM', country='Brazil', level=0) %>% st_as_sf()
Chile              <- getData('GADM', country='Chile', level=0) %>% st_as_sf()
Ecuador            <- getData('GADM', country='Ecuador', level=0) %>% st_as_sf()
Peru          <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Pais_Elev     <- getData('alt', country='Peru')
Per           <- getData('GADM', country='Peru', level=2) %>% st_as_sf()
Cusco         <- subset(Per, NAME_1  == "Cusco")
Cusco_xy      <- cbind(Cusco, st_coordinates(st_centroid(Cusco$geometry)))

Geo_data       <-  rasterToPoints(Pais_Elev)
Geo_data_frame <-  data.frame(Geo_data)

Prec_Cusco     <- crop(PPAnual_Per, Cusco)
Prec_Cusco     <- Prec_Cusco<- mask(Prec_Cusco,Cusco)
Geo_dat       <-  rasterToPoints(Prec_Cusco)
Geo_data_fram <-  data.frame(Geo_dat)

colores<- c("#015B26", "#3F8433", "#8BAB56", "#CFCF7D", "#C49450", "#AA5B32", "#872921")

Per=ggplot()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = PER_msk_alt) , show.legend = F)+
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 9, name = "Greys"), 
                       na.value = 'white')+
  geom_sf(data = Peru, fill=NA, color="black")+
  geom_sf(data=Bolivia, fill=NA, color="black", size=0.5)+
  geom_sf(data=Brazil, fill=NA, color="black", size=0.5)+
  geom_sf(data=Chile, fill=NA, color="black", size=0.5)+
  geom_sf(data=Ecuador, fill=NA, color="black", size=0.5)+
  coord_sf(xlim = c(-81.3307, -67), ylim = c(-18.3518 ,-0.03747),expand = FALSE)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -80, y = -1, hjust = 0, vjust = 1, 
           label = "Ecuador",size = 3, family="serif", color = "black",  fontface="italic")+
  annotate(geom = "text", x = -68, y = -15, hjust = 0, vjust = 1, angle = 90,
           label = "Bolivia",size = 3, family="serif", color = "black",  fontface="italic")+
  annotate(geom = "text", x = -70, y = -8, hjust = 0, vjust = 1, 
           label = "Brasil",size = 3, family="serif", color = "black",  fontface="italic")+
  annotate(geom = "text", x = -73, y = -1, hjust = 0, vjust = 1, 
           label = "Colombia",size = 3, family="serif", color = "black",  fontface="italic")

Peru.grob  <- ggplotGrob(Per)

Mapa= ggplot()+
  geom_sf(data=Bolivia, fill=NA, color="white", size=0.5)+
  geom_sf(data=Brazil, fill=NA, color="white", size=0.5)+
  geom_sf(data=Chile, fill=NA, color="white", size=0.5)+
  geom_sf(data=Ecuador, fill=NA, color="white", size=0.5)+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = PER_msk_alt))+
  scale_fill_gradientn(colours = colores, 
                       breaks = c(0,500,1000,2000,3000,4000,5000,6000),
                       na.value = 'white',
                       labels = c("[0 - 499] ","[500 - 999]", "[1000 - 1999]", "[2000 - 2999]", "[3000 - 3999]", "[4000 - 4999]",
                                  "[5000 - 5999]","[6000 - 6543]"),
                       name='Elevacion \n(msnm)')+
  geom_sf(data = Peru, fill=NA, color="black")+
  coord_sf(xlim = c(-82, -70), ylim = c(-13 ,-5))+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  theme_bw()+
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = gray(.4),
                                        linetype = "dashed", size = 0.4),
        axis.text.x  = element_text(face="bold", color="white", size=8),
        axis.text.y  = element_text(angle = 90,face="bold", color="white", size=8),
        legend.position = c(0.95,0.8),
        plot.title = element_text(size = 16, hjust = 0.5, color = "white", family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = "italic", color = "white", family="serif"),
        plot.caption = element_text(size = 10, hjust = 0.95, color = "white", family="serif", face = "italic"),
        legend.key.size = unit(0.4, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.7,"cm"), #ancho de cuadrados de referencia 
        legend.direction = "horizontal", #dirección de la leyenda
        legend.title=element_text(size=8, face = "bold"), #tamaño de titulo de leyenda
        text = element_text(size = 9, family = "Tahoma", color="black"),
        axis.title = element_text(face="bold", color="white"),
        legend.text=element_text(size=8))+
  annotate(geom = "text", x = -80, y = -1, hjust = 0, vjust = 1, 
           label = "Ecuador",size = 3, family="serif", color = "white",  fontface="italic")+
  annotate(geom = "text", x = -68, y = -15, hjust = 0, vjust = 1, angle = 90,
           label = "Bolivia",size = 3, family="serif", color = "white",  fontface="italic")+
  annotate(geom = "text", x = -70, y = -8, hjust = 0, vjust = 1, 
           label = "Brasil",size = 5, family="serif", color = "white",  fontface="italic")+
  annotate(geom = "text", x = -73, y = -1, hjust = 0, vjust = 1, 
           label = "Colombia",size = 3, family="serif", color = "white",  fontface="italic")+
  annotate(geom = "text", x = -78.2, y = -12.2, hjust = 0, vjust = 1, angle = 90,
           label = "Oceano Pacifico",size = 4, family="serif", color = "white",  fontface="italic")+
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  ggtitle("Mapa de Relieve del Peru")+
  labs(subtitle="Ing.Gorky Florez Castillo",  x="Longitud",y="Latitud",tag="B)",
       caption="Fuente: Data: https://srtm.csi.cgiar.org/")+
  annotation_custom(Peru.grob, xmin = -82, xmax = -79, ymin =-13, ymax=-9)


ggsave(plot = Mapa ,"Mapa/Mapa elevacion peru.png", 
       units = "cm", width = 29,height = 21, dpi = 1000) 


