#############################################################################################################


#                                     Proyecto Sale Market 


#############################################################################################################


# 1.- Preparación

# 1.1 Librerías

library(dplyr)
library(tidyverse)
library(ggplot2)
library(skimr)
library(visdat)
library(inspectdf)
library(patchwork)
library(psych)
library(lubridate)
library(gridExtra)
library(ggpubr)
library(ggplot2)

# 1.2 Cargar Base de datos

datos<- read.csv("supermarket_sales - Sheet1.csv")

#############################################################################################################

# 2.- Procesamiento de los datos

str(datos)

# Hay variables del dataset que no aportarán valor al estudio por lo cuales no serán considerados.

datos <- datos %>% 
           select(- Invoice.ID)%>%
           select(- Tax.5.)%>%
           select(- cogs)

# Hay variables categóricas que hay que identificar como factor .

  datos$Branch <- as.factor(datos$Branch)
  datos$City<- as.factor(datos$City)
  datos$Customer.type<- as.factor(datos$Customer.type)
  datos$Gender<- as.factor(datos$Gender)
  datos$Product.line<- as.factor(datos$Product.line)
  datos$Quantity<- as.numeric(datos$Quantity)
  datos$Payment<- as.factor(datos$Payment)
  datos$Date <- as.Date(datos$Date, "%m/%d/%y")
  year(datos$Date) <- 2019
  
  
 
# vemos los datos
  
  str(datos)
  
##  Como queremos ver las ventas por mes y por dia de la semana, necesitamos crear las variables "Month" y 
##  Day.
  
 
    datos$Month<- format(as.Date(datos$Date), "%B") 
    datos$Day <- format(as.Date(datos$Date), "%A")
    
    datos <- datos [ order ( datos$Date ) , ] # ordenamos los datos por fecha
    
   
 # volvemos a visualizar
   
     head(datos)
     tail(datos)
    
     
# Renombramos algunas variables para hacerlas más intuitivas
     
 datos <- rename(datos, Customer = Customer.type)
 datos <-  rename(datos, Total.Sales = Total)
 datos <-  rename(datos, Margin = gross.margin.percentage)
 datos <- rename(datos, Income = gross.income)
 
 # Revisamos si hay valores perdidos en el dataset
 
 any(is.na(datos))%>%
   sum(any(is.na(datos)))
 
 ############################################################################################################
 
#3.-  ANALIZAR
 
 # Analisis exploratorio de Datos
 
  summary(datos) # hacemos un resumen general de los datos

  skim(datos) # primera visualizacion de los datos
  
  general <- vis_dat(datos)
  tipos_de_datos <- inspect_types(datos) %>% 
                    show_plot()  # Explorar tipos de datos
  
  inspeccion_categoricos <- inspect_cat(datos) %>% 
                            show_plot() # Explorar datos categoricos
  
  inspeccion_numericos <- inspect_num(datos) %>% 
                          show_plot() # explorar los numericos
  
  plot_resumen <- tipos_de_datos + general +
    labs(title = "Dataset description" )
 
   plot_resumen
  
  
  inspeccion_categoricos 
  inspeccion_numericos    
  
  # Analizando relaciones
  
  relaciones<-select(datos,c("Unit.price","Quantity","Rating","Total.Sales"))

  pairs.panels(relaciones,
               smooth = TRUE,      # Si TRUE, dibuja ajuste suavizados de tipo loess
               scale = FALSE,      # Si TRUE, escala la fuente al grado de correlación
               density = TRUE,     # Si TRUE, añade histogramas y curvas de densidad
               ellipses = TRUE,    # Si TRUE, dibuja elipses
               method = "pearson", # Método de correlación (también "spearman" o "kendall")
               pch = 21,           # Símbolo pch
               lm = FALSE,         # Si TRUE, dibuja un ajuste lineal en lugar de un ajuste LOESS
               cor = TRUE,         # Si TRUE, agrega correlaciones
               jiggle = FALSE,     # Si TRUE, se añade ruido a los datos
               factor = 2,         # Nivel de ruido añadido a los datos
               hist.col = 4,       # Color de los histogramas
               stars = TRUE,       # Si TRUE, agrega el nivel de significación con estrellas
               ci = TRUE)# Si TRUE, añade intervalos de confianza a los ajustes

 
  # Analisis variables categóricas
  
  ## Análisis  de ventas por mes
  
 ## Serie de tiempo
  
  total_sales_per_day <- data.frame(xtabs(formula=Total.Sales~Date, data=datos))
  total_sales_per_day$Date <- as.Date(total_sales_per_day$Date)
  
 timeserie_monthly<-  ggplot(data = total_sales_per_day, mapping = aes(x = Date, y = Freq))+ 
    geom_line(color = "#8B0000", lwd = 1)+
    geom_point(color = "#8B0000")+
    theme_classic()+ 
    ggtitle("Serie de tiempo de ventas primer trimestre")+ 
    xlab("")+ 
    ylab("Ventas totales por dia")
 
 # Conteo ventas por mes
 
 monthly_res <- datos %>%
   group_by(Month)%>%
   summarise ( 'Sales' = sum(Total.Sales),
               'Quantity' = sum(Quantity),
               'Income' = sum(Income),
               'Margin' = mean(Margin))
 monthly_res
 
 f <- ggplot(monthly_res, aes(Month, Sales ))+
   geom_col(color = "black", lwd = 0.5 , fill= c("#FFA500", "#436EEE", "seagreen2"))+
   geom_text(aes(label = c("116292", "97219", "109456")), vjust = 2, colour = "black")+
   theme_classic()+
   ggtitle("Ventas totales mensuales")+ 
   xlab("")+ 
   ylab("Ventas totales por mes")
 
 
 sale_analysis<- grid.arrange(timeserie_monthly, f)
sale_analysis


# SUMA DE ARTICULOS ADQUIRIDOS POR COMPRA
 
  Quantity<- ggplot(monthly_res, aes(Month, Quantity ))+ 
    geom_col(color = "black", lwd = 0.5 , fill= c("#FFA500", "#436EEE", "seagreen2"))+
    geom_text(aes(label = c("1965", "1654", "1891")), vjust = 2, colour = "black")+
    theme_classic()+
    ggtitle("Cantidad de productos vendidos por mes")+ 
    xlab("")+ 
    ylab("Productos vendidos")

  Quantity
  
# Promedio de Ganancias por venta
  
  Income <- ggplot(monthly_res, aes(Month, Income))+
    geom_col(color = "black", lwd = 0.5 , fill= c("#FFA500", "#436EEE", "seagreen2"))+
    geom_text(aes(label = c("5538", "4629", "5212")), vjust = 2, colour = "black")+
    theme_classic()+
    ggtitle("Ganancias por mes")+ 
    xlab("")+ 
    ylab("Ganancias")
  
  Income

  # Promedio de  Marge obtenido  
  
  Margin <- ggplot(monthly_res, aes(Month, Margin))+
    geom_col(color = "black", lwd = 0.5 , fill= c("#FFA500", "#436EEE", "seagreen2"))+
    geom_text(aes(label = c("4.76", "4.76", "4.76")), vjust = 2, colour = "black")+
    theme_classic()+
    ggtitle("Margen mensual")+ 
    xlab("")+ 
    ylab("Margen")

  Margin  
  
 
  # multiplot
  
   plot <- grid.arrange(Quantity, Income, Margin , 
                       ncol = 2, 
                       nrow = 2)
  plot

  
  # Una alternativa incluyendo la tabla 
  
  stable.p <- ggtexttable(monthly_res, rows = NULL, 
                          theme = ttheme("mOrange"))
  
  plot2<- ggarrange(Quantity, Income, Margin, stable.p,
            ncol = 2, nrow = 3,
            heights = c(1, 0.5, 0.3)) 
  plot2
  
  
  # Productos Mas vendidos
  
  #Product Line
  
  products <- datos %>% count(Product.line, sort = TRUE)
  products<- rename(products, Product = Product.line) 
  products<- rename(products, Total = n) 
  products  

  ggplot(products, aes(x = Product, y = Total)) +
    geom_bar(fill = c('#2364aa','#3da5d9','#73bfb8','#fec601','#ea7317','#cd0c2b'),
             stat = "identity") +
    theme_classic()+
    scale_x_discrete(limits = c("Health and beauty" ,"Home and lifestyle", "Sports and travel",
                                "Electronic accessories","Food and beverages", "Fashion accessories"))+
    geom_text(aes(label=Total), vjust =0.8, hjust=1.5, col = "white",
              fontface='bold') +
    labs(title = "Ventas totales de lineas de productos")+
    coord_flip() 
  
  
  ## Analisis Semanal
  
  # resumen
  
  weekly <- datos %>%
    group_by(Day)%>%
    summarise ( 'Sales' = sum(Total.Sales),
                'Quantity' = sum(Quantity),
                'Income' = sum(Income),
                'Margin' = mean(Margin))

weekly <- weekly[ c(1, 3, 4, 5, 2, 7, 6),] # ordenamos las filas
weekly  


# visualizamos

#Ventas

plotweek<- ggplot(weekly, aes(x = Day, y = Sales)) +
  geom_col(fill = c('#2364aa','#3da5d9','#66CD00','#fec601','#ea7317','#cd0c2b', "#9932CC"), 
           color = "black") +
  geom_text(aes(label= c("44458", "37899", "51482", "43731", "45349", "43926", "56121")),
            vjust = 5, hjust=0.5, col = "white", fontface='bold')+
  labs( x = "", y = "Ventas totales", title = "Ventas semanales",
        subtitle = "Total de ventas por dia de la semana")+
             theme_classic()
           
   plotweek        
   
#Cantidad de productos vendidos
   
plotcantidad<- ggplot(weekly, aes(x = Day, y = Quantity)) +
     geom_col(fill = c('#2364aa','#3da5d9','#66CD00','#fec601','#ea7317','#cd0c2b', "#9932CC"), 
              color = "black") +
     geom_text(aes(label= c("778", "638", "862", "800", "755", "758", "919")),
               vjust = 5, hjust=0.5, col = "white", fontface='bold')+
     labs( x = "", y = "Cantidad total", title = " Cantidad total vendida",
           subtitle = "Productos totales vendidos por día de la semana")+
     theme_classic()
   
   plotcantidad   
   
# Ganancias 

plotganancia<- ggplot(weekly, aes(x = Day, y = Income)) +
     geom_col(fill = c('#2364aa','#3da5d9','#66CD00','#fec601','#ea7317','#cd0c2b', "#9932CC"), 
              color = "black") +
     geom_text(aes(label= c("15.9", "14.4", "15.5", "14.6", "15.6", "15.0", "16.3")),
               vjust = 5, hjust=0.5, col = "white", fontface='bold')+
     labs( x = "", y = "Ganancia total", title = " Ganancia semanal",
           subtitle = "Ganancia total por dia de la semana")+
     theme_classic()
   
   plotganancia
   
 # Margin

   # velocimetro

    gg.gauge <- function(pos,breaks=c(0,10,20,100)) {
    require(ggplot2)
    get.poly <- function(a,b,r1=0.5,r2=1.0) {
      th.start <- pi*(1-a/100)
      th.end   <- pi*(1-b/100)
      th       <- seq(th.start,th.end,length=100)
      x        <- c(r1*cos(th),rev(r2*cos(th)))
      y        <- c(r1*sin(th),rev(r2*sin(th)))
      return(data.frame(x,y))
    }
    ggplot()+ 
      geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="red")+
      geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="gold")+
      geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="forestgreen")+
      geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y))+
      geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
                aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"%")))+
      annotate("text",x=0,y=0,label=pos,vjust=0,size=8,fontface="bold")+
      labs(title = "Margen", subtitle = "Margen obtenido en la semana")+
      coord_fixed()+
      theme_bw()+
      theme(axis.text=element_blank(),
            axis.title=element_blank(),
            axis.ticks=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank()) 
  }
 gauge <- gg.gauge(4.76,breaks=c(0,10,20,100))
  gauge
  
  # multiplot

  plot3 <- grid.arrange(plotweek, plotcantidad , plotganancia, gauge,
                       ncol = 2, 
                       nrow = 2)
  plot3
  
  # Analisis por ciudad
 
  ciudad <- datos %>%
            group_by(City)%>%
    summarise ( 'Sales' = sum(Total.Sales),
                'Quantity' = sum(Quantity),
                'Income' = sum(Income),
                'Margin' = mean(Margin))
  ciudad
  
 
  # Ventas
  
  plotcity1<- ggplot(ciudad, aes(x = City, y = Sales)) +
    geom_col(fill = c('#00008B', '#CD3333', '#76EE00'), 
             color = "black") +
    geom_text(aes(label= c('106198', '110569', '106200')),
              vjust = 5, hjust=0.5, col = "white", fontface='bold')+
    labs( x = "", y = "Ventas totales", title = "Ventas por ciudad trimestral",
          subtitle = "Total de ventas por ciudad")+
    scale_y_continuous( breaks = c(0, 30000, 60000, 90000, 110000))+
    theme_classic()
  
  plotcity1       
   
 # visitantes
 
  visitantes <- datos %>%
                group_by(City)%>%
                count(City, sort=TRUE) 
  visitantes

  plotvisit <- ggplot(visitantes, aes(City, n, fill = City)) +  
  geom_col()+
    scale_fill_manual(values = c('#00008B', '#CD3333', '#76EE00'))+
    geom_text(aes(label = c('340', '332', '328')),
              vjust = 5, hjust=0.5, col = "white", fontface='bold')+
    labs( x = "", y = "Visitantes",
          title = "Visitantes por ciudad primer trimestre",
          subtitle = "Clientes que visitan los supermecados de la cadena por ciudad")+
    theme_classic()  
  plotvisit
  
  
  #Cantidad de productos vendidos
  
  plotcantidad2<- ggplot(ciudad, aes(x = City, y = Quantity)) +
    geom_col(fill = c('#00008B', '#CD3333', '#76EE00'), 
             color = "black") +
    geom_text(aes(label= c('1820', '1831', '1859')),
              vjust = 5, hjust=0.5, col = "white", fontface='bold')+
    labs( x = "", y = "Cantidad total", title = " Cantidad total vendida",
          subtitle = "Productos totales vendidos por ciudad")+
    theme_classic()
  
  plotcantidad2  
  
  # Ganancias 
  
  plotganancia2<- ggplot(ciudad, aes(x = City, y = Income)) +
    geom_col(fill = c('#00008B', '#CD3333', '#76EE00'), 
             color = "black") +
    geom_text(aes(label= c('5057', '5265', '5057')),
              vjust = 5, hjust=0.5, col = "white", fontface='bold')+
    labs( x = "", y = "Ganancia total", title = " Ganancia semanal",
          subtitle = "Ganancia total por ciudad")+
    theme_classic()
  
  plotganancia2
  
 # Multiplot
  
  plot4 <- grid.arrange(plotcity1, plotvisit, plotcantidad2 , plotganancia2,
                        ncol = 2, 
                        nrow = 2)
  plot4
  
  
  # Analisis por sucursal 

  sucursal <- datos %>%
    group_by(Branch)%>%
    summarise ( 'Sales' = sum(Total.Sales),
                'Quantity' = sum(Quantity),
                'Income' = sum(Income),
                'Margin' = mean(Margin),
                'Rating' = mean(Rating))
  sucursal


  # Plot Rating
  
  ggplot(datos, aes(x = Branch, y = Rating, fill = Branch))+
    geom_violin(trim = F)+
    geom_boxplot(width=0.4)+
    labs(title="Satisfacción del cliente por sucursal",
                                 fill="Kota")+
    xlab("Sucursal")+
    ylab("Calificación de satisfacción")+
    scale_fill_brewer(palette = "Paired")+
    stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black")
  
  
  # Clientes basado en género

  p<- ggplot(data = datos, mapping = aes(x = Branch)) + 
    geom_bar(mapping = aes(fill = Customer)) + 
    theme_linedraw() + 
    ggtitle("Distribución de clientes por sucursal basado en el Genéro") + 
    xlab("Sucursal") + ylab("Número de clientes") 
  p + facet_wrap(datos$Gender) + 
    scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150, 175)) + 
    labs(caption = "Mujeres (Miembros) - 80/85/96 \n Hombres (Miembros) - 87/80/73")
  
  
 # Clientes basado en el método de pago
  
  g <- ggplot(data = datos, mapping = aes(x = Branch)) + 
    geom_bar(mapping = aes(fill = Customer)) + 
    theme_linedraw() + 
    ggtitle("Distribución de Clientes por sucursal basado en el método de pago")+ 
    xlab("Sucursal") +
    ylab("Número de clientes") 
  g + facet_wrap(datos$Payment) + 
    scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150, 175)) + 
    labs(caption = "Contado (Miembros) - 56/53/59 \n Tarjeta de crédito (Miembros) - 49/63/60 \n Pago electrónico (Miembros) - 62/49/50")
  
  # Ventas por sucursal
  
  sucursal <- datos %>%
    group_by(Branch)%>%
    summarise ( 'Sales' = sum(Total.Sales),
                'Quantity' = sum(Quantity),
                'Income' = sum(Income),
                'Margin' = mean(Margin),
                'Rating' = mean(Rating))
  sucursal
  
  
  # Plot

  p1 <- ggplot(data = sucursal, mapping = aes(x = Branch, y = Sales)) + 
    geom_col(mapping = aes(fill = Branch), show.legend = FALSE) +
    geom_text(aes(label= c('106200', '106198', '110569')),
              vjust = 5, hjust = 0.5, col = "black", fontface='bold')+
    theme_classic() + 
    ggtitle("Ventas por sucursal") + 
    xlab("Sucursal") + ylab("Ventas Totales")
  p1
  
  
  # Análisis de Tipo de Cliente
  
# Métodos de Pago

  pay1<-sum(datos$Payment=="Cash")
  pay2<-sum(datos$Payment=="Credit card")
  pay3<-sum(datos$Payment=="Ewallet")
  piepay<-data.frame(
    Method=c("Cash","Credit Card","E-wallet"),
    Total=c(pay1,pay2,pay3),
    prop=c(100*pay1/(pay1+pay2+pay3),100*pay2/(pay1+pay2+pay3),100*pay3/(pay1+pay2+pay3))) 
  
  piepay<-piepay%>%
    arrange(desc(Method))%>%
    mutate(lab.ypos=cumsum(prop)-0.5*prop)
  
  plot7<-  ggplot(piepay,aes(x="",y=prop,fill=Method))+
    geom_bar(width=1,stat="identity",color="white")+
    coord_polar("y",start=0)+
    geom_text(aes(y=lab.ypos,label=paste0(prop,"%")),color="black",size=6)+
    scale_fill_brewer(palette = "Paired")+
    theme_void()+labs(title="Repartición de los tipos de clientes",
                      subtitle="Porcentaje de clientes según sus métos de pago utilizados")+
    labs(fill = "Métodos de pago")+
    scale_fill_hue(labels = c("Efectivo", "Tarjeta de crédito", "Pago electrónico"))

  # segun sexo

  gen1<-sum(datos$Gender=="Female")
  gen2<-sum(datos$Gender=="Male")
  piegen<-data.frame(
    Gender=c("Female","Male"),
    Total=c(gen1,gen2),
    prop=c(100*gen1/(gen1+gen2),100*gen2/(gen1+gen2))) 
  
  piegen<-piegen%>%
    arrange(desc(Gender))%>%
    mutate(lab.ypos=cumsum(prop)-0.5*prop)
  
 piegen<- ggplot(piegen,aes(x="",y=prop,fill=Gender))+
    geom_bar(width=1,stat="identity",color="white")+
    coord_polar("y",start=0)+
    geom_text(aes(y=lab.ypos,label=paste0(prop,"%")),color="black",size=6)+
    theme_void()+labs(title="Repartición de los clientes según género",
                      subtitle="Porcentaje de clientes según su género",fill="Género")+
    scale_fill_hue(labels = c("Fem", "Masc"))
  
 
 pies <- grid.arrange(plot7, piegen,
                               ncol = 2, 
                               nrow = 1)
 