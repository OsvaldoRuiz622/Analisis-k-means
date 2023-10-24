
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse","cluster", "factoextra","NbClust","tidyr")
ipak(packages)
#Definimos nuestro entorno de trabajo, en este caso, un archivo llamado USArrest que
#lo descarga de internet y lo asignamos a una variable df para trabajarla mas sencillo 
df <- USArrests
df

#la función scale() se utiliza para realizar la estandarización de 
#variables numéricas. La estandarización es un proceso que transforma una 
#variable numérica al restarle la media de la variable y dividirla por su 
#desviación estándar. Esto hace que la variable tenga una media igual a 
#cero y una desviación estándar igual a uno.

#Facilita la comparación cuando se trabaja con variables que tienen diferentes
#escalas o unidades, la estandarización permite compararlas de manera más 
#equitativa, ya que todas las variables estarán en la misma escala.
df <- scale(df)
#Imprimimos los primeros datos para ver el cambio
head(df)

#Checar si el metodo de k.medias es viable para analizar los datos
#calcular la matriz de distacias
m.distancia <- get_dist(df, method = "euclidean") #el método aceptado también puede ser: "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" o "kendall"
fviz_dist(m.distancia, gradient = list(low = "blue", mid = "white", high = "red"))

#estimar el número de clústers mediante distintos metodos
#Elbow, silhouette o gap_stat  method
fviz_nbclust(df, kmeans, method = "wss")
fviz_nbclust(df, kmeans, method = "silhouette")
fviz_nbclust(df, kmeans, method = "gap_http://127.0.0.1:46565/graphics/plot_zoom_png?width=353&height=463stat")

#con estas otras funciones se pueden calcular:
#the index to be calculated. This should be one of : "kl", "ch", "hartigan", "ccc", "scott",
#"marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda",
#"pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma",
#"gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all" (all indices except GAP,
#Gamma, Gplus and Tau), "alllong" (all indices with Gap, Gamma, Gplus and Tau included).

#Pero con esto, se calculan todas de una y te envia un resumen con la conclusion
resnumclust<-NbClust(df, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "alllong")


#calculamos los dos clústers, con 25 ciclos de ejecucion
k2 <- kmeans(df, centers = 2, nstart = 25)
k2

#mas informacion
str(k2)
#summary(k2)
#Graficar los cluster
fviz_cluster(k2, data = df)
fviz_cluster(k2, data = df, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE) #ellipse.type= "t", "norm", "euclid"
fviz_cluster(k2, data = df, ellipse.type = "norm")
fviz_cluster(k2, data = df, ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())

#Graficar, queda como Cluster jerarquico
res2 <- hcut(df, k = 2, stand = TRUE)
fviz_dend(res2, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF"))

res4 <- hcut(df, k = 4, stand = TRUE)
fviz_dend(res4, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF","green","black"))




#pasar los cluster a mi df inicial para trabajar con ellos
#Sacar medias de la tabla, la original
USArrests %>%
  mutate(Cluster = k2$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")


#Volvemos a definir df, para trabajar con los datos 
df <- USArrests
df
#La variable asiciada a k2
#mete el cluster de k2 como un factor dentro de df
df$clus<-as.factor(k2$cluster)
#Reaulta en la base de datos anterior, pero ahora tipificada con el cluster al que corresponde
df

#Repetimos el proceso solo para estandarizar variables, decirle que df lo use como data frame
#no como matriz, para poder trabajarlo.
df <- USArrests
df <- scale(df)
df<- as.data.frame(df)
df$clus<-as.factor(k2$cluster)
df

#Le decimos que la variable clus, es un factor, y clus sera una variable que no cambia
#Al transformar la tabla, los datos van a cambiar respecto a clus, para asi, eliminar
#por decirlo de alguna forma, el nombre del estado, y todo este en referencia al cluster
#Como queda a continuacion
df$clus<-factor(df$clus)
data_long <- gather(df, caracteristica, valor, Murder:Rape, factor_key=TRUE)
data_long

#Graficamos todos los datos
#Factor x es caracteritica, y es el valor
ggplot(data_long, aes(as.factor(x = caracteristica), y = valor,group=clus, colour = clus)) + 
  stat_summary(fun = mean, geom="pointrange", size = 1)+
  stat_summary(geom="line")
  geom_point(aes(shape=clus))
  