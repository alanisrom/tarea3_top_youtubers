library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)

df_top <- read_csv("Top Youtubers Dataset.csv")

df_top <- df_top |> 
  janitor::clean_names()

colnames(df_top)

#Eliminamos aquellos n que en variables poseen 0 y que en categoría salga na. además, hacemos filtro por año, ya que youtube se filtró en 2005 (Eliminar el outlier de tiempo (started < 2005)). 

data_filas_sin0 <- df_top|> 
  filter(subscribers != 0,
         video_views != 0,
         video_count != 0,
         category != "na",
         !is.na(category),
         started >= 2005)

#Luego eliminamos el 25% más pequeños de youtubers (subscribers)

q1_value <- summary(data_filas_sin0$subscribers)["1st Qu."]
print(paste("El valor de suscriptores en el Primer Cuartil (Q1) es:", q1_value))

data_filas_sin0 <- data_filas_sin0 |> 
  filter(subscribers > q1_value)

print(paste("Número de canales en la base final para PCA:", nrow(data_filas_sin0)))

#Vemos los cuartiles para poder generar una nueva variable para escalar a millones, ya que así tendremos los mismo valores, pero descritos más pequeños y fáciles de graficar en variable de subscriptores.

summary(data_filas_sin0$subscribers)

df_final <- data_filas_sin0 |> 
  mutate(subscribers_por_millones = subscribers / 1000000)

#Haremos gráficos exploratorios

#1 gráfico de líneas

#Contamos cuantos canales hay por categorías

conteo_categorias <- df_final |> 
  group_by(category) |> 
  summarise(cantidad = n(), .groups = 'drop') |> 
  arrange(desc(cantidad))

#grafico de barras

grafico_barras_categorias <- ggplot(conteo_categorias, aes(x = reorder(category, cantidad), y = cantidad)) +
  geom_col(fill = "#0072B2") +
  geom_text(aes(label = cantidad), hjust = -0.1, size = 3) + 
  
  coord_flip() + 
  
  labs(title = "Distribución de Canales de youtube dentro del top por Categoría",
       x = "Categoría",
       y = "Número de Canales",
       caption = "Fuente: Datos obtenidos del repositorio de GitHub 'top-youtubers-analysis'. \nElaboración propia.") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))


grafico_barras_categorias

#A partir de esto, para hacer el gráfico de líneas se decide incorporar solo las 9 categorías más predominantes. 

top_categories <- df_final |> 
  group_by(category) |> 
  summarise(total_count = n()) |> 
  arrange(desc(total_count)) |> 
  slice_head(n = 9) |>  # Escogemos los nombres de las 12 primeras
  pull(category)

top_categories


df_top_9_categories <- df_final |>
  filter(category %in% top_categories)

# Agrupamos por año de inicio y categoría para contar la cantidad de canales
df_top_9_categories <- df_top_9_categories |>
  group_by(started, category) |>
  summarise(
    cantidad_canales = n(),
    .groups = 'drop'
  ) |>
  rename(year = started)

# Vistazo a los datos preparados
print(head(df_top_9_categories))

#graficar 
grafico_lineas_evolucion <- ggplot(df_top_9_categories,
                                   aes(x = year,
                                       y = cantidad_canales,
                                       color = category,
                                       group = category)) +
  geom_line(size = 1.1) + 
  geom_point(size = 2.5) +
  scale_color_viridis_d(option = "turbo") +
  scale_x_continuous(breaks = seq(min(df_top_9_categories$year), max(df_top_9_categories$year), by = 2)) +
  scale_y_continuous(limits = c(0, max(df_top_9_categories$cantidad_canales) * 1.1)) + 
  labs(
    title = "Evolución Anual del Número de Canales Creados (Top 9 Categorías)",
    subtitle = "Tendencia en la creación de canales de YouTube desde 2005",
    x = "Año de Inicio del Canal",
    y = "Número de Canales Creados",
    caption = "Fuente: Datos obtenidos del repositorio de GitHub 'top-youtubers-analysis'. \nElaboración propia.",
    color = "Categoría de canal"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right" # Colocamos la leyenda a la derecha para no obstruir las líneas
  )

# Mostrar el gráfico
grafico_lineas_evolucion


#El gráfico de líneas muestra que las categorías de Entretenimiento y Música han sido, históricamente, las más populares a la hora de crear nuevos canales exitosos, con grandes picos de actividad especialmente entre 2014 y 2017, que fue el gran momento de crecimiento de YouTube. Aunque categorías como People & Blogs y Gaming también han tenido años de auge, en general, el gráfico nos dice que cada vez es más difícil para un canal completamente nuevo (creado después de 2021) entrar en este grupo de élite de Top YouTubers, lo que sugiere que el mercado está saturado, y los canales más antiguos y grandes dominan ahora la plataforma.

#gráficos de disperción

#A.comparación de cantidad de videos por canal y suscriptores (millones)


ggplot(
  df_final, 
  aes(
    x = video_count,
    y = subscribers_por_millones
  )
) +
  geom_point(color = "#8B1C62", size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", color = "#0072B2") +
  scale_x_log10(labels = scales::comma, 
                breaks = c(10, 100, 1000, 10000, 100000)) + 
  scale_y_log10(labels = scales::comma, 
                breaks = c(1, 5, 10, 50, 100, 200, 300)) + 
  geom_text(
    data = df_final %>% 
      filter(subscribers_por_millones > 100),
    aes(label = youtuber),
    size = 3,
    color = "black",
    check_overlap = TRUE,
    vjust = -1
  ) +
  
  labs(
    title = "Relación entre Videos Publicados y Suscriptores",
    subtitle = "Dispersión de canales de YouTube para evaluar si la cantidad de videos se correlaciona con la base de suscriptores.
Ambos ejes usan escala logarítmica.",
    x = "Cantidad de Videos por Canal (Escala Logarítmica)",
    y = "Suscriptores (Millones - Escala Logarítmica)", 
    caption = "Fuente: Datos obtenidos del repositorio de GitHub 'top-youtubers-analysis'. \nElaboración propia."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 11)
  )

#El gráfico de dispersión, que usa una escala especial (logarítmica) para ambos ejes, compara la Cantidad de Videos por Canal con la base de Suscriptores, y muestra que la relación es muy débil. La línea de tendencia es casi plana, lo que indica que subir más videos no garantiza tener más suscriptores. Hay una gran cantidad de canales que han subido miles o decenas de miles de videos (extremo derecho) pero se mantienen en el mismo nivel de suscriptores que canales que han subido mucho menos. Solo los outliers extremos, como MrBeast y T-Series, demuestran que, al combinar una gran cantidad de videos con una marca poderosa, es posible alcanzar los niveles más altos de suscriptores, pero para la mayoría de los canales, la cantidad de videos no es el factor determinante.

#B. Subscriptores vs vistas

#nos aseguramos de tener escala en millones en vistas

df_final <- df_final |> 
  mutate(video_views_por_millones = video_views / 1000000)

ggplot(
  df_final, 
  aes(
    # Eje X: Vistas totales escaladas a millones
    x = video_views_por_millones, 
    # Eje Y: Suscriptores escalados a millones
    y = subscribers_por_millones 
  )
) +
  geom_point(color = "#4c72b0", size = 2, alpha = 0.7) + 
  geom_smooth(method = "loess", color = "#8B1C62") + 
  scale_x_log10(labels = scales::comma, 
                breaks = c(100, 1000, 10000, 100000, 1000000)) + 
  scale_y_log10(labels = scales::comma, 
                breaks = c(1, 5, 10, 50, 100, 200, 300)) + 
  geom_text(
    data = df_final %>% 
      filter(subscribers_por_millones > 100 &
               video_views_por_millones < 90000),
    aes(label = youtuber),
    size = 3,
    color = "black",
    check_overlap = TRUE,
    vjust = -1
  ) +
  labs(
    title = "Relación entre Vistas Totales y Suscriptores por Canal",
    subtitle = "Dispersión de canales de YouTube entre las vistas totales del canal y la base de suscriptores. 
Ambos ejes usan escala logarítmica.",
    x = "Vistas Totales de Video (Millones - Escala Logarítmica)",
    y = "Suscriptores (Millones - Escala Logarítmica)", 
    caption = "Fuente: Datos obtenidos del repositorio de GitHub 'top-youtubers-analysis'. \nElaboración propia."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 11)
  )

#La curva de tendencia (loess) tiene dos partes: la zona plana a la izquierda indica que en los canales medianos es muy difícil convertir las vistas en suscriptores, ya que hay mucha competencia y el crecimiento es lento. Sin embargo, al alcanzar un punto clave de popularidad (cerca de 10,000 millones de vistas), la curva se dispara hacia arriba, lo que significa que en los canales gigantes (como MrBeast) el crecimiento se vuelve explosivo; su fama y el algoritmo de YouTube hacen que cada nueva vista genere muchos más suscriptores de manera eficiente, lo que confirma que los canales que logran pasar la fase difícil son recompensados con un crecimiento mucho más rápido.

#C. Cantidad de videos vs suscriptores (escala logarítmica en eje X)

ggplot(
  df_final, 
  aes(
    # Eje X: Vistas totales escaladas a millones (CORRECTO)
    x = video_views_por_millones, 
    # Eje Y: Cantidad de Videos (CORRECTO)
    y = video_count 
  )
) +
  geom_point(color = "#0072B2", size = 2, alpha = 0.7) + # Nuevo color de punto
  geom_smooth(method = "lm", color = "#8B1C62") + 
  scale_x_log10(labels = scales::comma, 
                breaks = c(100, 1000, 10000, 100000, 1000000)) + 
  scale_y_log10(labels = scales::comma, 
                breaks = c(10, 100, 1000, 10000, 100000)) + 
  labs(
    title = "Relación entre Vistas Totales y Cantidad de Videos por Canal",
    subtitle = "Regresión Lineal  para encontrar relación entre las vistas y la cantidad de videos. 
Ambos ejes usan escala logarítmica.",
    x = "Vistas Totales de Video (Millones - Escala Logarítmica)",
    y = "Cantidad de Videos por Canal (Escala Logarítmica)", 
    caption = "Fuente: Datos obtenidos del repositorio de GitHub 'top-youtubers-analysis'. \nElaboración propia."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 11)
  )

#El gráfico nos muestra si subir más videos ayuda a un canal a tener más vistas en total. Vemos una línea recta ascendente que confirma que, en promedio, sí hay una relación positiva: los canales que han subido más videos (Eje Y) suelen tener más vistas (Eje X). Sin embargo, el gran esparcimiento de puntos alrededor de esa línea nos dice que el volumen no lo es todo. Por ejemplo, hay canales que han subido muchísimos videos, pero no han acumulado tantas vistas (quedan por encima de la línea), mientras que otros canales han subido pocos videos pero han tenido un éxito enorme en vistas (quedan por debajo de la línea, en la parte derecha). Esto significa que aunque subir mucho contenido ayuda, la clave para el éxito masivo es la eficiencia o la calidad de los videos, pues unos pocos videos virales pueden superar el volumen de miles de videos de bajo impacto.

#3. Gráfico de barras: Suma total de suscriptores por categoría

# 1. Agrupar y Sumar Suscriptores por Categoría
# Calculamos el total de suscriptores (en millones) para cada categoría
df_subs_por_categoria <- df_final |>
  group_by(category) |>
  summarise(
    total_subscribers_millones = sum(subscribers_por_millones, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  arrange(desc(total_subscribers_millones))

# 2. Generar el Gráfico de Barras
grafico_subs_por_categoria <- ggplot(
  df_subs_por_categoria, 
  aes(
    x = reorder(category, total_subscribers_millones), 
    y = total_subscribers_millones
  )
) +
  geom_col(fill = "#8B1C62")+ 
  geom_text(
    aes(label = round(total_subscribers_millones, 0)), 
    hjust = -0.1, 
    size = 3
  ) +
  coord_flip() + 
  
  labs(
    title = "Suma Total de Suscriptores por Categoría de YouTube",
    subtitle = "Suscriptores acumulados (en millones) por las categorías principales",
    x = "Categoría",
    y = "Suscriptores Totales Acumulados (en Millones)",
    caption = "Fuente: Datos obtenidos del repositorio de GitHub 'top-youtubers-analysis'. \nElaboración propia."
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 11)
  )

# Mostrar el gráfico
grafico_subs_por_categoria

#El gráfico de barras muestra qué tipos de contenido de YouTube acumulan la mayor cantidad total de suscriptores sumando todos los canales de esa categoría. Claramente, las categorías de Entertainment (Entretenimiento, con más de 5,600 millones de suscriptores) y Music (Música, con más de 4,500 millones) dominan por completo, lo que significa que la audiencia global de los principales YouTubers se concentra en ver contenido divertido y musical. Las categorías como People & Blogs, Gaming y Comedy también son muy fuertes y acumulan miles de millones de suscriptores, mientras que los nichos como Noticias o Deportes, aunque importantes, tienen una base total de suscriptores mucho menor en este grupo de canales principales. Esto refleja las preferencias generales de los espectadores de YouTube, que tienden a gravitar hacia el entretenimiento y la música por encima de otros tipos de contenido.


# instrucciones -----------------------------------------------------------



#2. Dejar una base de datos que tenga, youtuber, subsicritores, views y número de videos. 
#3. Hacer un indicador tomando 3 variables con las que haré el indicador: suscriptores, views y número de videos. Youtuber no porqeu es unidad de análisis - otra base de datos qeu se llame como varibles de pca. Con esta base de datos hacer pasos que la profe entregó. 

#4. Recomienda antes de comenzar con el pca hacer gráficos exploratorios: 1. lineas: x año y: número de youtuber ¿por catgeoria, y así vemos su evolución por categoría (grafico de lineas de tiempo)- cruzar subscriterios con números de video (gráfico de disperión) si queda muy normal aplicar otras variables si quda muy concentrada, dinero siempre debe estar transofrmado a lograritmo natural.  
#5. Luego se hace el pca, edepende de cómo queden los gráficos exploratorios, hacer summary y eliminar los outlayers.

#*Sugerencia: *usar ggplot para pca,

#6. gráfcios debo analizar cada gráfico teoricamente, cualquier cosa busacr cómo analizar por youtube. 

#7. intentar cruzar variables y sacar correlaciones antes de hacer el pca