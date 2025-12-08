library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(broom)
library(tidyr)
library(ggrepel)
library(stats)
library(kableExtra)
library(rlang)

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


#Eliminamos outlaers que podrían perjudicar nuestro análisis

summary(df_final$subscribers)
summary(df_final$video_views)
summary(df_final$video_count)


# 1. Subscribers ----------------------------------------------------------

Q1_sub <- 19500000
Q3_sub <- 33375000
IQR_sub <- Q3_sub - Q1_sub

# Calcular límites
limite_inferior_sub <- Q1_sub - 1.5 * IQR_sub
limite_superior_sub <- Q3_sub + 1.5 * IQR_sub


# --- 2. Filtrar para eliminar outliers ---
# Se mantiene solo las filas donde 'subscribers' esté dentro del rango
df_final_clean <- df_final[df_final$subscribers >= limite_inferior_sub & 
                             df_final$subscribers <= limite_superior_sub, ]

#  En este caso, el máximo (284,000,000) está muy por encima del límite superior
# (54,187,500), por lo que será eliminado.

# --- 1. Definir los límites para 'video_views' ---
# Usamos notación científica para mayor precisión
Q1_views <- 6.917e+09
Q3_views <- 1.909e+10
IQR_views <- Q3_views - Q1_views

# Calcular límites
limite_inferior_views <- Q1_views - 1.5 * IQR_views
limite_superior_views <- Q3_views + 1.5 * IQR_views

# Imprimir límites (opcional, para revisar)
print(paste("Límite Inferior (video_views):", limite_inferior_views))
print(paste("Límite Superior (video_views):", limite_superior_views))

# Filtrar el dataframe para eliminar outliers 
# Aplicamos el filtro sobre el df que ya limpiamos de 'subscribers'
df_final_clean <- df_final_clean[df_final_clean$video_views >= limite_inferior_views & 
                                 df_final_clean$video_views <= limite_superior_views, ]

# El máximo (2.586e+11) es un outlier y será eliminado.

# 2. Views ----------------------------------------------------------------

# Definimos los límites
Q1_views <- 6.917e+09
Q3_views <- 1.909e+10
IQR_views <- Q3_views - Q1_views

# Calcular límites
limite_inferior_views <- Q1_views - 1.5 * IQR_views
limite_superior_views <- Q3_views + 1.5 * IQR_views

# Filtrar para eliminar outliers
# Aplicamos el filtro sobre el df que ya limpiamos de 'subscribers'
df_final_clean <- df_final_clean[df_final_clean$video_views >= limite_inferior_views & 
                                   df_final_clean$video_views <= limite_superior_views, ]

# El máximo (2.586e+11) es un outlier y será eliminado.

# 3. Video_count ----------------------------------------------------------

# Definir los límites para
Q1_count <- 537
Q3_count <- 4448
IQR_count <- Q3_count - Q1_count

# Calcular límites
limite_inferior_count <- Q1_count - 1.5 * IQR_count
limite_superior_count <- Q3_count + 1.5 * IQR_count


#  Filtrar el df para eliminar outliers ---
# Aplicamos el filtro final
df_final_clean <- df_final_clean[df_final_clean$video_count >= limite_inferior_count & 
                                   df_final_clean$video_count <= limite_superior_count, ]

# El máximo (386,195) es un outlier y será eliminado.


# Gráficos exploratorios --------------------------------------------------

#Haremos gráficos exploratorios

#1 gráfico de líneas

#Contamos cuantos canales hay por categorías

conteo_categorias <- df_final_clean |> 
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

top_categories <- df_final_clean |> 
  group_by(category) |> 
  summarise(total_count = n()) |> 
  arrange(desc(total_count)) |> 
  slice_head(n = 9) |>  
  pull(category)

top_categories


df_top_9_categories <- df_final_clean |>
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


#El gráfico de Evolución Anual del Número de Canales Creados (Top 9 Categorías) demuestra que las categorías de Entertainment y Music han sido históricamente las más prolíficas en generar canales exitosos, con el período de 2014 a 2017 destacándose como la era de mayor oportunidad, donde la creación de canales top alcanzó sus picos máximos, incluyendo también el auge de People & Blogs y Education. Sin embargo, a partir de 2017, la tendencia de creación de canales de élite se revierte y cae drásticamente, sugiriendo una saturación del mercado y una consolidación de la plataforma, pues la creación de nuevos canales exitosos se reduce casi a cero en los años más recientes (2022 y 2023), lo que indica que los canales grandes y antiguos dominan ahora la atención de la audiencia, dificultando la entrada de nuevos creadores a este grupo de élite.


# Gráficos de dispersión  -------------------------------------------------


#A.comparación de cantidad de videos por canal y suscriptores (millones)


ggplot(
  df_final_clean, 
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
    data = df_final_clean %>% 
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

#El gráfico de Relación entre Videos Publicados y Suscriptores, utilizando escalas logarítmicas en ambos ejes para mitigar el efecto de la asimetría, revela una conclusión fundamental: si bien existe una correlación positiva inicial donde un volumen bajo de videos (menos de 100) es necesario para construir la base de suscriptores, la tendencia principal indica que la cantidad masiva de videos (más de 200) tiene rendimientos decrecientes en términos de adquisición de audiencia. La línea de ajuste se aplana y se mantiene casi horizontal en el rango de 100 a 10,000 videos, mostrando una alta dispersión de puntos. Esto sugiere que, entre los canales exitosos, la calidad, el nicho o la antigüedad del canal son factores mucho más decisivos para el crecimiento de suscriptores que la simple acumulación de publicaciones.

#B. Subscriptores vs vistas

#nos aseguramos de tener escala en millones en vistas

df_final_clean <- df_final_clean |> 
  mutate(video_views_por_millones = video_views / 1000000)

ggplot(
  df_final_clean, 
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
    data = df_final_clean %>% 
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

#La curva de tendencia tiene dos partes: la zona plana a la izquierda indica que en los canales medianos es muy difícil convertir las vistas en suscriptores, ya que hay mucha competencia y el crecimiento es lento. Sin embargo, al alcanzar un punto clave de popularidad (cerca de 10,000 millones de vistas), la curva se dispara hacia arriba, lo que significa que en los canales gigantes, el crecimiento se vuelve explosivo; su fama y el algoritmo de YouTube hacen que cada nueva vista genere muchos más suscriptores de manera eficiente, lo que confirma que los canales que logran pasar la fase difícil son recompensados con un crecimiento mucho más rápido.

#C. Cantidad de videos vs suscriptores (escala logarítmica en eje X)

ggplot(
  df_final_clean, 
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
df_subs_por_categoria <- df_final_clean |>
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

#El gráfico de barras muestra qué tipos de contenido de YouTube acumulan la mayor cantidad total de suscriptores sumando todos los canales de esa categoría. Claramente, las categorías de Entertainment (Entretenimiento, con 3961 millones de suscriptores) y Music (Música, con más de 2925 millones) dominan por completo, lo que significa que la audiencia global de los principales YouTubers se concentra en ver contenido divertido y musical. Las categorías como People & Blogs, Gaming y Comedy también son muy fuertes y acumulan miles de millones de suscriptores, mientras que los nichos como Noticias o Deportes, aunque importantes, tienen una base total de suscriptores mucho menor en este grupo de canales principales. Esto refleja las preferencias generales de los espectadores de YouTube, que tienden a gravitar hacia el entretenimiento y la música por encima de otros tipos de contenido.


# Comenzamos con PCA ------------------------------------------------------
#Tengo de finalidad explicar versión fácil lo que aplicaré en esta tarea, ya que es algo que me costó entender, y quizás hay otras personas de la misma forma


df_pca_select <- df_final_clean |> 
  select(subscribers_por_millones, video_views, video_count)

#Acabas de tomar tus tres ingredientes principales para el éxito de un canal de YouTube:Suscriptores, Vistas, Cantidad de Videos

# Aplicar la normalización (Estandarización: media=0, desviación estándar=1)
# Esto es necesario para evitar que 'video_views' domine por su gran magnitud.
df_pca_normal <- scale(df_pca_select)

head(df_pca_normal)

# Aplicar el PCA (usando prcomp, que trabaja con las variables)

pca_result <- prcomp(df_pca_normal)

summary(pca_result)

#Esto sifnifica que aplicamos PCA para ver si podías combinar estos tres ingredientes en menos "sabores" sin perder el gusto.

#ADJUNTAR ACÁ IMG RESULTADOS 1


#PC1: El PC1 es tu componente más importante. Piensa en él como el "Sabor de Ser un Canal Grande" o la "Popularidad Bruta". Significado: Como explica el 50.29% de la varianza total de los datos, esto quiere decir que, en la estructura de los canales, los tres ingredientes (Subscribers, Video Views, Video Count) están fuertemente correlacionados. Si un canal es grande en una métrica, tiende a serlo en las otras. En Conclusión, este PC1 es la evidencia de que hay un factor subyacente de Magnitud del Canal.

#PC2: Si sumas el PC1 y el PC2, ya tienes el 82.95% de la información de tu base de datos. Conclusión: Esto es una gran victoria. Significa que ya no necesitas usar las tres variables originales. Puedes reemplazarlas por solo dos nuevas variables (PC1 y PC2) y tu análisis será casi igual de preciso (solo pierdes el 17.05% de la información).

#PCA3: El PC3 es el menos importante, explicando solo el 17.05% de la varianza total. Significado: Este componente captura las diferencias más sutiles entre los canales. Por ejemplo, podría estar destacando canales que tienen muchos videos pero relativamente pocos suscriptores y vistas, o viceversa. Conclusión: Aunque es interesante, este componente no es tan crucial para entender la mayoría de los canales en tu análisis. Según tus propios resultados, el PC3 no explica suficiente varianza para justificar su complejidad y debe ser descartado.

#Aunque el PC3 (con una desviación estándar de $0.7152$) no cumple con el Criterio de Kaiser (que sugiere retener solo los componentes con una desviación estándar mayor a 1), el criterio del 80-90% de Varianza Acumulada es a menudo preferido en la práctica para la visualización y la interpretación, y tú estás en el 82.95%.

#Acontinuación generamos cargas (loading). A partir de esto le ponemos nombres y significados a nuestros dos componenetes principales PCA1 y PCA2

pca_result$rotation

# Las cargas (loadings) te dicen cómo cada variable original contribuye a cada componente principal. Por ejemplo, si 'subscribers_por_millones' tiene una carga alta en PC1, significa que esta variable es muy importante para definir ese componente. Al final te dicen qué tan fuerte se relaciona (correlaciona) cada variable original con el nuevo componente. Cuanto más cerca esté un número de 1 o -1, más fuertemente se relaciona esa variable con el componente.


#IMAGEN RESULTADO 2

#PC1: El PC1 explica el 50.29% de la varianza total, y sus cargas son:

#Suscriptores: Muy fuerte (-0.668)

#Vistas: Muy fuerte (-0.699)

#Video Count: Débil (-0.255)

# El PC1 representa la Magnitud, Popularidad o Éxito General del canal. Un valor extremo (negativo) en PC1 significa que el canal tiene, simultáneamente, muchos Suscriptores y muchas Vistas. Este es el factor común que impulsa el éxito en YouTube.


#PCA2:El PC2 explica el 32.66% de la varianza total (acumulando el 82.95% con PC1) y sus cargas son:

#Video Count: Extremadamente fuerte (+0.951).

#Las cargas de subscribers_por_millones (-0.305) y video_views (-0.056) son mucho más bajas (cercanas a cero).

#En cnclusión, El PC2 representa la Estrategia o el Volumen de Contenido. Este componente es impulsado casi por completo por la Cantidad de Videos. Si un canal tiene un valor positivo alto en PC2, su éxito se define por el volumen puro de videos (Video Count alto). El PC2 nos ayuda a diferenciar entre canales que alcanzan el éxito (PC1) publicando muchísimo (PC2 alto) versus canales que lo hacen con pocos videos de alto impacto (PC2 bajo).

#PC3: El PC3 captura la varianza que queda, destacando una diferencia clave entre las métricas de audiencia. Sus cargas son:

#subscribers_por_millones tiene una carga negativa alta de -0.679.

#video_views tiene una carga positiva alta de +0.713.

#A partir de esto, este componente indica un trade-off (intercambio) o una diferencia en la eficiencia de conversión. Un canal con un PC3 positivo alto tiende a tener muchas video_views en relación con sus subscribers. Un canal con un PC3 bajo (negativo) tiende a tener muchos subscribers que han generado menos video_views.


# Generar Biplot ----------------------------------------------------------


#El gráfico que debes crear se llama Biplot. Este gráfico te permite ver, en un solo lugar, dos cosas:
  
#Dónde se ubica cada canal de YouTube en tu nuevo factor de Éxito/Magnitud (PC1) y Estrategia/Volumen (PC2).

#Cómo se relacionan tus variables originales (Suscriptores, Vistas, Videos) con esos dos nuevos factores (flechas).


df_pca_2 <- df_pca_normal |> 
  prcomp()

df_pca_2

df_pca_2 |> 
  broom::tidy(matrix = "eigenvalues") |> 
  kable() |> 
  head(n = 10)
df_pca_2 |> 
  broom::tidy(matrix = "rotation") |> 
  kable() |> 
  head(n = 10)


df_pca_2 |>
  broom::tidy(matrix = "scores") |> 
  pivot_wider(names_from = "PC",
              names_prefix = "PC_",
              values_from = "value") |>
  kable() |> 
  scroll_box() 

df_pca_2_cat <- df_pca_2 |>
  broom::tidy(matrix = "scores") |> 
  pivot_wider(names_from = "PC",
              names_prefix = "PC_",
              values_from = "value") |> 
  bind_cols(df_final_clean)

head(df_pca_2_cat)

#GRAFICO

colores_gradiente <- c("#990066", "#cc3399", "#ff6600", "#ffcc00")

colores_paises <- c("#cc3399","#ff6600","#ff9900","#006699")

colores_scree <- c("#99cc33", "#3399cc", "#cc3399", "#ff9900")

df_pca_2_cat |> 
  ggplot(aes(x = PC_1, y = PC_2)) +
  geom_point(alpha = 0.5, size = 0.2) +
  labs(x = "Componente 1 (64,5%)",
       y = "Componente 2 (10%)") +
  theme_linedraw() +
  theme(legend.position = "none")


#Agregamos *kernel density estimation*:
  
df_pca_2_cat |> 
  ggplot(aes(x = PC_1, y = PC_2)) +
  geom_point(alpha = 0.5, size = 0.2) +
  geom_density_2d(aes(color = after_stat(level)), linewidth = 0.8) +
  scale_color_gradientn(colours = colores_gradiente) +
  labs(x = "Componente 1 (50.29
       %)",
       y = "Componente 2 (32.66%)") +
  theme_linedraw() +
  theme(legend.position = "none")


# Borrador de lo que sigue ------------------------------------------------

# 1. Definir un factor de escala para las flechas (vectores)
factor_escala <- 8 

# 2. ESCALAMOS LA ROTACIÓN 
df_pca_rotation_scaled <- df_pca_rotation |>
  mutate(
    PC_1_scaled = PC_1 * factor_escala,
    PC_2_scaled = PC_2 * factor_escala
  )

df_pca_scores |>
  ggplot(aes(x = PC_1, y = PC_2)) +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_vline(xintercept = 0, color = "grey80") +
  
  # 1. Puntos (Canales) - Coloreados por la Categoría
  geom_point(aes(color = category), # ¡Añadimos el color por categoría!
             alpha = 0.5,
             size = 1.5) +
  
  # 2. Vectores (Flechas) - Usamos el df_pca_rotation_scaled
  geom_segment(
    data = df_pca_rotation_scaled,
    mapping = aes(
      x = 0,
      xend = PC_1_scaled,
      y = 0,
      yend = PC_2_scaled
    ),
    color = "#cc3399", 
    arrow = arrow(length = unit(0.2, "cm"), type = "closed")
  ) +
  
  # 3. Etiquetas de Variables
  geom_label_repel(
    data = df_pca_rotation_scaled,
    aes(
      x = PC_1_scaled,
      y = PC_2_scaled,
      label = variable 
    ),
    color = "#990066",
    fill = "#FFFFFF80",
    size = 3
  ) +
  
  # 4. Títulos y Etiquetas CORREGIDOS con tus valores de varianza
  labs(
    x = "PC1 (50.29%)", 
    y = "PC2 (32.66%)", 
    title = "Biplot: Canales de YouTube en las Dimensiones de Éxito",
    color = "Categoría" 
  ) +
  theme_linedraw() +
  theme(legend.position = "bottom")

# 1. Prepara los datos de Rotación para el gráfico de barras
# df_pca_rotation debe tener las columnas 'variable', 'PC_1', 'PC_2', 'PC_3'
df_pca_loadings_bar <- df_pca_rotation |>
  select(variable, PC_1, PC_2) |> # Seleccionamos solo PC1 y PC2
  pivot_longer(names_to = "pca", values_to = "valor_pca", cols = c(PC_1, PC_2))

# 2. Graficamos las cargas de las variables en PC1 y PC2
df_pca_loadings_bar |>
  ggplot(aes(x = fct_reorder(variable, valor_pca), y = valor_pca)) +
  geom_col(fill = "#006699") + 
  geom_hline(yintercept = 0) +
  
  # Etiquetas de valor
  geom_text(aes(label = round(valor_pca, 2),
                y = if_else(valor_pca >= 0,
                            valor_pca + 0.05,  
                            valor_pca - 0.05)), 
            size = 3) +
  
  coord_flip() +
  facet_grid(cols = vars(pca),
             labeller = labeller(
               PC1 = "Componente 1 (50.29%)", 
               PC2 = "Componente 2 (32.66%)"
             )) +
  labs(x = NULL,
       y = "Peso de la Variable (Carga)",
       title = "Cargas Factoriales en PC1 y PC2") +
  theme_linedraw()

# instrucciones -----------------------------------------------------------



#2. Dejar una base de datos que tenga, youtuber, subsicritores, views y número de videos. 
#3. Hacer un indicador tomando 3 variables con las que haré el indicador: suscriptores, views y número de videos. Youtuber no porqeu es unidad de análisis - otra base de datos qeu se llame como varibles de pca. Con esta base de datos hacer pasos que la profe entregó. 

#5. Luego se hace el pca, edepende de cómo queden los gráficos exploratorios, hacer summary y eliminar los outlayers.

#*Sugerencia: *usar ggplot para pca,

#6. gráfcios debo analizar cada gráfico teoricamente, cualquier cosa busacr cómo analizar por youtube. 

#7. intentar cruzar variables y sacar correlaciones antes de hacer el pca