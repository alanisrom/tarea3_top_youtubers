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


# LIMPIEZA DE BASE DE DATOS -----------------------------------------------


df_top <- read_csv("Top Youtubers Dataset.csv")

df_top <- df_top |> 
  janitor::clean_names()

colnames(df_top)


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


# Filtrar para eliminar outliers: se mantiene solo las filas donde 'subscribers' esté dentro del rango
df_final_clean <- df_final[df_final$subscribers >= limite_inferior_sub & 
                             df_final$subscribers <= limite_superior_sub, ]

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

# Gráficos exploratorios --------------------------------------------------

#Haremos gráficos exploratorios


# 1 gráfico de líneas -----------------------------------------------------


#Contamos cuántos canales hay por categorías

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


# GRÁFICOS DE DISPERSIÓN  -------------------------------------------------


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

#B. Subscriptores vs vistas

#nos aseguramos de tener escala en millones en vistas

df_final_clean <- df_final_clean |> 
  mutate(video_views_por_millones = video_views / 1000000)

ggplot(
  df_final_clean, 
  aes(
    x = video_views_por_millones, 
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


#C. Cantidad de videos vs vistas 

ggplot(
  df_final_clean, 
  aes(
    x = video_views_por_millones, 
    y = video_count 
  )
) +
  geom_point(color = "#0072B2", size = 2, alpha = 0.7) + 
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



# 3. Gráfico de barras: Suma total de suscriptores por categoría ----------


# 1. Agrupar y Sumar Suscriptores por Categoría: Calculamos el total de suscriptores (en millones) para cada categoría

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


# APLICAMOS PCA ------------------------------------------------------


df_pca_select <- df_final_clean |> 
  select(subscribers_por_millones, video_views, video_count)

df_pca_normal <- scale(df_pca_select)

head(df_pca_normal)

# Aplicar el PCA (usando prcomp, que trabaja con las variables)

pca_result <- prcomp(df_pca_normal)

summary(pca_result)

pca_result$rotation


# VISUALIZACIONES ----------------------------------------------------------


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

#Gráfico 1

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

#gráfico 2

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

#Gráfico 3

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

