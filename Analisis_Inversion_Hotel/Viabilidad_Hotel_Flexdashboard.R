##Formato Rmarkdown

---
title: "MiniCurso Flexdashboards: Caso práctico hoteles de Nueva York"
output: 
  flexdashboard::flex_dashboard:
editor_options: 
  chunk_output_type: console
---
```{r include = F}
#lista de paquetes que vamos a usar
paquetes <- c('flexdashboard',
              'DT',
              'tidyverse',
              'ggplot2',
              'plotly',
              'knitr',
              'kableExtra'
)
#Crea un vector logico con si estan instalados o no
instalados <- paquetes %in% installed.packages()
#Si hay al menos uno no instalado los instala
if(sum(instalados == FALSE) > 0) {
  install.packages(paquetes[!instalados])
}
lapply(paquetes,require,character.only = TRUE)
```


```{r setup, include = F}
#Desactivamos notación científica
options(scipen=999)
```

```{r, include = F}
#CARGA DE DATOS

df <- read_csv('./Datasets/AirbnbNY.csv')

#Revisamos y entendemos los datos
glimpse(df)
View(df)

#TRANSFORMACION DATOS

#Nombres a español

nombres <- c('id',
             'nombre',
             'dueno',
             'dueno_nombre',
             'barrio_grupo',
             'barrio',
             'latitud',
             'longitud',
             'tipo_habitacion',
             'precio',
             'min_noches',
             'num_resenas',
             'ult_resena',
             'resenas_mes',
             'dueno_listings',
             'disponibilidad')

names(df) <- nombres

#Ajuste de tipos

df <- df %>% 
  mutate_at(c('barrio_grupo','barrio','tipo_habitacion'),factor)

#Identificacion de nulos
df %>% 
  summarise_all(~sum(is.na(.))) %>% 
  t()

#Gestión de nulos
df <- df %>% 
  filter(!is.na(nombre) & !is.na(dueno_nombre)) %>% 
  mutate(resenas_mes = ifelse(is.na(resenas_mes), 0, resenas_mes))

```

```{r, include = F}
#Cálculos a usar en el dashboard

#Número de alojamientos totales
num_aloj <- nrow(df)

#Número de alojamientos por barrio
num_aloj_barrio <- count(df,barrio,sort = T)

#Número de alojamientos por grupo de barrio
num_aloj_barrio_grupo <- count(df,barrio_grupo)

#Número de alojamientos por tipo de habitación
num_aloj_tipo_hab <- count(df,tipo_habitacion)

#Reseñas por tipo de habitación
resenas_tipo_hab <- df %>% 
  group_by(tipo_habitacion) %>% 
  summarise(tot_resenas = mean(num_resenas))

#Saturación de cada tipo de habitación en cada grupo de barrio
saturacion <- count(df,barrio_grupo,tipo_habitacion) %>% 
  ggplot(aes(barrio_grupo,tipo_habitacion, fill = n)) + geom_tile()

#Precio por barrio
precio_barrio <- df %>% 
  group_by(barrio) %>% 
  summarise(Precio_medio = mean(precio))
```

Análisis de viabilidad para cadena hotelera [Curso Flexdashboard de DS4B]

Análisis de competencia
===================================================

## Columna 1 {data-width=200}

INDICADORES CLAVE

### Número de alojamientos totales
```{r}
valueBox(prettyNum(num_aloj,big.mark = '.'),
         caption = 'Número de alojamientos totales',
         color = 'red')
```

### Número de apartarmentos
```{r}
valueBox(prettyNum(
  filter(num_aloj_tipo_hab, tipo_habitacion == 'Entire home/apt') %>% pull(n),
  big.mark = '.'),
         caption = 'Número de apartamentos',
         color = 'blue')
```

### Número de habitaciones privadas
```{r}
valueBox(prettyNum(
  filter(num_aloj_tipo_hab, tipo_habitacion == 'Private room') %>% pull(n),
  big.mark = '.'),
         caption = 'Número de habitaciones privadas',
         color = 'green')
```

### Número de habitaciones compartidas
```{r}
valueBox(prettyNum(
  filter(num_aloj_tipo_hab, tipo_habitacion == 'Shared room') %>% pull(n),
  big.mark = '.'),
         caption = 'Número de habitaciones compartidas',
         color = 'grey')
```

### Comentario en texto libre {.no-title}

La oferta se divide en tres categorías:

- Apartamentos enteros
- Habitaciones privadas
- Habitaciones compartidas



## Columna 2 {.tabset} {data-width=400}

### COMPETENCIA POR GRUPO DE BARRIO
```{r}
  ggplot(num_aloj_barrio_grupo, aes(barrio_grupo,n)) + 
    geom_col() +
    theme_bw()
```

### COMPETENCIA POR TIPO DE HABITACION
```{r}
df %>% 
  group_by(tipo_habitacion) %>% 
  summarise(Conteo = n()) %>% 
  arrange(desc(tipo_habitacion)) %>% 
  mutate(proporcion = round(Conteo / sum(Conteo) * 100,1),
         posicion_y = cumsum(proporcion) - proporcion * 0.5) %>% 
  ggplot(aes(x = '', y = proporcion, fill = tipo_habitacion)) +
  geom_bar(width = 1, stat = 'identity', color = 'white', alpha = 0.7) +
  geom_text(aes(y = posicion_y, label = proporcion), color = 'white', size = 5) +
  coord_polar('y', start = 0) +
  theme_void() 
```

## Columna 3 {.tabset} {data-width=400}

### COMPETENCIA EN CADA UNO DE LOS BARRIOS

```{r}
datatable(num_aloj_barrio,
          options = list(
            scrollY = T,
            paging = F
          ))
```


Análisis de precio {.storyboard}
===================================================

### Existe una gran variabilidad de precio en función del barrio

```{r}
ggplotly(
  ggplot(precio_barrio, aes(x = fct_reorder(barrio,Precio_medio),
                          y = Precio_medio,
                          text = paste(barrio,round(Precio_medio), sep = ': '))) +
  geom_col() +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  
  coord_flip(),
  tooltip = 'text'
)
```

### Y también por tipo de habitación {data-commentary-width=400}

```{r}
df %>% 
  group_by(barrio_grupo,tipo_habitacion) %>% 
  summarise(precio_medio = round(mean(precio))) %>% 
  pivot_wider(names_from = tipo_habitacion, values_from = precio_medio) %>% 
  kable ()%>%
  kable_styling()
```

***

Principales conclusiones:

- En Manhattan los precios son más altos en todos los tipos de habitaciones
- Las habitaciones privadas más baratas están en Staten Island
- Las habitaciones compartidas más baratas están en Brooklyn


### Sin embargo no existe una relación clara en la que se vayan bajando los precios si aumenta el número de noches

```{r}
ggplot(df, aes(min_noches, precio, fill = tipo_habitacion, color = tipo_habitacion, shape = tipo_habitacion)) + 
    geom_point() +
    geom_smooth(aes(color = tipo_habitacion), se = F) +
    scale_x_continuous(limits = c(0, 400)) +
    theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
```


