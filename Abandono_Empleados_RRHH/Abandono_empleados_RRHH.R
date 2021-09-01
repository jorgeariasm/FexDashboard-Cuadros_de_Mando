##Formato Markdown

---
title: "MiniCurso Flexdashboards: Caso práctico abandono de empleados"
output: 
  flexdashboard::flex_dashboard:
editor_options: 
  chunk_output_type: console
---

```{r setup, include = F}
#Desactivamos notación científica
options(scipen=999)

#INSTALAMOS LOS PAQUETES
library(flexdashboard)
library(knitr)#para formatear las tablas
library(tidyverse)
library(plotly)
```

```{r, include = F}
#CARGA DE DATOS

df <- read_csv('./Datasets/abandono_rrhh.csv')

#Revisamos y entendemos los datos
glimpse(df)
View(df)

```

```{r, include = F}
#Cálculos a usar en el dashboard

#Tasa de fuga del último año
fuga_ult_ano <- df %>%
  count(abandono) %>% 
  mutate(porc = n / sum(n) * 100) %>% 
  filter(abandono == 'Yes') %>% 
  pull(porc) %>% 
  round()

#Número empleados con scoring superior a 20%
scoring_mayor_20 <- df %>% 
  filter(abandono == 'No' & scoring > 0.2) %>% 
  nrow()

#Empleados concretos en riesgo
empleados_en_riesgo <- df %>% 
  filter(abandono == 'No') %>% 
  arrange(desc(scoring)) %>% 
  slice(1:20) %>% 
  select(id,scoring,puesto,departamento)

#Empleados retenidos
empleados_retenidos <- 13

#Impacto económico medio
imp_eco_medio <- df %>% 
  filter(abandono == 'No') %>% 
  summarise(mean(impacto_abandono))

#Coste ahorrado
coste_ahorrado <- empleados_retenidos * imp_eco_medio

#Gráfico de matriz estratégica
graf_mat_est <- df %>%
  filter(abandono == 'No') %>%
  mutate(prioridad = as.factor(ifelse(
    scoring > 0.15 & impacto_abandono > 25000, 1, 0
  ))) %>%
  select(id, scoring, impacto_abandono, prioridad) %>%
  mutate(scoring = round(scoring,2)) %>% 
  ggplot(
    aes(
      x = scoring,
      y = impacto_abandono,
      color = prioridad,
      shape = prioridad,
      size = impacto_abandono,
      text = id
    )
  ) +
  geom_jitter() +
  geom_vline(xintercept = 0.15) +
  geom_hline(yintercept = 25000) +
  theme_bw()

#Gráfico de sensibilidad del modelo por departamento
graf_sensi_modelo <- ggplot(df,aes(x = puesto, y = scoring, color = abandono)) +
  geom_boxplot() +
  coord_flip()

```

Cuadro de mando realizado para el minicurso de Flexdashboard de DS4B

## Columna 1

### Tasa media de fuga actual
```{r}
gauge(fuga_ult_ano,
      min = 3,
      max = 25,
      gaugeSectors(
           danger = c(20,25),
           warning = c(11,19),
           success = c(3,10)))
```

### Empleados con scoring > 20 {.no-title}
```{r}
valueBox(scoring_mayor_20,
         caption = 'Número de empleados alto riesgo',
         color = 'red')
```

### Matriz de priorización
```{r}
ggplotly(graf_mat_est,
         tooltip = c('id','scoring'))
```


## Columna 2

### Empleados retenidos {.no-title}
```{r}
valueBox(empleados_retenidos,
         caption = 'Empleados retenidos',
         color = 'green')
```

### Coste ahorrado {.no-title}
```{r}
valueBox(prettyNum(coste_ahorrado, big.mark = '.',decimal.mark = ',', digits = 0),
         caption = 'Coste ahorrado',
         color = 'blue')
```

### Sensibilidad del modelo por departamento
```{r}
ggplotly(graf_sensi_modelo)
```


## Columna 3
### Empleados con mayor riesgo
```{r}
kable(empleados_en_riesgo)
```

