###################################################
#              Manejo de datos en R               #
#                                                 #
#  Curso R AEET | 19 Sept 2022 | Elena Quintero   #
#                                                 #
###################################################


# Paquetes
install.packages(c("tidyverse", 
                   "here",
                   "readxl",    
                   "tidylog", 
                   "summarytools"))

                   
library(tidyverse)
tidyverse_packages()

library(readr)          #leer archivos
library(readxl)         #leer archivos excel
library(dplyr)          #manipular datos
library(tidyr)          #ordenar y trasformar datasets 
library(stringr)        #manipular caracteres
library(forcats)        #manipular factores
library(lubridate)      #manipular fechas
library(here)           #refiere la ruta a la carpeta del proyecto
library(tidylog)        #informa sobre operaciones dplyr y tidyr
library(summarytools)   #resume de forma clara y rápida datos numéricos y categóricos
library(knitr)          #reportar datos en varios formatos


#Leer datos

waste <- read_csv(here("data/country_level_data.csv"))

colnames(waste)
dplyr::glimpse(waste)

head(waste)

tail(waste)

# Manejo de datos con tidyverse

## Funciones de `dplyr`

### Ordernar datos por columnas:
  
waste %>%
  arrange(population_population_number_of_people) 


### Ordernar datos por columnas  - *descendiente*:
  
waste %>%
  arrange(desc(population_population_number_of_people)) 

### Ordernar datos por orden jerárquico:
  
waste %>%
  arrange(region_id, country_name) 

### Cambiar nombre columnas:
  
waste %>%
  rename(population = population_population_number_of_people,
         total_waste = total_msw_total_msw_generated_tons_year)

### Seleccionar sólo variables de interés:

waste_select <- waste %>%
  select(iso3c,
         region_id,
         country = country_name, 
         income_id,
         gdp,
         population = population_population_number_of_people,
         total_waste = total_msw_total_msw_generated_tons_year,
         starts_with("composition"))  


glimpse(waste_select)


### (Des)seleccionar variables:

waste_select %>%
  select(-gdp)

### Organizar columnas:
  
waste_select %>%
  relocate(country, .before = iso3c) #<<


### Tablas de resumen de datos:

library(summarytools)
dfSummary(waste_select$region_id)

waste_select %>%
  select(population, gdp) %>%
  dfSummary()



### Extraer valores únicos (niveles) de una(s) variable(s):

waste_select %>%
  distinct(income_id) #<<

base::unique(waste_select$income_id)

# LIC = Low income;
# LMC = Lower middle income;
# UMC = Upper middle income;
# HIC = High income


### Recodificar niveles de una variable:

waste_select %>%
  distinct(region_id)

# - LCN: Latin America & Caribbean 
# - SAS: South Asia 
# - SSF: Sub-Saharan Africa
# - ECS: Europe & Central Asia 
# - MEA: Middle East & North Africa 
# - EAS: East Asia & Pacific
# - NAC: North America

### Recodificar niveles de una variable:

waste_regions <- waste_select %>%
  mutate(region_id = recode(region_id,
                            "LCN" = "Latin_America",
                            "SAS" = "South_Asia",
                            "SSF" = "Sub-Saharan_Africa",
                            "ECS" = "Europe_Central_Asia",
                            "MEA" = "Middle_East_North_Africa",
                            "EAS" = "East_Asia_Pacific",
                            "NAC" = "North_America"))

distinct(waste_regions, region_id)


### Agrupar datos y resumir:
  
waste_regions %>%
  group_by(region_id) %>% #<<
  summarise(total_waste = sum(total_waste, na.rm = TRUE)) #<<

### Crear nueva variable - Ej: transformar basura a millones de toneladas

waste_regions %>%
  group_by(region_id) %>%
  summarise(total_waste = sum(total_waste, na.rm = TRUE)) %>%
  mutate(waste_mtons = total_waste/1000000) #<<


### Filtrar datos:
  
waste_regions %>%
  filter(region_id == "Latin_America")

waste_regions %>%
  filter(region_id == "Europe_Central_Asia" & population <= 1000000)

### Clasificar variables en nuevo factor:
  
waste_regions %>%
  mutate(pop_size = case_when(
    population >= 1000000 ~ "big",
    population < 1000000 & population > 500000 ~ "medium",
    population <= 500000 ~ "small")) %>%
  relocate(pop_size, .before = population)



## Combinar bases de datos con `join`:

# Vamos a darle a nuestro dataset información sobre el continente donde se encuentra cada pais y su localización.

# Leer nuevo dataset con información sobre el continente:
world_data <- read_csv2(here::here("data/world_data.csv"))

glimpse(world_data)


### Seleccionar variables de interés:

continent <- world_data %>%
  select(iso_a3, 
         country_name = name_long, 
         continent)

glimpse(continent)


### Combinar datasets con full_join:

waste_world <- waste_regions %>% 
  rename(iso_a3 = iso3c) %>% #<<
  full_join(continent, by = "iso_a3")

### Combinar datasets con left_join:

waste_world <- waste_regions %>% 
  rename(iso_a3 = iso3c) %>%
  left_join(continent, by = "iso_a3")

### Qué paises se han quedado sin identificar?

waste_world %>%
  filter(is.na(continent)) %>%
  pull(country, iso_a3) #<<

### Buscar los paises que faltan en el dataset de continente:
  
continent %>% 
  filter(country_name %in% 
           c("Channel Islands", "Gibraltar", "Tuvalu", "Kosovo", "Taiwan"))


### Alternativa con library(stringr)

continent %>% 
  filter(str_detect(country_name, "Kosovo|Gibraltar|Tuvalu|Channel Islands|Taiwan"))


### Otro ejemplo - buscar Islas:
 
continent %>% 
  filter(str_detect(country_name, "Island"))


### Corregir un dato:
  
continent_corrected <- continent %>%
  mutate(iso_a3 = ifelse(country_name == "Kosovo", "XKX", iso_a3)) %>% #<<
  mutate(iso_a3 = ifelse(country_name == "Taiwan", "TWN", iso_a3)) #<<

### Volver a combinar dataset:
  
waste_world <- waste_regions %>% 
  rename(iso_a3 = iso3c) %>%
  left_join(continent_corrected, by="iso_a3")


### Ver que paises se han quedado fuera:
 
setdiff(waste_world$iso_a3, continent_corrected$iso_a3)
  
#Si cambiamos el orden de los datos, vemos que países están en *continent_corrected* que no están en *waste_world*

setdiff(continent_corrected$iso_a3, waste_world$iso_a3)


### Ver que paises hay en común:
  
intersect(waste_world$iso_a3, continent_corrected$iso_a3)


glimpse(waste_world)


### Guardar dataset para el próximo día:

write_csv(waste_world, here("data/waste_world.csv"))


### Reestructurar dataset con `library(tidyr)`

glimpse(waste_world)

composition <- waste_world %>%
  pivot_longer(cols = starts_with("composition"), names_to = "composition", values_to = "percent")


composition %>%
  select(country, composition, percent)

composition %>%
  select(country, composition, percent) %>%
  arrange(desc(country))

### Simplificar variables con `library(stringr)`:
  
composition_fix <- composition %>%
  mutate(composition=str_remove(composition, "composition_")) %>% #<<
  mutate(composition=str_remove(composition, "_percent"))  #<<

distinct(composition_fix, composition)

glimpse(composition_fix)

composition_clean <- composition_fix %>%
  select(-country_name) %>%
  relocate(continent, .before = region_id) %>%
  arrange(continent, country)

glimpse(composition_clean)


### Guardar dataset para el próximo día:

write_csv(composition_clean, here("data/waste_world_composition.csv"))



### **Comprobar datos:** asegurar que los porcentajes suman 100%

composition_clean %>%
  group_by(country) %>%
  summarise(per_sum = sum(percent, na.rm = TRUE)) 


composition_clean %>%
  group_by(country) %>%
  summarise(per_sum = sum(percent, na.rm = TRUE)) %>% 
  filter(per_sum < 99.9 | per_sum > 100.1)  %>% # arrange(per_sum) %>%
  pull(per_sum, country) 


### Crear dataset solo para los paises con información completa:

composition_complete <- composition_clean %>%
  group_by(country) %>%
  mutate(per_sum = sum(percent, na.rm = TRUE)) %>% #<<
  filter(per_sum < 99.9 | per_sum > 100.1)

### Atención, en este caso para filtrar el dataset usamos `mutate()` y no `summarise()`, ya que queremos toda la informacion desagregada.


## Ejercicio 1:
# Calcular composición de basura en España:

composition_complete %>%
  filter(country == "Spain") %>%
  select(composition, percent)

## Ejercicio 2:
# Usando el dataset *waste_world* - calcular la población media por regiones en millones de habitantes.

waste_world %>%
  group_by(region_id) %>%
  summarise(pop = mean(population, na.rm = TRUE)) %>%
  mutate(pop = pop / 1000000)


## Ejercicio 3:
# Crear una variable basada en el nivel de basura per cápita y contar el numero de paises en cada grupo.

# Ejemplo:
# - high_waste = >0.6 toneladas de basura por persona al año
# - medium_waste =  0.2 a 0.6 toneladas de basura por persona al año
# - low_waste = <0.2 toneladas de basura por persona al año

waste_world %>%
  select(country, population, total_waste) %>%
  mutate(waste_per_pers = total_waste / population) %>%
  mutate(waste_levels = case_when(
    waste_per_pers >= 0.6 ~ "high_waste",
    waste_per_pers <= 0.2 ~ "low_waste",
    waste_per_pers < 0.6 & waste_per_pers > 0.2 ~ "medium_waste")) %>%
  group_by(waste_levels) %>%
  summarise(n_countries = n())

#Paises que no tienen informacion de *waste_per_pers*
waste_world %>%
  select(country, population, total_waste) %>%
  mutate(waste_per_pers = total_waste/population) %>%
  filter(is.na(waste_per_pers))


## Ejercicio 4:
# Usando la nueva categoria de niveles de basura, contar paises por region y crear una tabla como esta:
  
waste_world %>%
  select(region_id, country, population, total_waste) %>%
  mutate(waste_per_pers = total_waste/population) %>%
  filter(!is.na(waste_per_pers)) %>%
  mutate(waste_levels = case_when(
    waste_per_pers >= 0.6 ~ "high_waste",
    waste_per_pers <= 0.2 ~ "low_waste",
    waste_per_pers < 0.6 & waste_per_pers > 0.2 ~ "medium_waste")) %>%
  group_by(region_id, waste_levels) %>%
  summarise(n_countries=n()) %>%
  pivot_wider(names_from=waste_levels, values_from=n_countries) %>%
  replace(is.na(.), 0) %>%
  kable()


## Ejercicio 5:
# Usando el dataset *waste_world* - calcular el residuo de basura plástica en millones de toneladas (composition_plastic_percent * total_waste) por continente y ordenarlo de mayor a menor.

waste_world %>%
  select(continent, 
         total_waste,
         plastic_per=composition_plastic_percent) %>%
  mutate(plastic_waste=total_waste*plastic_per) %>%
  group_by(continent) %>%
  summarise(plastic = sum(plastic_waste, na.rm=T)/1000000) %>%
  arrange(desc(plastic))
