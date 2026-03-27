
# AYUDANTÍA 2 · Procesamiento de datos con tidyverse
# Métodos Cuantitativos para la Administración Pública
# FAGOB · 27 de marzo de 2026


# 0. PAQUETES ----
library(pacman)
pacman::p_load(
  tidyverse,    # colección de paquetes para manipulación de datos
  dplyr,        # para manipular datos (select, filter, mutate...)
  car,          # para recodificar variables con recode()
  magrittr,     # para el operador pipe %>%
  sjmisc,       # para explorar y describir variables (frq)
  sjPlot,       # para visualizar la base completa (view_df)
  summarytools  # para resumen detallado de la base (dfSummary)
)

options(scipen = 999) # desactivar notación científica
rm(list = ls())       # limpiar ambiente de trabajo


# 1. INPUT: CARGAR DATOS ----


# Cargar desde GitHub (sin necesidad de descargar el archivo)
# .dta preserva el tipo haven_labelled con sus etiquetas intactas
casen <- haven::read_dta(
  "https://raw.githubusercontent.com/Cristobal-Mejias-G/Clase-2-MC-FAGOB/main/input/casen_fagob.dta"
)

# Alternativa: si tienes el archivo en tu carpeta input/
# casen <- haven::read_dta("input/casen_fagob.dta")



# BLOQUE 1: PRIMERA MIRADA ----


# Resumen completo: distribuciones, NAs, tipos de dato
view(dfSummary(casen, headings = FALSE, method = "render"))

# Vista de variables con etiquetas y distribución visual
view_df(casen, max.len = 100)

# Tabla de frecuencias para variables categóricas
sjmisc::frq(casen$region)
sjmisc::frq(casen$sexo)

# Estadísticos de variables numéricas
mean(casen$o10, na.rm = TRUE)  # promedio horas trabajadas
summary(casen$o10)             # mín, Q1, mediana, media, Q3, máx



# BLOQUE 2: TIPOS DE VARIABLES ----


# Explorar tipos con class()
class(casen$sexo)         
class(casen$edad)
class(casen$o10)
class(casen$yoprcor)

# Lo que veremos en la mayoría de variables:
# [1] "haven_labelled" "vctrs_vctr" "double"
# Leído de abajo hacia arriba:
#   double         → número en el fondo
#   vctrs_vctr     → compatible con tidyverse (detalle técnico)
#   haven_labelled → tiene etiquetas del formato Stata/SPSS pegadas


# 2. PROC: TRATAMIENTO DE DATOS ----

# BLOQUE 3: select() — SELECCIONAR Y RENOMBRAR VARIABLES ----


# Seleccionamos las 16 variables y les ponemos nombres descriptivos
casen_proc <- select(casen,
  # Identificación
  region,
  zona           = area,
  # Demografía
  sexo,
  edad,
  estado_civil   = ecivil,
  # Educación
  nivel_educ     = educc,
  asiste_educ    = e3,
  # Trabajo
  actividad      = activ,
  categ_ocup     = o15,
  horas_trab     = o10,
  contrato       = o19,
  # Ingresos
  ingreso        = yoprcor,
  ingreso_pc     = ypchtotcor,
  # Pobreza y características
  pobreza        = pobreza_severa,
  pueblo_orig    = pueblos_indigenas,
  lugar_nac
)

# Verificar el resultado
glimpse(casen_proc)
names(casen_proc)

# Quitar haven_labelled: convertir a numeric puro para poder recodificar
casen_proc <- mutate(casen_proc, across(everything(), haven::zap_labels))



# BLOQUE 4: filter() — FILTRAR FILAS ----


# Solo personas mayores de 18
adultos <- filter(casen_proc, edad > 18)

# Solo personas ocupadas (actividad == 1, aún como código numérico)
ocupados <- filter(casen_proc, actividad == 1)

# Solo Región Metropolitana (13) y Valparaíso (5)
rm_valpo <- filter(casen_proc, region %in% c(13, 5))

# Verificar siempre con nrow()
nrow(casen_proc)
nrow(adultos)
nrow(ocupados)
nrow(rm_valpo)



# BLOQUE 5: mutate() + recode() + factor() — RECODIFICAR ----


# -- 5.1 Recodificar SEXO --
# car::recode() usa sintaxis de string: "codigo = 'etiqueta'; codigo = 'etiqueta'"
# as.factor = TRUE convierte directo a factor; levels = define el orden

casen_proc <- mutate(casen_proc,
                     sexo = car::recode(sexo,
                                        recodes = c("1 = 'Hombre'; 2 = 'Mujer'"),
                                        as.factor = TRUE,
                                        levels = c("Hombre", "Mujer")))

# Verificar
table(casen_proc$sexo)
class(casen_proc$sexo)


# -- 5.2 Recodificar ACTIVIDAD --
casen_proc <- mutate(casen_proc,
                     actividad = car::recode(actividad,
                                             recodes = c("1 = 'Ocupado/a';
                                                          2 = 'Desocupado/a';
                                                          3 = 'Inactivo/a'"),
                                             as.factor = TRUE,
                                             levels = c("Ocupado/a",
                                                        "Desocupado/a",
                                                        "Inactivo/a")))
table(casen_proc$actividad)


# -- 5.3 Recodificar POBREZA --
casen_proc <- mutate(casen_proc,
                     pobreza = car::recode(pobreza,
                                           recodes = c("1 = 'Pobreza severa';
                                                        2 = 'Pobreza por ingresos';
                                                        3 = 'Pobreza multidimensional';
                                                        4 = 'No pobre'"),
                                           as.factor = TRUE,
                                           levels = c("Pobreza severa",
                                                      "Pobreza por ingresos",
                                                      "Pobreza multidimensional",
                                                      "No pobre")))

table(casen_proc$pobreza)


# Verificar el estado actual de toda la base
glimpse(casen_proc)
sjmisc::frq(casen_proc$sexo)
sjmisc::frq(casen_proc$actividad)
sjmisc::frq(casen_proc$pobreza)



# BLOQUE 6: VALORES PERDIDOS (NA) ----


# -- 6.1 Ver los valores especiales antes de convertir --
table(casen_proc$horas_trab)   # buscar -88 y -99
table(casen_proc$contrato)

# NAs reales que ya existen
colSums(is.na(casen_proc))
sum(is.na(casen_proc))

# -- 6.2 Convertir -88 y -99 a NA --
# Siguiendo el patrón de car::recode(): "-88 = NA; -99 = NA"
# Se hace variable por variable para ser explícito sobre cuáles tienen valores especiales

casen_proc <- mutate(casen_proc,
  ingreso    = car::recode(ingreso,    recodes = c("-88 = NA; -99 = NA")),
  ingreso_pc = car::recode(ingreso_pc, recodes = c("-88 = NA; -99 = NA")),
  horas_trab = car::recode(horas_trab, recodes = c("-88 = NA; -99 = NA")),
  contrato   = car::recode(contrato,   recodes = c("-88 = NA; -99 = NA")),
  categ_ocup = car::recode(categ_ocup, recodes = c("-88 = NA; -99 = NA"))
)

# Verificar
colSums(is.na(casen_proc))
sum(is.na(casen_proc))
colMeans(is.na(casen_proc))  # proporción de NAs por variable


# -- 6.3 Dos objetos: con NAs y sin NAs en ingreso --
# casen_proc ya lo tenemos (con NAs)

# Objeto sin NAs

casen_proc_na <- na.omit(casen_proc)

# ¿Cuántos casos perdemos?
nrow(casen_proc)
nrow(casen_proc_na)

# Continuamos el análisis con casen_proc_na
# Para variables sin NA estructural (edad, sexo...) usamos na.rm = TRUE


# -- 6.4 El problema silencioso: NAs en funciones estadísticas --
mean(casen_proc$ingreso)               # NA <- sin resultado, sin error
mean(casen_proc$ingreso, na.rm = TRUE) # <- correcto
median(casen_proc$horas_trab, na.rm = TRUE)



# BLOQUE 7: case_when() e if_else() — CREAR CATEGORÍAS ----


# -- 7.1 if_else(): condición binaria --
casen_proc_na <- mutate(casen_proc_na,
  adulto_mayor = if_else(edad >= 60, "Adulto mayor", "No adulto mayor"),
  adulto_mayor = factor(adulto_mayor,
                        levels = c("No adulto mayor", "Adulto mayor"))
)

table(casen_proc_na$adulto_mayor)


# -- 7.2 case_when(): múltiples condiciones --
# R asigna el valor del PRIMER TRUE que encuentre — el orden importa
casen_proc_na <- mutate(casen_proc_na,
  tramo_edad = case_when(
    edad < 18              ~ "Menor de edad",
    edad >= 18 & edad < 40 ~ "Adulto joven",
    edad >= 40 & edad < 60 ~ "Adulto",
    edad >= 60             ~ "Adulto mayor"
  ),
  tramo_edad = factor(tramo_edad,
                      levels = c("Menor de edad", "Adulto joven",
                                 "Adulto", "Adulto mayor"))
)

table(casen_proc_na$tramo_edad)


# -- 7.3 case_when() con ingresos --
casen_proc_na <- mutate(casen_proc_na,
  tramo_ingreso = case_when(
    ingreso < 300000                     ~ "Bajo",
    ingreso >= 300000 & ingreso < 700000 ~ "Medio",
    ingreso >= 700000                    ~ "Alto"
  ),
  tramo_ingreso = factor(tramo_ingreso,
                         levels  = c("Bajo", "Medio", "Alto"),
                         ordered = TRUE)
)

table(casen_proc_na$tramo_ingreso)

# 3. OUTPUT: GUARDAR DATOS PROCESADOS ----

# Guardar en formato RDS (formato nativo de R, más eficiente)
saveRDS(casen_proc_na, file = "output/casen_proc.rds")

# Para cargar en la próxima ayudantía:
# casen_proc <- readRDS("output/casen_proc.rds")


# 4. PRÁCTICO 

# EJERCICIO AUTÓNOMO (10 min) ----

# Con casen_proc (el objeto CON NAs), trabaja con:
#   zona, estado_civil, nivel_educ

# 1. Explora con sjmisc::frq() y class()
sjmisc::frq(casen_proc$zona)
sjmisc::frq(casen_proc$estado_civil)
sjmisc::frq(casen_proc$nivel_educ)

# 2. Recodifica zona (1=Urbano, 2=Rural) con recode() → factor
# [Tu código aquí]

# 3. Recodifica nivel_educ (0=Sin educ. → 6=Superior completa)
#    con car::recode() → factor con levels en orden ascendente
# [Tu código aquí]

# 4. Filtra solo personas de zona urbana
# [Tu código aquí]

# 5. Calcula el promedio de ingreso_pc por zona con mean(..., na.rm = TRUE)
# [Tu código aquí]



# CIERRE: EL PIPE %>% ---

# Quitar haven_labelled: convertir a numeric puro para poder recodificar
casen <- mutate(casen, across(everything(), haven::zap_labels))

# Sin pipe (lo que hemos practicado hoy):
casen_proc2 <- select(casen, sexo, edad, actividad = activ,
                      ingreso = yoprcor, pobreza = pobreza_severa)
casen_proc2 <- filter(casen_proc2, edad >= 18)
casen_proc2 <- mutate(casen_proc2,
                      sexo = car::recode(sexo,
                                         recodes = c("1 = 'Hombre'; 2 = 'Mujer'"),
                                         as.factor = TRUE,
                                         levels = c("Hombre", "Mujer")))

# Con pipe (equivalente, más limpio):
casen_proc2 <- casen %>%
  select(sexo, edad, actividad = activ,
         ingreso = yoprcor, pobreza = pobreza_severa) %>%
  filter(edad >= 18) %>%
  mutate(sexo = car::recode(sexo,
                             recodes = c("1 = 'Hombre'; 2 = 'Mujer'"),
                             as.factor = TRUE,
                             levels = c("Hombre", "Mujer")))

