### removemos todo 
# Limpiar el entorno eliminando todas las variables
rm(list = ls())

pacman::p_load("tidyverse", "magrittr", "wid", "sjmisc", "readxl", "plm", "ggplo2")

### Descarga de datos 
data<-wid::download_wid(
       indicators = "ghweal",
       areas = "all",
       years = "all",
       perc = "all",
       ages = "992",
       pop = "j",
       metadata = FALSE,
       include_extrapolations = TRUE,
       verbose = FALSE
   )

# Crear la nueva variable con el conteo de valores únicos en 'country'
data_con_conteo <- data %>%
  mutate(conteo_valores_unicos = n_distinct(country))

# Calcular la media, desviación estándar, máximo y mínimo agrupando por 'country'
resultados_por_pais <- data %>%
  group_by(country) %>%
  summarise(
    media = mean(value),
    desviacion = sd(value),
    maximo = max(value),
    minimo = min(value)
  )

## Cargamos datos de crecimiento 

crec<-read_excel("data/FebPwtExport8192023.xlsx", sheet=2) %>% select(c(year=YearCode, country=RegionCode, PIB=AggValue)) %>% 
  mutate(country = case_when(
    country == "DZA" ~ "DZ", country == "AGO" ~ "AO", country == "BEN" ~ "BJ", country == "BWA" ~ "BW", country == "BFA" ~ "BF", country == "BDI" ~ "BI", country == "CMR" ~ "CM", country == "CAF" ~ "CF",
    country == "TCD" ~ "TD", country == "COM" ~ "KM", country == "DJI" ~ "DJ", country == "EGY" ~ "EG", country == "GNQ" ~ "GQ", country == "ETH" ~ "ET", country == "GAB" ~ "GA", country == "GMB" ~ "GM",
    country == "GHA" ~ "GH", country == "GIN" ~ "GN", country == "GNB" ~ "GW", country == "CIV" ~ "CI", country == "KEN" ~ "KE", country == "LSO" ~ "LS",country == "LBR" ~ "LR", country == "MDG" ~ "MG", country == "MWI" ~ "MW",
    country == "MLI" ~ "ML", country == "MRT" ~ "MR", country == "MUS" ~ "MU", country == "MAR" ~ "MA", country == "MOZ" ~ "MZ", country == "NAM" ~ "NA", country == "NER" ~ "NE",
    country == "NGA" ~ "NG", country == "RWA" ~ "RW", country == "STP" ~ "ST", country == "SEN" ~ "SN", country == "SYC" ~ "SC", country == "SLE" ~ "SL", country == "ZAF" ~ "ZA",
    country == "SDN" ~ "SD", country == "TZA" ~ "TZ", country == "TGO" ~ "TG", country == "TUN" ~ "TN", country == "UGA" ~ "UG", country == "ZMB" ~ "ZM", country == "ZWE" ~ "ZW", country == "ARM" ~ "AM",
    country == "AZE" ~ "AZ", country == "BHR" ~ "BH", country == "BGD" ~ "BD", country == "BTN" ~ "BT", country == "BRN" ~ "BN", country == "KHM" ~ "KH", country == "CHN" ~ "CN", country == "GEO" ~ "GE",
    country == "HKG" ~ "HK", country == "IND" ~ "IN", country == "IDN" ~ "ID", country == "IRN" ~ "IR", country == "IRQ" ~ "IQ", country == "ISR" ~ "IL",country == "JPN" ~ "JP", country == "JOR" ~ "JO", country == "KAZ" ~ "KZ",
    country == "KWT" ~ "KW", country == "KGZ" ~ "KG", country == "LAO" ~ "LA", country == "LBN" ~ "LB", country == "MAC" ~ "MO", country == "MYS" ~ "MY", country == "MDV" ~ "MV",
    country == "MNG" ~ "MN", country == "MMR" ~ "MM", country == "NPL" ~ "NP", country == "OMN" ~ "OM", country == "PAK" ~ "PK", country == "PHL" ~ "PH", country == "QAT" ~ "QA",
    country == "SAU" ~ "SA", country == "SGP" ~ "SG", country == "KOR" ~ "KR", country == "LKA" ~ "LK", country == "PSE" ~ "PS", country == "SYR" ~ "SY", country == "TWN" ~ "TW", country == "TJK" ~ "TJ",
    country == "THA" ~ "TH", country == "TUR" ~ "TR", country == "TKM" ~ "TM", country == "ARE" ~ "AE", country == "UZB" ~ "UZ", country == "VNM" ~ "VN", country == "YEM" ~ "YE", country == "ALB" ~ "AL",
    country == "AUT" ~ "AT", country == "BLR" ~ "BY", country == "BEL" ~ "BE", country == "BIH" ~ "BA", country == "BGR" ~ "BG", country == "HRV" ~ "HR",country == "CYP" ~ "CY", country == "CZE" ~ "CZ", country == "DNK" ~ "DK",
    country == "EST" ~ "EE", country == "FIN" ~ "FI", country == "FRA" ~ "FR", country == "DEU" ~ "DE", country == "GRC" ~ "GR", country == "HUN" ~ "HU", country == "ISL" ~ "IS",
    country == "IRL" ~ "IE", country == "ITA" ~ "IT", country == "LVA" ~ "LV", country == "LTU" ~ "LT", country == "LUX" ~ "LU", country == "MLT" ~ "MT", country == "MDA" ~ "MD",
    country == "MNE" ~ "ME", country == "NLD" ~ "NL", country == "MKD" ~ "MK", country == "NOR" ~ "NO", country == "POL" ~ "PL", country == "PRT" ~ "PT", country == "ROU" ~ "RO", country == "RUS" ~ "RU",
    country == "SRB" ~ "RS", country == "SXM" ~ "SX", country == "SVK" ~ "SK", country == "SVN" ~ "SI", country == "ESP" ~ "ES", country == "SWE" ~ "SE", country == "CHE" ~ "CH", country == "UKR" ~ "UA",
    country == "GBR" ~ "GB", country == "AIA" ~ "AI", country == "ATG" ~ "AG", country == "ABW" ~ "AW", country == "BHS" ~ "BS", country == "BRB" ~ "BB",country == "BLZ" ~ "BZ", country == "BMU" ~ "BM", country == "VGB" ~ "VG",
    country == "CAN" ~ "CA", country == "CRI" ~ "CR", country == "CUW" ~ "CW", country == "DMA" ~ "DM", country == "DOM" ~ "DO", country == "SLV" ~ "SV", country == "GRD" ~ "GD",
    country == "GTM" ~ "GT", country == "HTI" ~ "HT", country == "HND" ~ "HN", country == "JAM" ~ "JM", country == "MEX" ~ "MX", country == "MSR" ~ "MS", country == "NIC" ~ "NI",
    country == "PAN" ~ "PA", country == "KNA" ~ "KN", country == "LCA" ~ "LC", country == "VCT" ~ "VC", country == "TTO" ~ "TT", country == "TCA" ~ "TC", country == "USA" ~ "US",
    country == "AUS" ~ "AU", country == "FJI" ~ "FJ", country == "NZL" ~ "NZ", country == "ARG" ~ "AR", country == "BOL" ~ "BO", country == "BRA" ~ "BR", country == "CHL" ~ "CL",
    country == "COL" ~ "CO", country == "ECU" ~ "EC", country == "GUY" ~ "GY", country == "PRY" ~ "PY", country == "PER" ~ "PE", country == "SUR" ~ "SR", country == "URY" ~ "UY", country == "VEN" ~ "VE",
    TRUE ~ country  # Mantener los valores que no se modifican
  ))


## Mezclamos datos de PIB y Wealth Gini 

df<- merge(crec, data, by=c("country", "year"), all.x = TRUE) %>% select(c("country", "year", "PIB", "value"))

## Creamos variable de tasa de crecimiento de PIB 
df$PIB=as.numeric(df$PIB)



# Crear una nueva variable 'tasa_crecimiento'
df <- df %>% 
  arrange(country, year) %>%  # Ordenar por país y año
  group_by(country) # %>% filter(year>=1995 & year<=2019)


# Calcular la tasa de crecimiento usando diff() y ave()
df$tas_crecimiento <- with(df, ave(PIB, country, FUN = function(x) c(NA, diff(x) / x[-length(x)])))

df_years <- df %>% filter(year>=1995 & year<=2019) %>% na.omit()

## Creamos una base de datos sólo con países OCDE

df_ocde<- df_years %>% filter(country %in% c("CA", "US", "GB", "DK", "IS", "NO", "TR", "ES", "PT", "FR", "IE",
                                             "BE", "DE", "GR", "SE", "CH", "AT", "NL", "LU", "IT", "JP", "FI", 
                                             "AU", "NZ", "MX", "CZ", "HU", "PL", "KR", "SK", "CL", "SI", "IL", 
                                             "EE", "LV", "LT", "CO", "CR")) %>% rename(wealth_gini=value, tasa_crecimiento=tas_crecimiento)




library(stargazer)
library(xtable)
# Calcular estadísticas descriptivas ginis de riqueza 
desc_stats_wealth <- df_ocde %>%
  summarise(
            Media_wealth_gini = mean(wealth_gini),
            DesvEstandar_wealth_gini = sd(wealth_gini),
            Min_wealth_gini = min(wealth_gini),
            Max_wealth_gini = max(wealth_gini)) %>% filter(country %in% c("MX", "CL", "US", "TR", 
                                                                                    "FR", "IT", "NO", "DE", "SE", "NZ", 
                                                                                    "LT", "LV", "KR", "JP")) 
desc_stats_crec <- df_ocde %>%
  summarise(
    Media_crecimiento = mean(tasa_crecimiento),
    Desv_crecimiento = sd(tasa_crecimiento),
    Min_crecimiento = min(tasa_crecimiento),
    Max_crecimiento = max(tasa_crecimiento)) %>% filter(country %in% c("MX", "CL", "US", "TR", 
                                                                            "FR", "IT", "NO", "DE", "SE", "NZ", 
                                                                            "LT", "LV", "KR", "JP"))

# Convertir a tabla LaTeX con xtable
table_latex <- xtable(desc_stats_wealth, caption = "Estadísticas descriptivas",
                      label = "tabla:desc_stats_wealth")

table_latex_crec <- xtable(desc_stats_crec, caption = "Estadísticas descriptivas",
                      label = "tabla:desc_stats_wealth")

# Exportar a archivo LaTeX

print(table_latex, file = "output/desc_stats_wealth.tex", include.rownames = FALSE)

print(table_latex_crec, file = "output/desc_stats_crec.tex", include.rownames = FALSE)


# summary_table <- df_ocde %>% 
#   summarise(N = n(),
#             Media_value = mean(wealth_gini),
#             DesvEstandar_value = sd(wealth_gini),
#             Minimo_value = min(wealth_gini),
#             Maximo_value = max(wealth_gini),
#             Media_tasa_crecimiento = mean(tas_crecimiento),
#             DesvEstandar_tasa_crecimiento = sd(tas_crecimiento),
#             Minimo_tasa_crecimiento = min(tas_crecimiento),
#             Maximo_tasa_crecimiento = max(tas_crecimiento))
# 
# 
# 
# # Exportar la tabla a LaTeX
# stargazer(summary_table, title = "Estadísticas descriptivas",
#           align = TRUE, # Alinear los números en la tabla
#           digits = 2,   # Número de decimales
#           out = "tabla_descriptiva.tex", # Nombre del archivo LaTeX de salida
#           header = FALSE) # No incluir encabezado
# 
# 



### Gráfico de dispersión 

library(ggplot2)

gg=ggplot(df_ocde, aes(x = wealth_gini, y = tasa_crecimiento)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Gráfico bivariado: Tasa de Crecimiento vs. Gini de riqueza",
       x = "Gini de riqueza",
       y = "Tasa de Crecimiento") 


# Guardar el gráfico en formato PNG
ggsave("output/grafico_bivariado.png", plot = gg, width = 8, height = 6, dpi = 300)

## Regresión datos de panel efecto fijo

# Realizar regresión de efectos fijos
reg_model <- plm(tasa_crecimiento ~ wealth_gini, data = df_ocde, model = "within")

# Mostrar resultados de la regresión
summary(reg_model)



# Generar tabla de resumen en formato LaTeX
reg_summary <- stargazer(reg_model, title = "Resultados de la regresión de efectos fijos",
                         align = TRUE, digits = 4, out = "reg_results.tex")

# Exportar la tabla a un archivo LaTeX
cat(reg_summary, file = "reg_results.tex")

