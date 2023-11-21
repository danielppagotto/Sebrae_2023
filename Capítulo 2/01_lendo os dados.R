library(tidyverse) 
library(data.table)
library(genderBR) 
library(lubridate)
library(tibble)
library(readxl)

setwd("C:/Users/Lapei_Cigets/Desktop/RFB 09.2023/Estabelecimentos")

# lendo dados ----

# Leitura de estabelecimento

estab0 <- fread("estab0.ESTABELE", select=c(1, 2, 3, 4, 5, 6, 7,
                                           11, 12, 20, 21)) 

estab0 <- estab0[V6 == 2 & V20 == "GO" & V4 == 1, ]

estab1 <- fread("estab1.ESTABELE", select=c(1, 2, 3, 4, 5, 6, 7,
                                            11, 12, 20, 21))

estab1 <- estab1[V6 == 2 & V20 == "GO" &  V4 == 1, ]

estab2 <- fread("estab2.ESTABELE", select=c(1, 2, 3, 4, 5, 6, 7,
                                            11, 12, 20, 21))

estab2 <- estab2[V6 == 2 & V20 == "GO" &  V4 == 1, ]

estab3 <- fread("estab3.ESTABELE", select=c(1, 2, 3, 4, 5, 6, 7,
                                            11, 12, 20, 21))

estab3 <- estab3[V6 == 2 & V20 == "GO" &  V4 == 1, ]

estab4 <- fread("estab4.ESTABELE", select=c(1, 2, 3, 4, 5, 6, 7,
                                            11, 12, 20, 21))

estab4 <- estab4[V6 == 2 & V20 == "GO" &  V4 == 1, ]

estab5 <- fread("estab5.ESTABELE", select=c(1, 2, 3, 4, 5, 6, 7,
                                            11, 12, 20, 21))

estab5 <- estab5[V6 == 2 & V20 == "GO" &  V4 == 1, ]

estab6 <- fread("estab6.ESTABELE", select=c(1, 2, 3, 4, 5, 6, 7,
                                            11, 12, 20, 21))

estab6 <- estab6[V6 == 2 & V20 == "GO" &  V4 == 1, ]

estab7 <- fread("estab7.ESTABELE", select=c(1, 2, 3, 4, 5, 6, 7,
                                            11, 12, 20, 21))

estab7 <- estab7[V6 == 2 & V20 == "GO" &  V4 == 1, ]

estab8 <- fread("estab8.ESTABELE", select=c(1, 2, 3, 4, 5, 6, 7,
                                            11, 12, 20, 21))

estab8 <- estab8[V6 == 2 & V20 == "GO" &  V4 == 1, ]

estab9 <- fread("estab9.ESTABELE", select=c(1, 2, 3, 4, 5, 6, 7,
                                            11, 12, 20, 21))

estab9 <- estab9[V6 == 2 & V20 == "GO" &  V4 == 1, ]


estabelecimentos_ativos_go <- 
  rbind(estab0,
        estab1,
        estab2,
        estab3,
        estab4,
        estab5,
        estab6,
        estab7,
        estab8,
        estab9)

estabelecimentos_ativos_go <- 
  estabelecimentos_ativos_go %>% 
  rename(cnpj_basico = V1, cnpj_ordem = V2, 
         cnpj_dv = V3, matriz = V4, nome_fantasia = V5, 
         situacao = V6, data_situacao_atual = V7, 
         data_inicio_atividade = V11, cnae = V12, uf = V20, 
         municipio = V21)

# write.csv(estabelecimentos_ativos_go,"estabelecimentos_ativos_go.csv")

# Empresas leitura -----

# mantendo so ME (microempresas) por enquanto

setwd("C:/Users/Lapei_Cigets/Desktop/RFB 09.2023/Empresas")

emp0 <- fread("emp0.EMPRECSV", select = c(1, 2, 3))
emp0 <- emp0[V3 == 2135, ]

emp1 <- fread("emp1.EMPRECSV", select = c(1, 2, 3))
emp1 <- emp1[V3 == 2135, ]

emp2 <- fread("emp2.EMPRECSV", select = c(1, 2, 3))
emp2 <- emp2[V3 == 2135, ]

emp3 <- fread("emp3.EMPRECSV", select = c(1, 2, 3))
emp3 <- emp3[V3 == 2135, ]

emp4 <- fread("emp4.EMPRECSV", select = c(1, 2, 3))
emp4 <- emp4[V3 == 2135, ]

emp5 <- fread("emp5.EMPRECSV", select = c(1, 2, 3))
emp5 <- emp5[V3 == 2135, ]

emp6 <- fread("emp6.EMPRECSV", select = c(1, 2, 3))
emp6 <- emp6[V3 == 2135, ]

emp7 <- fread("emp7.EMPRECSV", select = c(1, 2, 3))
emp7 <- emp7[V3 == 2135, ]

emp8 <- fread("emp8.EMPRECSV", select = c(1, 2, 3))
emp8 <- emp8[V3 == 2135, ]

emp9 <- fread("emp9.EMPRECSV", select = c(1, 2, 3))
emp9 <- emp9[V3 == 2135, ]


me <- rbind(emp0,
            emp1,
            emp2,
            emp3,
            emp4,
            emp5,
            emp6,
            emp7,
            emp8,
            emp9)

me <- me |> 
        rename(cnpj_basico = V1, razao_social = V2,
               nat_juridica = V3)

#write.csv(me, "microempresas.csv")
#arrow::write_parquet(me, "microempresas.parquet")

# empresas nao-ME e com fins lucrativos


nao_me <- 
  c("2046", "2054", "2062", "2076", 
  "2089", "2097", "2100", "2119", 
  "2127", "2143", "2151", "2160", 
  "2186", "2224", "2232", "2240", 
  "2259", "2267", "2283", "2291", 
  "2305", "2313", "2321", "2330", 
  "2992", "4014", "4022", "4030", 
  "4049", "4080", "4081", "4111", 
  "4120")

emp0 <- fread("emp0.EMPRECSV", select = c(1, 2, 3))
emp0_naome <- emp0 |> filter(V3 %in% nao_me)

emp1 <- fread("emp1.EMPRECSV", select = c(1, 2, 3))
emp1_naome <- emp1|> filter(V3 %in% nao_me)

emp2 <- fread("emp2.EMPRECSV", select = c(1, 2, 3))
emp2_naome <- emp2|> filter(V3 %in% nao_me)

emp3 <- fread("emp3.EMPRECSV", select = c(1, 2, 3))
emp3_naome <- emp3|> filter(V3 %in% nao_me)

emp4 <- fread("emp4.EMPRECSV", select = c(1, 2, 3))
emp4_naome <- emp4|> filter(V3 %in% nao_me)

emp5 <- fread("emp5.EMPRECSV", select = c(1, 2, 3))
emp5_naome <- emp5|> filter(V3 %in% nao_me)

emp6 <- fread("emp6.EMPRECSV", select = c(1, 2, 3))
emp6_naome <- emp6|> filter(V3 %in% nao_me)

emp7 <- fread("emp7.EMPRECSV", select = c(1, 2, 3))
emp7_naome <- emp7|> filter(V3 %in% nao_me)

emp8 <- fread("emp8.EMPRECSV", select = c(1, 2, 3))
emp8_naome <- emp8|> filter(V3 %in% nao_me)

emp9 <- fread("emp9.EMPRECSV", select = c(1, 2, 3))
emp9_naome <- emp9|> filter(V3 %in% nao_me)


nao_me <- rbind(emp0_naome,
            emp1_naome,
            emp2_naome,
            emp3_naome,
            emp4_naome,
            emp5_naome,
            emp6_naome,
            emp7_naome,
            emp8_naome,
            emp9_naome)

nao_me <- nao_me |> 
  rename(cnpj_basico = V1, razao_social = V2,
         nat_juridica = V3)

#write.csv(nao_me, "nao_microempresas.csv")
#arrow::write_parquet(nao_me, "nao_microempresas.parquet")

# Socios ----

setwd("C:/Users/Lapei_Cigets/Desktop/RFB 09.2023/Socios")

socios0 <- fread("socios0.SOCIOCSV", select = c(1, 2, 3, 5, 6))
socios1 <- fread("socios1.SOCIOCSV", select = c(1, 2, 3, 5, 6))
socios2 <- fread("socios2.SOCIOCSV", select = c(1, 2, 3, 5, 6))
socios3 <- fread("socios3.SOCIOCSV", select = c(1, 2, 3, 5, 6))
socios4 <- fread("socios4.SOCIOCSV", select = c(1, 2, 3, 5, 6))
socios5 <- fread("socios5.SOCIOCSV", select = c(1, 2, 3, 5, 6))
socios6 <- fread("socios6.SOCIOCSV", select = c(1, 2, 3, 5, 6))
socios7 <- fread("socios7.SOCIOCSV", select = c(1, 2, 3, 5, 6))
socios8 <- fread("socios8.SOCIOCSV", select = c(1, 2, 3, 5, 6))
socios9 <- fread("socios9.SOCIOCSV", select = c(1, 2, 3, 5, 6))


socios <- rbind(socios0, socios1, socios2, socios3, socios4, socios5, 
                socios6, socios7, socios8, socios9) |>  
  rename(cnpj_basico = V1, id_socio = V2, 
         nome_socio = V3, qualificacao_socio = V5, 
         entrada_sociedade = V6)

# write.csv(socios, "socios.csv")
# arrow::write_parquet(socios, "socios.parquet")

# Tratamentos -------------------------------------------------------------

municipios_serpro <- read_excel("C:/Users/Lapei_Cigets/Desktop/RFB 09.2023/Municipios/municipios_serpro.xlsx") |> 
                        filter(UF == "GO")

municipios_serpro$codigo_arrumado <- as.integer(municipios_serpro$codigo_arrumado)

# tratamento apenas das ME por enquanto 

estabelecimentos_ativos_go <- fread("C:/Users/Lapei_Cigets/Desktop/RFB 09.2023/Estabelecimentos/estabelecimentos_ativos_go.csv")

microempresas <- fread("C:/Users/Lapei_Cigets/Desktop/RFB 09.2023/Empresas/microempresas.csv", 
                       select = c(2, 3, 4))


me_ativas <- estabelecimentos_ativos_go |> 
                  left_join(microempresas, 
                            by = c("cnpj_basico"))

# esse tratamento foi feito para retirar numeros e outros caracteres que eventualmente
# se encontravam no na variavel razao social

me_ativas_tratado <- me_ativas |> 
                      mutate(razao_social_t = gsub('[[:digit:]]+', '', razao_social)) |> 
                      mutate(razao_social_t = gsub('\\.', '', razao_social_t)) |> 
                      mutate(razao_social_t = str_trim(razao_social_t)) |> 
                      mutate(genero = get_gender(razao_social_t))

dist_empresas <- 
  me_ativas_tratado |> 
  filter(nat_juridica == 2135) |> 
  group_by(genero) |> 
  count()
  

distribuicao_municipios <- 
  me_ativas_tratado |>
  filter(nat_juridica == 2135) |> 
  group_by(uf, municipio, genero) |> 
  count() |> 
  filter(genero != "NA") |> 
  inner_join(municipios_serpro,
            by = c("municipio"="codigo_arrumado")) |> 
  mutate(tipo = "microempresa") |> 
  select(-Codigo_TOM_SERPRO)


dist_me_tempo <- me_ativas_tratado |> 
                        mutate(ano = as.character(data_inicio_atividade)) |> 
                        mutate(ano = substr(ano, 1, 4)) |> 
                        filter(genero != "NA") |> 
                        group_by(ano, genero) |> 
                        count()

#setwd("C:/Users/Lapei_Cigets/Desktop/RFB 09.2023/analises")
#write.csv(distribuicao_municipios,"distribuicao_me.csv")

#---- Tratamento das Nao-ME --------------

nao_microempresas <- fread("C:/Users/Lapei_Cigets/Desktop/RFB 09.2023/Empresas/nao_microempresas.csv",
                           select = c(2, 3, 4))

socios <- fread("C:/Users/Lapei_Cigets/Desktop/RFB 09.2023/Socios/socios.csv")

nao_me_ativas_go <- 
  estabelecimentos_ativos_go |> 
  left_join(nao_microempresas, by = c("cnpj_basico")) |> 
  left_join(socios, by = c("cnpj_basico"))

nao_me_gender <- 
  nao_me_ativas_go |> 
  filter(nat_juridica %in% nao_me) 

# quantas empresas? 

nao_me_gender |> 
  group_by(cnpj_basico, nome_fantasia) |> 
  count()

nao_me_gender <- 
  nao_me_gender |> 
  filter(data_situacao_atual == entrada_sociedade) 

nao_me_gender_2 <- 
  nao_me_gender |> 
  distinct(municipio, nome_socio, .keep_all = TRUE)

nao_me_gender_3 <- 
  nao_me_gender_2 |> 
    mutate(genero = get_gender(nome_socio))

nao_me_gender_3 |> 
  group_by(genero) |> 
  count()

nao_me_completa <- 
  nao_me_gender_3 |> 
  group_by(uf, municipio, genero) |> 
  count() |> 
  filter(genero != "NA") |> 
  inner_join(municipios_serpro,
             by = c("municipio"="codigo_arrumado")) |> 
  mutate(tipo = "nao me") |> 
  select(-Codigo_TOM_SERPRO)


dist_nao_me_tempo <- 
  nao_me_gender_3 |> 
    mutate(ano = as.character(data_inicio_atividade)) |> 
    mutate(ano = substr(ano, 1, 4)) |> 
    filter(genero != "NA") |> 
    group_by(ano, genero) |> 
    count()


# juntando ME e não ME ------

base_completa <- 
  rbind(distribuicao_municipios,
      nao_me_completa)

base_completa1 <- 
  base_completa |> 
  group_by(uf, municipio, Municipio, cod_IBGE, genero) |> 
  summarise(total = sum(n)) |> 
  mutate(freq = total/sum(total))

# write.csv(base_completa1, "percentual_feminino_masculino.csv")

# taxa ------------------------------------------------------------------

pop_FEMININA <- read_csv("C:/Users/Lapei_Cigets/Desktop/RFB 09.2023/pop_FEMININA.csv")

hierarquia_municipios <- read_excel("C:/Users/Lapei_Cigets/Desktop/RFB 09.2023/Municipios/hierarquia_municipios.xlsx")

base_completa2 <- 
  base_completa1 |> 
    filter(genero == "Female") |> 
    mutate(cod_IBGE = as.numeric(cod_IBGE)) |> 
    left_join(pop_FEMININA, by = c("cod_IBGE"="COD_MUN")) |> 
    mutate(taxa = (total/POPULACAO) * 100) 

base_completa2 <- read_excel("C:/Users/Lapei_Cigets/Desktop/RFB 09.2023/analises/base_completa.xlsx")

base_completa3 <- 
  base_completa2 |> 
  left_join(hierarquia_municipios, 
            by = c("cod_IBGE"="cod_municipiodv")) |> 
  select(cod_IBGE, genero, municipio_pad, genero, total, freq, 
         POPULACAO, taxa)

#writexl::write_xlsx(base_completa3, "analises_RFB_2023.xlsx")

# grafico evolucao --------------------------------------------------------

rbind(dist_me_tempo,
      dist_nao_me_tempo) |> 
  group_by(ano, genero) |> 
  summarise(total = sum(n)) |> 
  mutate(freq = total/sum(total)) |> 
  mutate(ano = as.numeric(ano)) |> 
  filter(ano >= 1980) |> 
  ggplot(aes(x = ano, y = freq, col = genero)) + 
  geom_line(size = 1) + theme_minimal() + 
  scale_x_continuous(breaks = seq(1980, 2023, 3))



#writexl::write_xlsx(base_completa2, "base_completa.xlsx")






# Arquivo que contem as leituras inicias da base

# arquivos_estab <- c("estab0.ESTABELE","estab1.ESTABELE","estab2.ESTABELE",
#                     "estab3.ESTABELE","estab4.ESTABELE","estab5.ESTABELE",
#                     "estab6.ESTABELE","estab7.ESTABELE","estab8.ESTABELE",
#                     "estab9.ESTABELE")

# leitura_estab <- function(arquivo){
#   
#   dados <- fread(arquivo, select=c(1, 2, 3, 4, 5, 
#                                    6, 7, 11, 12, 
#                                    20, 21))
#   
#   dados <- dados[V6 == 2 & V20 == "GO", ]
#   
#   return(dados)
# }
# 
# base <- data.table()
# 
# for(i in arquivos_estab){
#   
#   
#   dados_lidos <- leitura_estab(i)
#   base_estabelecimentos <- rbind(base, dados_lidos)
#   
#}






