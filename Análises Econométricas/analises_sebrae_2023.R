
library(readxl)
library(esquisse)
library(readr)
library(tidyverse)

setwd("~/GitHub/Sebrae_2023/Análises Econométricas")

pib <- read_csv("atuacao_pib.csv")

dados <- read_excel("Dados - IDM2.xlsx")

populacao_go_2020 <- read_csv("populacao_go_2020.csv")

quantile(populacao_go_2020$populacao, probs = c(0.20,0.40,0.60,0.90))

taxa <- read_delim("taxa-de-homicidios-faixa-etaria-de-15-29-anos-mulheres.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE) |> 
                   filter(período == "2018")

consulta_crimes <- read_excel("consulta-crimes1.xlsx")

crimes_spread <- 
  consulta_crimes |> 
  spread(variavel, value =  valor)

populacao <- populacao_go_2020 |> 
  mutate(categoria = case_when(populacao < 3265 ~ "até p20",
                               populacao >= 3265 & 
                               populacao < 5215 ~ "p20 a p40",
                               populacao >= 5215 &
                               populacao < 9725 ~ "p40 a p60",
                               populacao >= 9725 &
                               populacao < 46639 ~ "p60 a p90",
                               populacao >= 46640 ~ "acima de p90"))


dados_modelo <- 
  dados |> 
  left_join(populacao, by = c("codigo_6"="cod_municipio")) |> 
  inner_join(crimes_spread, by = c("codigo_6"="ibge")) |> 
  left_join(pib, by = c("codigo_7"="codigo_municipio"))

# Sobre taxa do IDM

#Desse modo, quanto mais o valor observado se 
# aproximar do valor estabelecido como melhor, 
# mais o índice tende para o valor dez (melhor 
# desempenho). Por outro lado, quando o valor 
# observado se aproxima do pior valor (mínimo),
# o índice tende para zero (pior desempenho)

dados_modelo |> 
  group_by(atividade_com_maior_valor_adicionado_bruto) |> 
  summarise(media = mean(taxa_daiane2))


dados_modelo |> 
  ggplot(aes(x = atividade_com_maior_valor_adicionado_bruto,
             y = taxa_daiane)) + geom_label(aes(label = nome1))


ggplot(dados_modelo) +
  aes(x = `Contravenções penais`, y = taxa_daiane_dois) +
  geom_point(shape = "circle", 
             size = 1.5, colour = "#112446") +
  geom_label(aes(label = nome1)) + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() + ggtitle("Taxa - 2019") + 
  facet_wrap(vars(categoria), scales = "free")

ggplot(dados_modelo) +
  aes(x = `Crimes contra a dignidade sexual`, y = taxa_daiane) +
  geom_point(shape = "circle", 
             size = 1.5, colour = "#112446") +
  geom_label(aes(label = nome1)) + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() + ggtitle("Taxa - 2019") + 
  facet_wrap(vars(categoria), scales = "free")

ggplot(dados_modelo) +
  aes(x = `Crimes contra a pessoa`, y = taxa_daiane) +
  geom_point(shape = "circle", 
             size = 1.5, colour = "#112446") +
  geom_label(aes(label = nome1)) + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() + ggtitle("Taxa - 2019") + 
  facet_wrap(vars(categoria), scales = "free")

ggplot(dados_modelo) +
  aes(x = `Crimes contra o patrimônio`, y = taxa_daiane) +
  geom_point(shape = "circle", 
             size = 1.5, colour = "#112446") +
  geom_label(aes(label = nome1)) + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() + ggtitle("Taxa - 2019") + 
  facet_wrap(vars(categoria), scales = "free")

ggplot(dados_modelo) +
  aes(x = `Tráfico de drogas`, y = taxa_daiane) +
  geom_point(shape = "circle", 
             size = 1.5, colour = "#112446") +
  geom_label(aes(label = nome1)) + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() + ggtitle("Taxa - 2019") + 
  facet_wrap(vars(categoria), scales = "free")



ggplot(dados_modelo) +
  aes(x = idm_infra, y = 100*taxa_daiane_dois) +
  geom_point(shape = "circle", 
             size = 1.5, colour = "#112446") +
  geom_label(aes(label = nome1, fill = categoria), size = 2) + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() + ggtitle("Taxa de empreendedorismo vs Infraestrutura") + 
  facet_wrap(vars(categoria)) + ylab("Taxa de empreendedorismo") + 
  xlab("Índice de Infraestrutura")

ggplot(dados_modelo) +
  aes(x = idm_econ, y = 100*taxa_daiane_dois) +
  geom_point(shape = "circle", 
             size = 1.5, colour = "#112446") +
  geom_label(aes(label = nome1, fill = categoria), size = 2) + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() + ggtitle("Taxa de empreendedorismo vs Economia") + 
  facet_wrap(vars(categoria)) + ylab("Taxa de empreendedorismo") + 
  xlab("Índice de Economia")


ggplot(dados_modelo) +
  aes(x = idm_educ, y = 100*taxa_daiane_dois) +
  geom_point(shape = "circle", 
             size = 1.5, colour = "#112446") +
  geom_label(aes(label = nome1, fill = categoria), size = 2) + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() + ggtitle("Taxa de empreendedorismo vs Educação") + 
  facet_wrap(vars(categoria)) + ylab("Taxa de empreendedorismo") + 
  xlab("Índice de Educação")


ggplot(dados_modelo) +
  aes(x = idm_seguranca, y = 100*taxa_daiane_dois) +
  geom_point(shape = "circle", 
             size = 1.5, colour = "#112446") +
  geom_label(aes(label = nome1, fill = categoria), size = 2) + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() + ggtitle("Taxa de empreendedorismo vs Segurança") + 
  facet_wrap(vars(categoria)) + ylab("Taxa de empreendedorismo") + 
  xlab("Índice de Segurança")


ggplot(dados_modelo) +
  aes(x = idm_saude, y = 100*taxa_daiane_dois) +
  geom_point(shape = "circle", 
             size = 1.5, colour = "#112446") +
  geom_label(aes(label = nome1, fill = categoria), size = 2) + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() + ggtitle("Taxa de empreendedorismo vs Saúde") + 
  facet_wrap(vars(categoria)) + ylab("Taxa de empreendedorismo") + 
  xlab("Índice de Saúde")


ggplot(dados_modelo) +
  aes(x = idm_seguranca, y = idm_infra) +
  geom_point(shape = "circle", 
             size = 1.5, colour = "#112446") +
  geom_label(aes(label = nome1, fill = categoria), size = 2) + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() + ggtitle("Segurança vs Infraestrutura") + 
  facet_wrap(vars(categoria)) + ylab("Infraestrutura") + 
  xlab("Índice de Segurança")





# radar -------------------------------------------------------------------

suporte1 <- rep(0,6)
suporte2 <- rep(10,6)

suporte3 <- rbind(suporte1,
                  suporte2)

colnames(suporte3) <- c("idm_econ","idm_educ",
                        "idm_infra","idm_saude",
                        "idm_seguranca","idm_trab")

suporte4 <- dados |> 
  filter(nome1 == "Goiânia") |> 
  select(starts_with("idm"))

grafico_radar <- rbind(suporte3,
                       suporte4) |> 
                 rename(Educação = idm_educ,
                        Economia = idm_econ,
                        Trabalho = idm_trab,
                        Infraestrutura = idm_infra,
                        Saúde = idm_saude,
                        Segurança = idm_seguranca)

radarchart(grafico_radar, axistype = 1,
           pcol=rgb(0.2,0.5,0.5,0.9), 
           pfcol=rgb(0.2,0.5,0.5,0.5), 
           plwd=4,   cglcol="grey", cglty=1, axislabcol="grey", 
           caxislabels=seq(0,10,2.5), cglwd=0.8,
           
           ) 


modelo <- lm(taxa_daiane_dois ~ idm_educ +
             idm_econ + idm_infra + idm_saude+
             idm_seguranca , 
             data = dados_modelo)

summary(modelo)
