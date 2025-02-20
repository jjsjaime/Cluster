# K-means Cluster Analysis

# ------ Carregando pacotes
pacotes <- c('tidyverse', 'lubridate', 'ggplot2', 'factoextra',
             'openxlsx', 'data.table', 'readr', 'readxl', 'cluster' )

# carregando
for (i in 1:length(pacotes))
{
  pck <- names(installed.packages()[, 1]) == pacotes[i]
  if (length(names(installed.packages()[, 1])[pck]) == 0) {
    install.packages(pacotes[i], repos = 'http://cran.fiocruz.br/')
  }
  suppressPackageStartupMessages(library(pacotes[i],character.only = TRUE)) 
}
rm(list=ls())

# pasta base
setwd("D:/Documentos_2024/Profissional/Projetos/Melhor cidade")

# importando dados 
pop_censo2022 <- read_excel("dados/tabela9845_pop_censo2022.xlsx", skip = 2)
pop_censo2022 <- pop_censo2022 %>% filter(!is.na(Município))
pop_censo2022 <- subset(pop_censo2022, select = c( "Cód.","Município","Ano x Sexo x Idade x Forma de declaração da idade"))
names(pop_censo2022) <- c("CD_MUN", "municipio","populacao_2022")
pop_censo2022$populacao_2022 <- as.numeric(pop_censo2022$populacao_2022)
pop_censo2022$CD_MUN2 <- substr(pop_censo2022$CD_MUN, 1, 6)

leitos <- read_csv("dados/EXTRATO DOS LEITOS.csv", col_types = cols(`CÓDIGO DO MUNICÍPIO` = col_character()))
leitos_mun <- leitos %>% group_by(`CÓDIGO DO MUNICÍPIO` , UF, MUNICÍPIO ) %>% summarise(leitos = sum(`LEITOS EXISTENTES`))
sum(leitos_mun$leitos)

# fazendo primeiro join
# ESCOLHA DO ANALISTA - DEIXAR COM ZERO O MUNICIPIO QUE NAO TEM DADO DE LEITOS DE HOSPITAL
base0 <- left_join(pop_censo2022, leitos_mun,  by = c("CD_MUN2" = "CÓDIGO DO MUNICÍPIO"))
base0$leitos2 <- ifelse(is.na(base0$leitos), 0, base0$leitos)
base0 <- subset(base0, select = c(CD_MUN, municipio, populacao_2022, CD_MUN2, leitos2))
rm(leitos, pop_censo2022, leitos_mun)

# PNUD
pnud <- read_excel("dados/pnud_completa2.xlsx")
pnud <- pnud %>% filter(!is.na(`IDHM 2010`))
names(pnud) <- c("Territorialidades", "Taxa_atividade_18_2010", "%_ocupados_ensino_superior", "rend_med_ocupados_2010",
                 "Esperança_vida_2010","IDHM_2010", "focos_calor_2017", "Média_anos_estudo_2021", 
                 "%_25_anos_mais_ensino_superior_2010", "Expec_anos_estudo_18_2010", "Renda_per_capita_2010")

pnud

# deflacionando renda da população ocupada para reais de dez/2022
fator_bc <- 2.03851610
pnud$rend_med_ocupados_def_2022 <- pnud$rend_med_ocupados_2010*fator_bc

pop_2010 <- read_excel("dados/pop_2010.xlsx", skip = 2)
pop_2010 <- pop_2010 %>% filter(!is.na(Município))
pop_2010 <- subset(pop_2010, select = c( "Cód.","Município"))
names(pop_2010) <- c("CD_MUN", "municipio")

# JOIN
# apos testes chegou-se a conclusao de cruzar com censo 2010
# e o resultado foi de 37 municipios sem match
# foi feita uma correção de nomes desses sem match 
# recomenda-se deixar tudo maiusculo e sem acentos, nao se recomenda ver um a um - NÃO ESCALAVEL

# condicional para corrigir nomes
pop_2010$municipio <- ifelse(pop_2010$municipio == "Eldorado do Carajás (PA)", "Eldorado dos Carajás (PA)",
                             ifelse(pop_2010$municipio =="Ereré (CE)" , "Ererê (CE)",
                             ifelse(pop_2010$municipio == "Quixaba (PB)", "Quixabá (PB)",
                             ifelse(pop_2010$municipio =="Amparo do São Francisco (SE)", "Amparo de São Francisco (SE)",
                             ifelse(pop_2010$municipio =="Araçás (BA)", "Araças (BA)",
                             ifelse(pop_2010$municipio =="Iuiu (BA)", "Iuiú (BA)",
                             ifelse(pop_2010$municipio =="Muquém do São Francisco (BA)", "Muquém de São Francisco (BA)",
                             ifelse(pop_2010$municipio =="Santa Terezinha (BA)", "Santa Teresinha (BA)",
                             ifelse(pop_2010$municipio =="Brazópolis (MG)", "Brasópolis (MG)",
                             ifelse(pop_2010$municipio =="Dona Euzébia (MG)", "Dona Eusébia (MG)",
                             ifelse(pop_2010$municipio =="Passa Vinte (MG)", "Passa-Vinte (MG)",
                             ifelse(pop_2010$municipio =="São Tomé das Letras (MG)", "São Thomé das Letras (MG)",
                             ifelse(pop_2010$municipio =="Atílio Vivácqua (ES)", "Atilio Vivacqua (ES)",
                             ifelse(pop_2010$municipio =="Biritiba Mirim (SP)", "Biritiba-Mirim (SP)",
                             ifelse(pop_2010$municipio =="Embu das Artes (SP)", "Embu (SP)", # mudança pela Lei nº 14.537, 
                                    #de 06 de setembro de 2011
                             ifelse(pop_2010$municipio =="Florínea (SP)", "Florínia (SP)",
                             ifelse(pop_2010$municipio =="São Luiz do Paraitinga (SP)", "São Luís do Paraitinga (SP)",
                             ifelse(pop_2010$municipio =="Grão-Pará (SC)", "Grão Pará (SC)",
                             ifelse(pop_2010$municipio =="Lauro Müller (SC)", "Lauro Muller (SC)",
                             ifelse(pop_2010$municipio =="São Cristóvão do Sul (SC)", "São Cristovão do Sul (SC)",
                             ifelse(pop_2010$municipio =="Restinga Sêca (RS)", "Restinga Seca (RS)",
                             ifelse(pop_2010$municipio =="Vespasiano Corrêa (RS)", "Vespasiano Correa (RS)",
                             ifelse(pop_2010$municipio =="Westfália (RS)", "Westfalia (RS)",
                             ifelse(pop_2010$municipio =="Poxoréu (MT)", "Poxoréo (MT)",
                             ifelse(pop_2010$municipio =="Santo Antônio de Leverger (MT)", "Santo Antônio do Leverger (MT)",
                             ifelse(pop_2010$municipio =="São Luiz do Norte (GO)", "São Luíz do Norte (GO)", 
                             pop_2010$municipio))))))))))))))))))))))))))



# deixando o nome maiusculo
pop_2010$nome <- str_to_upper(pop_2010$municipio)
pnud$nome <- str_to_upper(pnud$Territorialidades) 

pnud2 <- left_join(pop_2010, pnud, by = "nome")
sum(is.na(pnud2$IDHM_2010))
names(pnud2)
pnud2 <- subset(pnud2, select = c("CD_MUN", "Taxa_atividade_18_2010", "%_ocupados_ensino_superior","Esperança_vida_2010", 
                                  "IDHM_2010", "focos_calor_2017", "Média_anos_estudo_2021", 
                                  "%_25_anos_mais_ensino_superior_2010", "Expec_anos_estudo_18_2010", 
                                  "rend_med_ocupados_def_2022"))
pnud2

# unindo pnud com censo 2022
base1 <- left_join(base0, pnud2, by = "CD_MUN")
rm(base0, pnud, pnud2, pop_2010)


# tratamento e join com dados do IDEB
ideb <- read_excel("dados/divulgacao_ensino_medio_municipios_2023.xlsx", skip = 9)
ideb <- subset(ideb, select = c(SG_UF,	CO_MUNICIPIO,	NO_MUNICIPIO,	REDE,VL_OBSERVADO_2019, VL_OBSERVADO_2021, VL_OBSERVADO_2023))
ideb <- ideb %>% filter(REDE == "Pública")

ideb$ideb2 <- if_else(ideb$VL_OBSERVADO_2023 == "-" & ideb$VL_OBSERVADO_2021 != "-", ideb$VL_OBSERVADO_2021,
                      if_else(ideb$VL_OBSERVADO_2023 == "-" & ideb$VL_OBSERVADO_2021 == "-", ideb$VL_OBSERVADO_2019,
                              ideb$VL_OBSERVADO_2023))

ideb %>% filter(ideb2 == "-")                      

ideb$ideb2 <- as.numeric(ideb$ideb2)
sum(is.na(ideb$ideb2))
ideb <- subset(ideb, select = c(CO_MUNICIPIO,	ideb2))
ideb$CO_MUNICIPIO <- as.character(ideb$CO_MUNICIPIO)

# join
base2 <- left_join(base1, ideb,  by = c("CD_MUN" = "CO_MUNICIPIO"))
rm(base1, ideb)

# tratamento e join com dados de homicidios
homicidios <- read_excel("dados/ipeadata_tx_hom.xls") # Taxa de homicídios (100.000 Habitantes) 
homicidios$tx_homicidios <- if_else(is.na(homicidios$`2022`), 0, homicidios$`2022`)
homicidios <- subset(homicidios, select = c(Codigo, tx_homicidios))
names(homicidios) <- c( "Codigo",  "tx_homicidios")

# uma das bases tem mais municipios que a base territorial
# existem dois caminhos: 
# 1) investigar quais municipios tem a mais e usar essa base como referencia
# 2) usar essa base como referencia, mas no final deixar apenas o municipios que 
# tem todos os dados - assim perde 26 municipios que nao tem dodos os dados ou 0,4% 
# 3) Usar base que tem mais municipios e imputar dados por microrregião nos municipios que nao tem dado - OPÇÃO ESCOLHIDA

length(homicidios$Codigo)
homicidios %>% group_by(Codigo) %>% summarise(n = n()) %>% filter(n > 1)

# join 3
base3 <- left_join(homicidios, base2,   by = c("Codigo" = "CD_MUN"))

names(base3)
rm(base2, homicidios)
# imputando a média da microrregião para reduzir os dados faltantes

# importan DTB
dtb_brasil <- read_excel("dados/RELATORIO_DTB_BRASIL_MUNICIPIO.xlsx", skip = 6)
dtb_brasil <- subset(dtb_brasil, select = c( "Código Município Completo", "Nome_Microrregião") )
names(dtb_brasil)

# join 
base3 <- left_join(base3, dtb_brasil,   by = c("Codigo" = "Código Município Completo"))

# calculando dados por microrregiao
estima <- base3 %>% group_by(Nome_Microrregião) %>% 
  summarise(focos_calor_2017_estima = mean(focos_calor_2017, na.rm = T ),
            ideb2_estima = mean(ideb2,  na.rm = T))

estima <- na.omit(estima)
estima

base3 <- left_join(base3, estima, by = "Nome_Microrregião")

sum(is.na(base3$focos_calor_2017));sum(is.na(base3$ideb2));
base3$focos_calor_2017 <- ifelse(is.na(base3$focos_calor_2017), base3$focos_calor_2017_estima, base3$focos_calor_2017)
base3$ideb2 <- ifelse(is.na(base3$ideb2), base3$focos_calor_2017_estima, base3$ideb2_estima)
sum(is.na(base3$focos_calor_2017));sum(is.na(base3$ideb2));

# verificando missings na base
base3 %>% summarise(na_pop = sum(is.na(populacao_2022)),
                    na_homi = sum(is.na(tx_homicidios)),
                    na_leito = sum(is.na(leitos2)),
                    na_ativ = sum(is.na(Taxa_atividade_18_2010)),
                    na_superior1 = sum(is.na(`%_ocupados_ensino_superior`)),
                    na_superior2 = sum(is.na(`%_25_anos_mais_ensino_superior_2010`)),
                    na_estudo = sum(is.na(Média_anos_estudo_2021)),
                    na_expec_estudo = sum(is.na(Expec_anos_estudo_18_2010)),
                    na_vida = sum(is.na(Esperança_vida_2010 )),
                    na_IDHM = sum(is.na(IDHM_2010)),
                    na_calor = sum(is.na(focos_calor_2017 )),
                    na_renda = sum(is.na(rend_med_ocupados_def_2022)),
                    na_ideb = sum(is.na(ideb2)))


# notas: 42 municipios nao tem algum dado isso é 0,7% da base


#index
base <- as.data.frame(base3)
rownames(base) <- base$Codigo
head(base)

# tirando o IDHM e expectativa de estudos
base <- subset(base, select = c(rend_med_ocupados_def_2022, Taxa_atividade_18_2010, populacao_2022, Esperança_vida_2010,
                                 tx_homicidios, leitos2, Média_anos_estudo_2021, `%_ocupados_ensino_superior`,
                                `%_25_anos_mais_ensino_superior_2010`,ideb2, focos_calor_2017))

gc()

#------------------------
# NUMERO OTIMO DE CLUSTER

head(base)

# simplesmente deleta os NAs do dados
base <- na.omit(base)
dados <- scale(base)

# tem duas formas de medir a distancia entre os individuos
# Euclidean and Manhattan distance
# foi usada a  "euclidean"

distance <- get_dist(dados, method = "euclidean")
head(distance)


# ENCONTRANDO O NUMERO ÓTIMO DE CLUSTERS 3 metodos:
# Elbow method, Silhouette, Gap statistic

#--------------------------------------------------------------------------------
# contovelo - o wss tem que ser minimo

set.seed(123)

contovelo <- fviz_nbclust(dados, kmeans, method = "wss",  k.max = 70)
contovelo

cotovelo2 <- as.data.frame(contovelo$data)
diff(cotovelo2$y)


# pelo analise visual e pelo calculo da diferença o cotovelo sugere que 24 clusters otimizam melhor
# o conjunto de dados

#--------------------------------------------------------------------------------
# Average Silhouette Method - tem que maximizar  average silhouette width

# funcao grande
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(dados, centers = k, nstart = 2)
  ss <- silhouette(km.res$cluster, dist(dados))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 300
k.values <- 2:300

# extract avg silhouette for 2-15 clusters
silhueta <- fviz_nbclust(dados, kmeans, method = "silhouette", k.max = 200)
silhueta_data <- as.data.frame(silhueta$data)
max(silhueta_data$y)
gc()

write.xlsx(silhueta_data, "Numero otimo de clusters silhueta.xlsx", col_names = TRUE, format_headers = FALSE)

# Pela analise da silhueta o numero ótimo seria de 2 ou 4 ou 8 clusters otimizam, que é menor que o esperado para o proposito da analise


#--------------------------------------------------------------------------------
# Gap Statistic Method
set.seed(123)
gap_stat <- clusGap(dados, FUN = kmeans, nstart = 25,
                    K.max = 50, B = 10)

# Print the result - ver onde a variavel gap é maxima
print(gap_stat, method = "firstmax")

gap <- as.data.frame(gap_stat$Tab)
fviz_gap_stat(gap_stat)
gc()

# pelo gap seria 2 ou 98 clusters - um numero muito menor ou maior que o esperado para o proposito da analise



#-----------------------------------
# FAZENDO EFETIVAMENTE O CLUSTER
#-----------------------------------
# depois dos metodos escolheu 24 clusters
set.seed(123)
final <- kmeans(dados, centers = 24, nstart = 50) 
print(final)

# visualizacao dos clusters
fviz_cluster(final, data = dados)

final$centers

# Voltar o cluster para formato de data frame
coisa <- as.data.frame(final$cluster)
names(coisa) <- "cluster"

# deixar o codigo do municipio como uma coluna
coisa$cod_mun <- row.names(coisa)
base$cod_mun <- row.names(base)

# fazer join de volta com os dados originais
head(coisa)
head(base)
resultado <- left_join(base, coisa, by = 'cod_mun')
head(resultado)

# analisar os grupos pela media com um group by padrao
centros <- resultado %>%  group_by(cluster) %>% 
  summarise(renda_med = mean(rend_med_ocupados_def_2022),
            Taxa_atividade_18_2010_med = mean(Taxa_atividade_18_2010),
            populacao_2022_med  = mean(populacao_2022),
            Esperança_vida_2010_med  = mean(Esperança_vida_2010 ),
            tx_homicidios_med = mean(tx_homicidios),
            leitos2_med  = mean(leitos2 ),
            Média_anos_estudo_2021_med  = mean(Média_anos_estudo_2021 ),
            ideb2_med = mean(ideb2),
            ocup_superior_med = mean(`%_ocupados_ensino_superior`),
            superior_com_25_anos = mean(`%_25_anos_mais_ensino_superior_2010`),
            calor_med = mean(focos_calor_2017),
            n_cidades = n())

centros


# RANQUEAMENTO DOS clusters

# fatores economicos
centros <- centros %>%  mutate(eco1 = order(order(renda_med, decreasing=T))) %>% arrange(eco1)
centros <- centros %>%  mutate(eco2 = order(order(Taxa_atividade_18_2010_med, decreasing=T))) %>% arrange(eco2)
centros <- centros %>%  mutate(eco3 = order(order(ocup_superior_med, decreasing=T))) %>% arrange(eco3)
# fatores populacionais
centros <- centros %>%  mutate(populacao1 = order(order(populacao_2022_med, decreasing=T))) %>% arrange(populacao1)
centros <- centros %>%  mutate(populacao2 = order(order(superior_com_25_anos, decreasing=T))) %>% arrange(populacao2)
# fatores de saude
centros <- centros %>%  mutate(saude1 = order(order(Esperança_vida_2010_med, decreasing=T))) %>% arrange(saude1)
centros <- centros %>%  mutate(saude2 = order(order(leitos2_med, decreasing=T))) %>% arrange(saude2)
centros <- centros %>%  mutate(saude3 = order(order(calor_med, decreasing=F))) %>% arrange(saude3)
# fatores educacionais
centros <- centros %>%  mutate(educacao1 = order(order(Média_anos_estudo_2021_med, decreasing=T))) %>% arrange(educacao1)
centros <- centros %>%  mutate(educacao2 = order(order(ideb2_med, decreasing=T))) %>% arrange(educacao2)
# fator de segurança
centros <- centros %>%  mutate(seguranca = order(order(tx_homicidios_med, decreasing=F))) %>% arrange(seguranca)


centros <- centros %>% mutate(ordem = eco1 + eco2 + eco3 + populacao1 + populacao2 +
                                saude1 + saude2 + saude3 + educacao1 + educacao2 + seguranca ) %>% arrange(ordem)

# salvando esse dado
write.xlsx(centros, "centros.xlsx", col_names = TRUE, format_headers = FALSE)

# pegando base territorial
base_territorio <- read_excel("dados/AR_BR_RG_UF_RGINT_MES_MIC_MUN_2022.xls", 
                              sheet = "AR_BR_MUN_2022", col_types = c("text", 
                                                                      "text", "text", "text", "text", "text", "numeric"))

# limpeza e tratamento final
base_territorio <- base_territorio %>% filter(!is.na(ID))
base_territorio <- subset(base_territorio, select = c(ID, CD_UF, NM_UF, NM_UF_SIGLA, CD_MUN,  NM_MUN ))

resultado2 <- left_join(resultado, base_territorio,  by = c("cod_mun" = "CD_MUN"))
sum(is.na(resultado2$cluster))

# EXTRAINDO RESULTADOS
# agregando o cluster na base original

#ordenando os dados com dados originais
head(resultado2)
resultado2 <- left_join(resultado2, centros,  by = "cluster")

resultado2 %>%  group_by(cluster) %>% 
  summarise(renda_med = mean(rend_med_ocupados_def_2022),
            Taxa_atividade_18_2010_med = mean(Taxa_atividade_18_2010),
            populacao_2022_med  = mean(populacao_2022),
            Esperança_vida_2010_med  = mean(Esperança_vida_2010 ),
            tx_homicidios_med = mean(tx_homicidios),
            leitos2_med  = mean(leitos2 ),
            Média_anos_estudo_2021_med  = mean(Média_anos_estudo_2021 ),
            ideb2_med = mean(ideb2),
            ocup_superior_med = mean(`%_ocupados_ensino_superior`),
            superior_com_25_anos = mean(`%_25_anos_mais_ensino_superior_2010`),
            calor_med = mean(focos_calor_2017),
            n_cidades = n())

# calcula a distancia com relacao ao centro do grupo
resultado_dist <- resultado2 %>% group_by(cluster) %>%
  mutate(Dist = colMeans(as.matrix(dist(cbind(rend_med_ocupados_def_2022, Taxa_atividade_18_2010, populacao_2022,
                                              Esperança_vida_2010, tx_homicidios, leitos2, Média_anos_estudo_2021,
                                              ideb2, `%_ocupados_ensino_superior`, `%_25_anos_mais_ensino_superior_2010`,
                                              focos_calor_2017)))))

# verifica grupo especifico
resultado_dist %>% filter(cluster == 9)

# teste de ranqueamento interno

resultado_dist %>% 
  group_by(cluster) %>%
  arrange(Dist, .by_group = TRUE)

# ordena e cria o rank interno
resultado_final <- resultado_dist %>%
  group_by(cluster) %>%
  mutate(rank_int = order(order(Dist, decreasing=FALSE))) %>% arrange(rank_int, .by_group = TRUE)

head(resultado_final)

# voltando o dado de IDHM
idh <- subset(base3, select = c(Codigo, IDHM_2010))

resultado_final <- left_join(resultado_final, idh, by = c("cod_mun" = "Codigo"))

resultado_final <- resultado_final %>% mutate(rank2 = ordem +  rank_int) %>% arrange(rank2)

resultado_final['melhor'] <- 1:dim(resultado_final)[1]

# diferenciando as primeiras 100 cidades do resto
resultado_final$top_100 <- ifelse(resultado_final$melhor <= 100, "Top 100", "Resto")

# filtrando as colunas de interesse

filtro <- c("cod_mun", "CD_UF",  "NM_UF", "NM_UF_SIGLA", "NM_MUN", "cluster", "rend_med_ocupados_def_2022", 
           "Taxa_atividade_18_2010", "%_ocupados_ensino_superior", "populacao_2022", 
           "%_25_anos_mais_ensino_superior_2010",   "tx_homicidios","Esperança_vida_2010", "leitos2", 
           "Média_anos_estudo_2021", "focos_calor_2017", "ideb2", "IDHM_2010", "renda_med", "Taxa_atividade_18_2010_med",
           "populacao_2022_med", "Esperança_vida_2010_med", "tx_homicidios_med", "leitos2_med", 
           "Média_anos_estudo_2021_med", "ideb2_med", "ocup_superior_med", "superior_com_25_anos", "calor_med",
           "ordem", "Dist" ,"rank_int", "rank2","melhor", "top_100"  )

resultado_final <- subset(resultado_final, select = filtro)
  

# salva resultado final
resultado_final
write.xlsx(resultado_final, "Melhor cidade v2.xlsx", col_names = TRUE, format_headers = FALSE)

# fim

