# Script para leitura de bancos de dados diversos para geração de um data frame de uma única linha referente as informações do estado do aluno

# Ao receber este script esqueleto colocá-lo no repositório LOCAL Extensao, que deve ter sido clonado do GitHub
# Enviar o script esqueleto para o repositório REMOTO com o nome extensao-esqueleto.R

# Para realizar as tarefas da ETAPA 1, ABRIR ANTES uma branch de nome SINASC no main de Extensao e ir para ela
# Após os alunos concluírem a ETAPA 1 a professora orientará fazer o merge into main e depois abrir outro branch. Aguarde...


####################################
# ETAPA 1: BANCO DE DADOS DO SINASC
####################################

# A ALTERAÇÃO DO SCRIPT ESQUELETO - ETAPA 1 - DEVERÁ SER FEITA DENTRO DA BRANCH SINASC

# Tarefa 1. Leitura do banco de dados do SINASC 2015  com 3017668 linhas e 61 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sinasc

### Pacotes utilizados

library(dplyr)

### Leitura do banco de dados

dados_sinasc = read.csv("SINASC_2015.csv", header = T, sep = ";" )

# Tarefa 2. Reduzir dados_sinasc apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sinasc_1
# as colunas serão 1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61
# nomes das respectivas variáveis: CONTADOR, CODMUNNASC, LOCNASC, IDADEMAE, ESTCIVMAE, CODMUNRES, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, PESO, IDANOMAL, ESCMAE2010, RACACORMAE, SEMAGESTAC, CONSPRENAT, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK

### Reduzindo as colunas

dados_sinasc_1 = dados_sinasc[, c(1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61)]

# Tarefa 3. Reduzir dados_sinasc_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinasc_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 

# observar abaixo o número de nascimentos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 27918     12: 16980     13: 80097     14: 11409     15: 143657    16: 15750      17: 25110
# 21: 117564    22: 49253     23: 132516    24: 49099     25: 59089     26: 145024     27: 52257     28: 34917     29: 206655
# 31: 268305    32: 56941     33: 236960    35: 634026     
# 41: 160947    42: 97223     43: 148359
# 50: 44142     51: 56673     52: 100672    53: 46122 

# Exportar o arquivo com o nome dados_sinasc_2.csv

### Reduzindo as linhas para SP (código 35)

dados_sinasc_2 = dados_sinasc_1 %>% 
  filter(substr(CODMUNRES, 1, 2) == "35")

### Gerando arquivo com dados apenas de SP

write.csv(dados_sinasc_2, "dados_sinasc_2.csv", row.names = FALSE)

# Ao concluir a Tarefa 3 da Etapa 1 commite e envie para o repositório REMOTO o script e dados_sinasc_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"


# Tarefa 4. Verificar em dados_sinasc_2 a frequência das categorias das seguintes variáveis: LOCNASC, ESTCIVMAE, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, IDANOMAL, ESCMAE2010, RACACORMAE, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK

### Verificando frequência das categorias

locnasc_f = table(dados_sinasc_2$LOCNASC)

estcivmae_f = table(dados_sinasc_2$ESTCIVMAE)

gestacao_f = table(dados_sinasc_2$GESTACAO)

gravidez_f = table(dados_sinasc_2$GRAVIDEZ)

parto_f = table(dados_sinasc_2$PARTO)

sexo_f = table(dados_sinasc_2$SEXO)

racacor_f = table(dados_sinasc_2$RACACOR)

idanomal_f = table(dados_sinasc_2$IDANOMAL)

escmae2010_f = table(dados_sinasc_2$ESCMAE2010)

racacormae_f = table(dados_sinasc_2$RACACORMAE)

tpapresent_f = table(dados_sinasc_2$TPAPRESENT)

tprobson_f = table(dados_sinasc_2$TPROBSON)

paridade_f = table(dados_sinasc_2$PARIDADE)

kotelchuck_f = table(dados_sinasc_2$KOTELCHUCK)


# Tarefa 5. Atribuir para cada variável de dados_sinasc_2 como sendo NA a categoria de "Não informado ou Ignorado", geralmente com código 9
# KOTELCHUCK = 9 significa "não informado"   TPROBSON = 11 significa "não classificado por falta de informação"
# veja o dicionário do SINASC para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADEMAE, APGAR5 e PESO e SEMAGESTAC verificar se existem valores como 99 para NA

### Atribuindo NA p/ categorias de "Não informado"/"Ignorado"

dados_sinasc_2 = dados_sinasc_2 %>%
  mutate(LOCNASC = na_if(LOCNASC, 9))
table(dados_sinasc_2$LOCNASC)

dados_sinasc_2 = dados_sinasc_2 %>%
  mutate(IDADEMAE = na_if(IDADEMAE, 99))
table(dados_sinasc_2$IDADEMAE)

dados_sinasc_2 = dados_sinasc_2 %>%
  mutate(ESTCIVMAE = na_if(ESTCIVMAE, 9))
table(dados_sinasc_2$ESTCIVMAE)

dados_sinasc_2 = dados_sinasc_2 %>%
  mutate(GESTACAO = na_if(GESTACAO, 9))
table(dados_sinasc_2$GESTACAO)

dados_sinasc_2 = dados_sinasc_2 %>%
  mutate(GRAVIDEZ = na_if(GRAVIDEZ, 9))
table(dados_sinasc_2$GRAVIDEZ)

dados_sinasc_2 = dados_sinasc_2 %>%
  mutate(PARTO = na_if(PARTO, 9))
table(dados_sinasc_2$PARTO)

dados_sinasc_2 = dados_sinasc_2 %>%
  mutate(SEXO = na_if(SEXO, 0))
table(dados_sinasc_2$SEXO)

dados_sinasc_2 = dados_sinasc_2 %>%
  mutate(APGAR5 = na_if(APGAR5, 99))
table(dados_sinasc_2$APGAR5)

dados_sinasc_2 = dados_sinasc_2 %>%
  mutate(IDANOMAL = na_if(IDANOMAL, 9))
table(dados_sinasc_2$IDANOMAL)

dados_sinasc_2 = dados_sinasc_2 %>%
  mutate(ESCMAE2010 = na_if(ESCMAE2010, 9)) 
table(dados_sinasc_2$ESCMAE2010)

dados_sinasc_2 = dados_sinasc_2 %>%
  mutate(TPAPRESENT = na_if(TPAPRESENT, 9))
table(dados_sinasc_2$TPAPRESENT)

dados_sinasc_2 = dados_sinasc_2 %>%
  mutate(TPROBSON = na_if(TPROBSON, 11))
table(dados_sinasc_2$TPROBSON)

dados_sinasc_2 = dados_sinasc_2 %>%
  mutate(KOTELCHUCK = na_if(KOTELCHUCK, 9))
table(dados_sinasc_2$KOTELCHUCK)

# Tarefa 6. Atribuir legendas para as categorias das variáveis investigadas na etapa 4.
# Exemplo: dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), 
# labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado",  
# "Mais que adequado")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados

### Atribuindo legendas para as variáveis

str(dados_sinasc_2$LOCNASC)
dados_sinasc_2$LOCNASC = factor(dados_sinasc_2$LOCNASC, levels = c(1, 2, 3, 4),
                                labels = c("Hospital", "Outros estabelecimentos de saúde", "Domicílio", 
                                           "Outros"))

str(dados_sinasc_2$ESTCIVMAE)
dados_sinasc_2$ESTCIVMAE = factor(dados_sinasc_2$ESTCIVMAE, levels = c(1, 2, 3, 4, 5),
                                  labels = c("Solteira", "Casada", "Viúva", "Separada judicialmente/divorciada", 
                                             "União estável"))

str(dados_sinasc_2$GESTACAO)
dados_sinasc_2$GESTACAO = factor(dados_sinasc_2$GESTACAO, levels = c(1, 2, 3, 4, 5, 6),
                                 labels = c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas",
                                            "32 a 36 semanas", "37 a 41 semanas", "42 semanas e mais"))

str(dados_sinasc_2$GRAVIDEZ)
dados_sinasc_2$GRAVIDEZ = factor(dados_sinasc_2$GRAVIDEZ, levels = c(1, 2, 3),
                                 labels = c("Única", "Dupla", "Tripla ou mais"))

str(dados_sinasc_2$PARTO)
dados_sinasc_2$PARTO = factor(dados_sinasc_2$PARTO, levels = c(1, 2),
                              labels = c("Vaginal", "Cesário"))

str(dados_sinasc_2$SEXO)
dados_sinasc_2$SEXO = factor(dados_sinasc_2$SEXO, levels = c(1, 2),
                             labels = c("Masculino", "Feminino"))

str(dados_sinasc_2$RACACOR)
dados_sinasc_2$RACACOR = factor(dados_sinasc_2$RACACOR, levels = c(1, 2, 3, 4, 5),
                                labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))

str(dados_sinasc_2$IDANOMAL)
dados_sinasc_2$IDANOMAL = factor(dados_sinasc_2$IDANOMAL, levels = c(1, 2),
                                 labels = c("Sim", "Não"))

str(dados_sinasc_2$ESCMAE2010)
dados_sinasc_2$ESCMAE2010 = factor(dados_sinasc_2$ESCMAE2010, levels = c(0, 1, 2, 3, 4, 5),
                                   labels = c("Sem escolaridade", "Fundamental I (1a a 4a série)", 
                                              "Fundamental II (5a a 8a série)", "Médio (antigo 2o Grau)",
                                              "Superior incompleto", "Superior completo"))

str(dados_sinasc_2$RACACORMAE)
dados_sinasc_2$RACACORMAE = factor(dados_sinasc_2$RACACORMAE, levels = c(1, 2, 3, 4, 5),
                                   labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))

str(dados_sinasc_2$TPAPRESENT)
dados_sinasc_2$TPAPRESENT = factor(dados_sinasc_2$TPAPRESENT, levels = c(1, 2, 3),
                                   labels = c("Cefálico", "Pélvica ou podálica", "Transversa"))

str(dados_sinasc_2$TPROBSON)
dados_sinasc_2$TPROBSON = factor(dados_sinasc_2$TPROBSON, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                 labels = c("Grupo 1", "Grupo 2", "Grupo 3", "Grupo 4", "Grupo 5", "Grupo 6",
                                            "Grupo 7", "Grupo 8", "Grupo 9", "Grupo 10"))

str(dados_sinasc_2$PARIDADE)
dados_sinasc_2$PARIDADE = factor(dados_sinasc_2$PARIDADE, levels = c(1, 0),
                                 labels = c("Multípara", "Nulípara"))

# Tarefa 7. Categorizar as variáveis IDADEMAE, PESO e APGAR5 e criar variáveis referentes ao deslocamento materno (peregrinação) e estado civil
# nova variável: dados_sinasc_2$F_PESO com PESO: < 2500: Baixo peso, >=2500 e < 4000: Peso normal, >= 4000: Macrossomia
# nova variável dados_sinasc_2$F_IDADE com IDADEMAE: <15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+
# nova variável dados_sinasc_2$F_APGAR5 com APGAR5: < 7: Baixo, >= 7: Normal
# Atenção para casos de NA em IDADEMAE, PESO e APGAR5

# nova variável: dados_sinasc_2$PERIG: Não: CODMUNNASC igual a CODMUNRES, Sim: CODMUNNASC diferente de CODMUNRES
# nova variável: dados_sinasc_2$ESTCIV: Sem companheiro: ESTCIVMAE 1, 3 ou 4, Com companheiro: ESTCIVMAE 2 ou 5
# Ao categorizar as variáveis, garantir que sejam transformadas em tipo fator

### Categorizando as variáveis PESO, IDADEMAE e APGAR5

dados_sinasc_2 = dados_sinasc_2 %>% 
  mutate(F_PESO = case_when(
    PESO < 2500 ~ "Baixo peso",
    PESO < 4000 ~ "Peso normal",
    PESO >= 4000 ~ "Macrossomia"))
dados_sinasc_2 = dados_sinasc_2 %>% 
  mutate(F_PESO = factor(F_PESO, levels = c("Baixo peso", "Peso normal", 
                                            "Macrossomia")))

dados_sinasc_2 = dados_sinasc_2 %>% 
  mutate(F_IDADE = case_when(
    IDADEMAE < 15 ~ "<15",
    IDADEMAE < 19 ~ "15-19",
    IDADEMAE < 24 ~ "20-24",
    IDADEMAE < 29 ~ "25-29",
    IDADEMAE < 34 ~ "30-34",
    IDADEMAE < 39 ~ "35-39",
    IDADEMAE < 44 ~ "40-44",
    IDADEMAE <= 49 ~ "45-49",
    IDADEMAE > 49 ~ "50+"))
dados_sinasc_2 = dados_sinasc_2 %>% 
  mutate(F_IDADE = factor(F_IDADE, levels = c("<15", "15-19", "20-24", "25-29", "30-34", 
                                              "35-39","40-44", "45-49", "50+")))

dados_sinasc_2 = dados_sinasc_2 %>% 
  mutate(F_APGAR5 = case_when(
    APGAR5 < 7 ~ "Baixo",
    APGAR5 >= 7 ~ "Normal"))
dados_sinasc_2 = dados_sinasc_2 %>% 
  mutate(F_APGAR5 = factor(F_APGAR5, levels = c("Baixo", "Normal")))

### Criando variáveis de deslocamento materno e estado civil

dados_sinasc_2 = dados_sinasc_2 %>% 
  mutate(PERIG = ifelse(CODMUNNASC == CODMUNRES, "Não", "Sim"))
dados_sinasc_2 = dados_sinasc_2 %>% 
  mutate(PERIG = factor(PERIG, levels = c("Não", "Sim")))

dados_sinasc_2 = dados_sinasc_2 %>% 
  mutate(ESTCIV = case_when(
    ESTCIVMAE %in% c("Solteira", "Viúva", "Separada judicialmente/divorciada") ~ "Sem companheiro",
    ESTCIVMAE %in% c("Casada", "União estável") ~ "Com companheiro"))
dados_sinasc_2 = dados_sinasc_2 %>% 
  mutate(ESTCIV = factor(ESTCIV, levels = c("Sem companheiro", "Com companheiro")))


# Tarefa 8. Agregar ao banco de dados_sinasc_2 as informações PESO_P10 e PESO_P90 a partir de Tabela_PIG_Brasil.csv
# a Tabela PIG informa P10 e P90 dos pesos, de acordo com a idade gestacional
# criar nova variável referente ao peso, de acordo com a idade gestacional, conforme indicado abaixo
# nova variável apenas para casos de GRAVIDEZ Única: dados_sinasc_2$F_PIG: PIG: PESO < PESO_P10, AIG: PESO_P10 <= PESO <= PESO_P90, GIG: PESO > PESO_P90
# Atenção para casos de NA em SEMAGESTAC, PESO ou SEXO. Lembre-se também que em dados_sinasc_2 SEXO está como fator com as categorias Feminino e Masculino.

### Criando variáveis de classificação do peso (PIG, GIG, AIG)

tabela_pig = read.csv("Tabela_PIG_Brasil.csv", header = T, sep = ";")
tabela_pig$SEXO = factor(tabela_pig$SEXO, levels = c("Masculino", "Feminino"))
dados_sinasc_2 = merge(dados_sinasc_2, tabela_pig, by = c("SEMAGESTAC", "SEXO"), all.x = T)
dados_sinasc_2 = dados_sinasc_2 %>% filter(GRAVIDEZ == "Única") %>%
  mutate(F_PIG = case_when(
    PESO < PESO_P10 ~ "PIG",
    PESO_P10 <= PESO & PESO <= PESO_P90 ~ "AIG",
    PESO > PESO_P90 ~ "GIG",
    is.na(PESO) | is.na(PESO_P10) | is.na(PESO_P90) ~ NA_character_))
dados_sinasc_2$F_PIG = factor(dados_sinasc_2$F_PIG, levels = c("PIG", "AIG", "GIG"))
str(dados_sinasc_2$F_PIG)

# Tarefa 9. Obter as frequências das categorias das variáveis qualitativas e medidas descritivas de variáveis quantitativas e salvar os resultados em novas variáveis.
# Exemplo: freq_SEXO = table(dados_sinasc_2$SEXO)   media_peso = mean(dados_sinasc_2$PESO)
# Medidas descritivas a serem calculadas para variáveis QUANTITATIVAS: P25, P50, P75, média e desvio-padrão. Atenção: usar na.rm = TRUE, quando necessário.

# Criando a base

base = data.frame(CODMUNRES = sort(unique(dados_sinasc_2$CODMUNRES)))

### 1. Informações sobre os nascimentos

### 1.1 Total de nascimentos

base = dados_sinasc_2 %>% 
  count(CODMUNRES, name = "TN") %>% 
  right_join(base, by = "CODMUNRES")

### 1.2 Total de nascimentos com registros completos nas 61 variáveis do SINASC

dados_SP = dados_sinasc %>% 
  filter(substr(CODMUNRES, 1, 2) == "35")
dados_SP_comp = dados_SP %>% group_by(CODMUNRES) %>% 
  summarise(TNRC = sum(complete.cases(.)))
base = base %>% left_join(dados_SP_comp, by = "CODMUNRES")

### 1.3 Total de nascimentos com registros completos nas 22 variáveis selecionadas

dados_SP1_comp = dados_sinasc_2 %>% 
  mutate(comp = complete.cases(.)) %>% 
  group_by(CODMUNRES) %>% 
  summarise(TNRCR = sum(comp), .groups = "drop")
base = base %>% left_join(dados_SP1_comp, by = "CODMUNRES")

### 2. Informações sobre as gestantes

### 2.1 Idade das gestantes

tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>% 
  summarise(TGI_15 = sum(F_IDADE == "<15", na.rm = TRUE),
            TGI_15_19 = sum(F_IDADE == "15-19", na.rm = TRUE),
            TGI_20_24 = sum(F_IDADE == "20-24", na.rm = TRUE),
            TGI_25_29 = sum(F_IDADE == "25-29", na.rm = TRUE),
            TGI_30_34 = sum(F_IDADE == "30-34", na.rm = TRUE),
            TGI_35_39 = sum(F_IDADE == "35-39", na.rm = TRUE),
            TGI_40_44 = sum(F_IDADE == "40-44", na.rm = TRUE),
            TGI_45_49 = sum(F_IDADE == "45-49", na.rm = TRUE),
            TGI_50 = sum(F_IDADE == "50+", na.rm = TRUE),
            TGIF = sum(F_IDADE %in% c("<15", "15-19", "20-24", "25-29", "30-34",
                                      "35-39", "40-44", "45-49"), na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 2.2 Medidas de posição e dispersão da idade materna

tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>% 
  summarise(IM_P25 = quantile(IDADEMAE, probs = 0.25, na.rm = TRUE),
            IM_P50 = quantile(IDADEMAE, probs = 0.50, na.rm = TRUE),
            IM_P75 = quantile(IDADEMAE, probs = 0.75, na.rm = TRUE),
            IM_MD = mean(IDADEMAE, na.rm = TRUE),
            IM_DP = sd(IDADEMAE, na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 2.3 Escolaridade materna

tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>% 
  summarise(EM_S = sum(ESCMAE2010 == "Sem escolaridade", na.rm = TRUE),
            EM_FI  = sum(ESCMAE2010 == "Fundamental I (1a a 4a série)", na.rm = TRUE),
            EM_FII = sum(ESCMAE2010 == "Fundamental II (5a a 8a série)", na.rm = TRUE),
            EM_M   = sum(ESCMAE2010 == "Médio (antigo 2o Grau)", na.rm = TRUE),
            EM_SI  = sum(ESCMAE2010 == "Superior incompleto", na.rm = TRUE),
            EM_SC  = sum(ESCMAE2010 == "Superior completo", na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 2.4 Raça/cor das gestantes

tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>%
  summarise(TGRC_B  = sum(RACACORMAE == "Branca", na.rm = TRUE),
            TGRC_PT = sum(RACACORMAE == "Preta", na.rm = TRUE),
            TGRC_A  = sum(RACACORMAE == "Amarela", na.rm = TRUE),
            TGRC_PD = sum(RACACORMAE == "Parda", na.rm = TRUE),
            TGRC_I  = sum(RACACORMAE == "Indígena", na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 2.5 Estado civil das gestantes

tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>%
  summarise(TGSC = sum(ESTCIV == "Sem companheiro", na.rm = TRUE),
            TGCC = sum(ESTCIV == "Com companheiro", na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 2.6 Gestações primíparas/não-primíparas

tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>%
  summarise(TGPRI = sum(PARIDADE == "Nulípara", na.rm = TRUE),
            TGNPRI = sum(PARIDADE == "Multípara", na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 3. Informações sobre as gestantes

#### 3.1 Gestações únicas/gemelares

tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>%
  summarise(TGU = sum(GRAVIDEZ == "Única", na.rm = TRUE),
            TGG = sum(GRAVIDEZ %in% c("Dupla", "Tripla ou mais"), na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 3.2 Duração das gestações

tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>%
  summarise(TGD_22 = sum(GESTACAO == "Menos de 22 semanas", na.rm = TRUE),
            TGD_22_27 = sum(GESTACAO == "22 a 27 semanas", na.rm = TRUE),
            TGD_28_31 = sum(GESTACAO == "28 a 31 semanas", na.rm = TRUE),
            TGD_32_36 = sum(GESTACAO == "32 a 36 semanas", na.rm = TRUE),
            TGD_37_41 = sum(GESTACAO == "37 a 41 semanas", na.rm = TRUE),
            TGD_42    = sum(GESTACAO == "42 semanas e mais", na.rm = TRUE),
            TGD_PRT = sum(GESTACAO %in% c("Menos de 22 semanas", "22 a 27 semanas",
                                          "28 a 31 semanas","32 a 36 semanas"), na.rm = TRUE),
            TGD_AT  = sum(GESTACAO == "37 a 41 semanas", na.rm = TRUE),
            TGD_PST = sum(GESTACAO == "42 semanas e mais", na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>%
  summarise(DG_P25 = quantile(SEMAGESTAC, probs = 0.25, na.rm = TRUE),
            DG_P50 = quantile(SEMAGESTAC, probs = 0.50, na.rm = TRUE),
            DG_P75 = quantile(SEMAGESTAC, probs = 0.75, na.rm = TRUE),
            DG_MD  = mean(SEMAGESTAC, na.rm = TRUE),
            DG_DP  = sd(SEMAGESTAC, na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 3.3 Pré-natal

dados_sinasc_2$KOTELCHUCK = as.factor(dados_sinasc_2$KOTELCHUCK)
tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>%
  summarise(TKC_NR  = sum(KOTELCHUCK == "1", na.rm = TRUE), 
            TKC_ID  = sum(KOTELCHUCK == "2", na.rm = TRUE), 
            TKC_IT  = sum(KOTELCHUCK == "3", na.rm = TRUE), 
            TKC_AD  = sum(KOTELCHUCK == "4", na.rm = TRUE), 
            TKC_MAD = sum(KOTELCHUCK == "5", na.rm = TRUE)) 
base = base %>% left_join(tab, by = "CODMUNRES")

### 4. Informações sobre o parto

### 4.1 Peregrinação

tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>%
  summarise(TGPRG_S = sum(PERIG == "Sim", na.rm = TRUE),
            TGPRG_N = sum(PERIG == "Não", na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 4.2 Tipos de parto

tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>%
  summarise(TPV = sum(PARTO == "Vaginal", na.rm = TRUE),
            TPC = sum(PARTO == "Cesário", na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 4.3 Posição dos recém-nascidos

tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>%
  summarise(TRAP_C = sum(TPAPRESENT == "Cefálico", na.rm = TRUE),
            TRAP_P = sum(TPAPRESENT == "Pélvica ou podálica", na.rm = TRUE),
            TRAP_T = sum(TPAPRESENT == "Transversa", na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 4.4 Grupo de Robson

tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>%
  summarise(TGROB_1  = sum(TPROBSON == "Grupo 1", na.rm = TRUE),
            TGROB_2  = sum(TPROBSON == "Grupo 2", na.rm = TRUE),
            TGROB_3  = sum(TPROBSON == "Grupo 3", na.rm = TRUE),
            TGROB_4  = sum(TPROBSON == "Grupo 4", na.rm = TRUE),
            TGROB_5  = sum(TPROBSON == "Grupo 5", na.rm = TRUE),
            TGROB_6  = sum(TPROBSON == "Grupo 6", na.rm = TRUE),
            TGROB_7  = sum(TPROBSON == "Grupo 7", na.rm = TRUE),
            TGROB_8  = sum(TPROBSON == "Grupo 8", na.rm = TRUE),
            TGROB_9  = sum(TPROBSON == "Grupo 9", na.rm = TRUE),
            TGROB_10 = sum(TPROBSON == "Grupo 10", na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 4.5 Local do nascimento

tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>%
  summarise(TNLOC_H  = sum(LOCNASC == "Hospital", na.rm = TRUE),
            TNLOC_ES = sum(LOCNASC == "Outros estabelecimentos de saúde", na.rm = TRUE),
            TNLOC_D  = sum(LOCNASC == "Domicílio", na.rm = TRUE),
            TNLOC_O  = sum(LOCNASC == "Outros", na.rm = TRUE),
            TNLOC_AI = sum(LOCNASC == "Aldeia indígena", na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 5. Informações sobre os recém-nascidos

### 5.1 Sexo

tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>%
  summarise(TRS_M = sum(SEXO == "Masculino", na.rm = TRUE),
            TRS_F = sum(SEXO == "Feminino", na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 5.2 Raça/cor dos recém-nascidos

tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>%
  summarise(TRRC_B  = sum(RACACOR == "Branca", na.rm = TRUE),
            TRRC_PT = sum(RACACOR == "Preta", na.rm = TRUE),
            TRRC_A  = sum(RACACOR == "Amarela", na.rm = TRUE),
            TRRC_PD = sum(RACACOR == "Parda", na.rm = TRUE),
            TRRC_I  = sum(RACACOR == "Indígena", na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 5.3 Peso

tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>%
  summarise(TRP_BP = sum(F_PESO == "Baixo peso", na.rm = TRUE),
            TRP_N  = sum(F_PESO == "Peso normal", na.rm = TRUE),
            TRP_M  = sum(F_PESO == "Macrossomia", na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

# Medidas de posição e dispersão

tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>%
  summarise(PESO_P25 = quantile(PESO, probs = 0.25, na.rm = TRUE),
            PESO_P50 = quantile(PESO, probs = 0.50, na.rm = TRUE),
            PESO_P75 = quantile(PESO, probs = 0.75, na.rm = TRUE),
            PESO_MD  = mean(PESO, na.rm = TRUE),
            PESO_DP  = sd(PESO, na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

# PIG, GIG e AIG

tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>%
  summarise(TRPIG_P = sum(F_PIG == "PIG", na.rm = TRUE), # Pequeno para Idade Gestacional
            TRPIG_A = sum(F_PIG == "AIG", na.rm = TRUE), # Adequado
            TRPIG_G = sum(F_PIG == "GIG", na.rm = TRUE)) # Grande
base = base %>% left_join(tab, by = "CODMUNRES")

### 5.4 APGAR

tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>%
  summarise(TRAPG5_B = sum(F_APGAR5 == "Baixo", na.rm = TRUE),
            TRAPG5_N = sum(F_APGAR5 == "Normal", na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

# Medidas de posição e dispersão

tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>%
  summarise(APG5_MD = mean(APGAR5, na.rm = TRUE),
            APG5_DP = sd(APGAR5, na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 5.5 Anomalias

tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>%
  summarise(TRAC  = sum(IDANOMAL == "Sim", na.rm = TRUE),
            TRSAC = sum(IDANOMAL == "Não", na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

# Adicionando a variável ANO, NIVEL e a linha de UF

# ANO

dados_sinasc_2 = dados_sinasc_2 %>%
  mutate(ANO = "2015")  %>%
  relocate(ANO, .before = 1)
base = base %>%
  mutate(ANO = "2015")  %>%
  relocate(ANO, .before = 1)

# Criando df com dados de UF

estado_sp = dados_sinasc_2 %>%
  summarise(ANO = unique(ANO)[1],
            CODMUNRES = 35,
    
    ### Nascimentos
    TN = n(),
    TNRCR = sum(complete.cases(.)),
    
    ### Idade
    TGI_15 = sum(F_IDADE == "<15", na.rm = TRUE),
    TGI_15_19 = sum(F_IDADE == "15-19", na.rm = TRUE),
    TGI_20_24 = sum(F_IDADE == "20-24", na.rm = TRUE),
    TGI_25_29 = sum(F_IDADE == "25-29", na.rm = TRUE),
    TGI_30_34 = sum(F_IDADE == "30-34", na.rm = TRUE),
    TGI_35_39 = sum(F_IDADE == "35-39", na.rm = TRUE),
    TGI_40_44 = sum(F_IDADE == "40-44", na.rm = TRUE),
    TGI_45_49 = sum(F_IDADE == "45-49", na.rm = TRUE),
    TGI_50 = sum(F_IDADE == "50+", na.rm = TRUE),
    TGIF = sum(F_IDADE %in% c("<15","15-19","20-24","25-29",
                              "30-34","35-39","40-44","45-49"), na.rm = TRUE),
    
    IM_P25 = quantile(IDADEMAE, 0.25, na.rm = TRUE),
    IM_P50 = quantile(IDADEMAE, 0.50, na.rm = TRUE),
    IM_P75 = quantile(IDADEMAE, 0.75, na.rm = TRUE),
    IM_MD = mean(IDADEMAE, na.rm = TRUE),
    IM_DP = sd(IDADEMAE, na.rm = TRUE),
    
    ### Escolaridade
    EM_S  = sum(ESCMAE2010 == "Sem escolaridade", na.rm = TRUE),
    EM_FI = sum(ESCMAE2010 == "Fundamental I (1a a 4a série)", na.rm = TRUE),
    EM_FII= sum(ESCMAE2010 == "Fundamental II (5a a 8a série)", na.rm = TRUE),
    EM_M  = sum(ESCMAE2010 == "Médio (antigo 2o Grau)", na.rm = TRUE),
    EM_SI = sum(ESCMAE2010 == "Superior incompleto", na.rm = TRUE),
    EM_SC = sum(ESCMAE2010 == "Superior completo", na.rm = TRUE),
    
    ### Raça/cor mãe
    TGRC_B  = sum(RACACORMAE == "Branca", na.rm = TRUE),
    TGRC_PT = sum(RACACORMAE == "Preta", na.rm = TRUE),
    TGRC_A  = sum(RACACORMAE == "Amarela", na.rm = TRUE),
    TGRC_PD = sum(RACACORMAE == "Parda", na.rm = TRUE),
    TGRC_I  = sum(RACACORMAE == "Indígena", na.rm = TRUE),
    
    ### Estado civil
    TGSC = sum(ESTCIV == "Sem companheiro", na.rm = TRUE),
    TGCC = sum(ESTCIV == "Com companheiro", na.rm = TRUE),
    
    ### Paridade
    TGPRI = sum(PARIDADE == "Nulípara", na.rm = TRUE),
    TGNPRI = sum(PARIDADE == "Multípara", na.rm = TRUE),
    
    ### Gravidez
    TGU = sum(GRAVIDEZ == "Única", na.rm = TRUE),
    TGG = sum(GRAVIDEZ %in% c("Dupla","Tripla ou mais"), na.rm = TRUE),
    
    ### Duração gestação
    TGD_22 = sum(GESTACAO == "Menos de 22 semanas", na.rm = TRUE),
    TGD_22_27 = sum(GESTACAO == "22 a 27 semanas", na.rm = TRUE),
    TGD_28_31 = sum(GESTACAO == "28 a 31 semanas", na.rm = TRUE),
    TGD_32_36 = sum(GESTACAO == "32 a 36 semanas", na.rm = TRUE),
    TGD_37_41 = sum(GESTACAO == "37 a 41 semanas", na.rm = TRUE),
    TGD_42    = sum(GESTACAO == "42 semanas e mais", na.rm = TRUE),
    
    TGD_PRT = sum(GESTACAO %in% c("Menos de 22 semanas","22 a 27 semanas",
                                  "28 a 31 semanas","32 a 36 semanas"), na.rm = TRUE),
    TGD_AT  = sum(GESTACAO == "37 a 41 semanas", na.rm = TRUE),
    TGD_PST = sum(GESTACAO == "42 semanas e mais", na.rm = TRUE),
    
    DG_P25 = quantile(SEMAGESTAC, 0.25, na.rm = TRUE),
    DG_P50 = quantile(SEMAGESTAC, 0.50, na.rm = TRUE),
    DG_P75 = quantile(SEMAGESTAC, 0.75, na.rm = TRUE),
    DG_MD  = mean(SEMAGESTAC, na.rm = TRUE),
    DG_DP  = sd(SEMAGESTAC, na.rm = TRUE),
    
    ### Pré-natal
    TKC_NR  = sum(KOTELCHUCK == "1", na.rm = TRUE),
    TKC_ID  = sum(KOTELCHUCK == "2", na.rm = TRUE),
    TKC_IT  = sum(KOTELCHUCK == "3", na.rm = TRUE),
    TKC_AD  = sum(KOTELCHUCK == "4", na.rm = TRUE),
    TKC_MAD = sum(KOTELCHUCK == "5", na.rm = TRUE),
    
    ### Peregrinação
    TGPRG_S = sum(PERIG == "Sim", na.rm = TRUE),
    TGPRG_N = sum(PERIG == "Não", na.rm = TRUE),
    
    ### Tipo de parto
    TPV = sum(PARTO == "Vaginal", na.rm = TRUE),
    TPC = sum(PARTO == "Cesário", na.rm = TRUE),
    
    ### Apresentação
    TRAP_C = sum(TPAPRESENT == "Cefálico", na.rm = TRUE),
    TRAP_P = sum(TPAPRESENT == "Pélvica ou podálica", na.rm = TRUE),
    TRAP_T = sum(TPAPRESENT == "Transversa", na.rm = TRUE),
    
    ### Robson
    TGROB_1  = sum(TPROBSON == "Grupo 1", na.rm = TRUE),
    TGROB_2  = sum(TPROBSON == "Grupo 2", na.rm = TRUE),
    TGROB_3  = sum(TPROBSON == "Grupo 3", na.rm = TRUE),
    TGROB_4  = sum(TPROBSON == "Grupo 4", na.rm = TRUE),
    TGROB_5  = sum(TPROBSON == "Grupo 5", na.rm = TRUE),
    TGROB_6  = sum(TPROBSON == "Grupo 6", na.rm = TRUE),
    TGROB_7  = sum(TPROBSON == "Grupo 7", na.rm = TRUE),
    TGROB_8  = sum(TPROBSON == "Grupo 8", na.rm = TRUE),
    TGROB_9  = sum(TPROBSON == "Grupo 9", na.rm = TRUE),
    TGROB_10 = sum(TPROBSON == "Grupo 10", na.rm = TRUE),
    
    ### Local nascimento
    TNLOC_H  = sum(LOCNASC == "Hospital", na.rm = TRUE),
    TNLOC_ES = sum(LOCNASC == "Outros estabelecimentos de saúde", na.rm = TRUE),
    TNLOC_D  = sum(LOCNASC == "Domicílio", na.rm = TRUE),
    TNLOC_O  = sum(LOCNASC == "Outros", na.rm = TRUE),
    TNLOC_AI = sum(LOCNASC == "Aldeia indígena", na.rm = TRUE),
    
    ### RN sexo
    TRS_M = sum(SEXO == "Masculino", na.rm = TRUE),
    TRS_F = sum(SEXO == "Feminino", na.rm = TRUE),
    
    ### RN raça
    TRRC_B  = sum(RACACOR == "Branca", na.rm = TRUE),
    TRRC_PT = sum(RACACOR == "Preta", na.rm = TRUE),
    TRRC_A  = sum(RACACOR == "Amarela", na.rm = TRUE),
    TRRC_PD = sum(RACACOR == "Parda", na.rm = TRUE),
    TRRC_I  = sum(RACACOR == "Indígena", na.rm = TRUE),
    
    ### Peso
    TRP_BP = sum(F_PESO == "Baixo peso", na.rm = TRUE),
    TRP_N  = sum(F_PESO == "Peso normal", na.rm = TRUE),
    TRP_M  = sum(F_PESO == "Macrossomia", na.rm = TRUE),
    
    PESO_P25 = quantile(PESO, 0.25, na.rm = TRUE),
    PESO_P50 = quantile(PESO, 0.50, na.rm = TRUE),
    PESO_P75 = quantile(PESO, 0.75, na.rm = TRUE),
    PESO_MD  = mean(PESO, na.rm = TRUE),
    PESO_DP  = sd(PESO, na.rm = TRUE),
    
    ### PIG/AIG/GIG
    TRPIG_P = sum(F_PIG == "PIG", na.rm = TRUE),
    TRPIG_A = sum(F_PIG == "AIG", na.rm = TRUE),
    TRPIG_G = sum(F_PIG == "GIG", na.rm = TRUE),
    
    ### APGAR
    TRAPG5_B = sum(F_APGAR5 == "Baixo", na.rm = TRUE),
    TRAPG5_N = sum(F_APGAR5 == "Normal", na.rm = TRUE),
    
    APG5_MD = mean(APGAR5, na.rm = TRUE),
    APG5_DP = sd(APGAR5, na.rm = TRUE),
    
    ### Anomalias
    TRAC  = sum(IDANOMAL == "Sim", na.rm = TRUE),
    TRSAC = sum(IDANOMAL == "Não", na.rm = TRUE))
estado_sp$TNRC = sum(complete.cases(dados_sinasc))

# Alinhando as colunas
estado_sp = estado_sp %>%
  select(names(base))

base = bind_rows(base, estado_sp)

# NIVEL

base = base %>% 
  mutate(NIVEL = if_else(CODMUNRES == 35, "UF", "MUNICIPIO")) %>% 
  relocate(NIVEL, .before = 2)

base = base %>%
  arrange(CODMUNRES != 35)

# Tarefa 10. Criar as colunas do novo banco de dados (de nome SINASC_UF.csv Exemplo: SINASC_RJ.csv) com base nas análises prévias, devendo as variáveis estar na ordem indicada abaixo
# ATENÇÃO aos nomes das variáveis e ordem das colunas
# 1. ANO: 2015  2. UFR (Estado de residência)   3. TN (total de nascimentos)   4. TNRC (total de nascimentos com registros completos, ou seja, sem NA em todas as variáveis do banco de dados)
# 5. TGI_15 (total de gestantes com idade inferior a 15 anos - F_IDADE)   6. TGI_15_19 (total de gestantes com idade >=15 e <=19 anos)
# 7: TGI_20_24 (total de gestantes com idade >=20 e <=24 anos)   8. TGI_25_29 (total de gestantes com idade >=25 e <=29 anos)
# 9: TGI_30_34 (total de gestantes com idade >=30 e <=34 anos)   10. TGI_35_39 (total de gestantes com idade >=35 e <=39 anos)
# 11: TGI_40_44 (total de gestantes com idade >=40 e <=44 anos)  12. TGI_45_49 (total de gestantes com idade >=45 e <=49 anos)
# 13: TGI_50 (total de gestantes com idade >=50)   14: TGIF (total de gestantes em idade fértil, idade >=15 e <=49 anos)
# 15: IM_P25 (percentil 25 da idade materna - IDADEMAE) 16: IM_P50 (percentil 50 da idade materna)   17: IM_P75 (percentil 75 da idade materna)
# 18. IM_MD (idade média materna)   19: IM_DP (desvio-padrão da idade materna)
# 20. EM_S (total de gestantes sem escolaridade, ESCMAE2010=0)   21: EM_FI (total de gestantes com escolaridade Fundamental I)
# 22. EM_FII (total de gestantes com escolaridade Fundamental II)   23. EM_M (total de gestantes com escolaridade Médio)   
# 24. EM_SI (total de gestantes com escolaridade Superior Incompleto)   25. EM_SC (total de gestantes com escolaridade Superior Completo) 
# 26. TGRC_B (total de gestantes da raça/cor branca - RACACORMAE)   27. TGRC_PT (total de gestantes da raça/cor preta)
# 28. TGRC_A (total de gestantes da raça/cor amarela)   29. TGRC_PD (total de gestantes da raça/cor parda)
# 30. TGRC_I (total de gestantes da raça/cor indígena)
# 31. TGSC (total de gestantes sem companheiro - ESTCIV)   32. TGCC (total de gestantes com companheiro)
# 33. TGPRI (total de gestantes primíparas - PARIDADE)     34. TGNPRI (total de gestantes não primíparas)
# 35. TGU (total de gestações única)   36. TGG (total de gestações gemelares)   37. TGD_22 (total de gestações com duração inferior a 22 semanas - GESTACAO)
# 38. TGD_22_27 (total de gestações com duração da gestação >=22 e <=27)   39. TGD_28_31 (total de gestações com duração da gestação >=28 e <=31)
# 40. TGD_32_36 (total de gestações com duração da gestação >=32 e <=36)   41. TGD_37_41 (total de gestações com duração da gestação >=37 e <=41)
# 42. TGD_42 (total de gestações com duração da gestação >=42)   43. TGD_PRT (total de gestações pre-termo, duração < 37 semanas)
# 44. TGD_AT (total de gestações a termo, duração >=37 e <=41)   45. TGD_PST  (total de gestações pós termo, duração >=42) 
# 46. DG_P25 (percentil 25 da duração da gestação - SEMAGESTAC)  47. DG_P50 (percentil 50 da duração da gestação)   
# 48. DG_P75 (percentil 75 da duração da gestação)   49. DG_MD (idade média da duração da gestação)   50. DG_DP (desvio-padrão da duração da gestação)
# 51. TKC_NR (total de consultas de pre-natal não realizado - KOTELCHUCK)   52. TKC_ID (total de consultas de pre-natal inadequado)
# 53. TKC_IT (total de consultas de pre-natal intermediário)   54. TKC_AD (total de consultas de pre-natal adequado)  
# 55. TKC_MAD (total de consultas de pre-natal mais que adequado)   56. TGPRG_S (total de gestantes que peregrinaram)  
# 57. TGPRG_N (total de gestantes que não peregrinaram)    58. TPV (total de partos vaginais)   59. TPC (total de partos cesáreos) 
# 60. TRAP_C (total de recém-nascidos na posição cefálica - TPAPRESENT)   61. TRAP_P (total de recém-nascidos na posição pélvica ou podálica)
# 62. TRAP_T (total de recém-nascidos na posição transversa)  63. TGROB_1 (total de gestantes do grupo de Robson 1 - TPROBSON)
# 64. TGROB_2 (total de gestantes do grupo de Robson 2)   65. TGROB_3 (total de gestantes do grupo de Robson 3)
# 66. TGROB_4 (total de gestantes do grupo de Robson 4)   67. TGROB_5 (total de gestantes do grupo de Robson 5)
# 68. TGROB_6 (total de gestantes do grupo de Robson 6)   69. TGROB_7 (total de gestantes do grupo de Robson 7)
# 70. TGROB_8 (total de gestantes do grupo de Robson 8)   71. TGROB_9 (total de gestantes do grupo de Robson 9)
# 72. TGROB_10 (total de gestantes do grupo de Robson 10)   
# 73. TNLOC_H (total de nascimentos em hospital)   74. TNLOC_ES (total de nascimentos em outros estabelecimentos de saúde)
# 75. TNLOC_D (total de nascimentos em domicílio)  76. TNLOC_O (total de nascimentos em outros locais) 
# 77. TNLOC_AI (total de nascimentos em aldeias indígenas)   
# 78. TRRC_B (total de recém-nascidos da raça/cor branca - RACACOR)   79. TRRC_PT (total de recém-nascidos da raça/cor preta)
# 80. TRRC_A (total de recém-nascidos da raça/cor amarela)   81. TRRC_PD (total de recém-nascidos da raça/cor parda)
# 82. TRRC_I (total de recém-nascidos da raça/cor indígena)  83. TRP_BP (total de recém nascidos com baixo peso - FPESO)
# 84. TRP_N (total de recém nascidos com peso normal)   85. TRP_M (total de recém nascidos com macrossomia)
# 86. PESO_P25 (percentil 25 do peso dos recém-nascidos - PESO)  87. PESO_P50 (percentil 50 do peso dos recém-nascidos)   
# 88. PESO_P75 (percentil 75 do peso dos recém-nascidos)  89. PESO_MD (peso médio dos recém-nascidos)   
# 90. PESO_DP (desvio-padrão dos pesos dos recém-nascidos)    91. TRPIG_P (total de recém-nascidos de GESTAÇÕES ÚNICAS com PIG) 
# 92. TRPIG_A (total de recém-nascidos de GESTAÇÕES ÚNICAS com AIG)   93. TRPIG_G (total de recém-nascidos de GESTAÇÕES ÚNICAS com GIG)
# 94: TRAPG5_B (total de recém-nascidos com Apgar5 baixo, ou seja, < 7)
# 95: TRAPG5_N (total de recém-nascidos com Apgar5 normal, ou seja, >= 7)   96. APG5_MD (Apgar5 médio dos recém-nascidos)   
# 97. APG5_DP (desvio-padrão dos Apgar5 dos recém-nascidos)   98. TRAC (total de recém-nascidos com anomalia congênita - IDANOMAL)
# 99. TRSAC (total de recém-nascidos sem anomalia congênita)


# Tarefa 11: Exporte o banco de dados com o nome SINASC_UF.csv

write.csv(base, "SINASC_UF.csv", row.names = FALSE)

# Ao terminar a ETAPA 1 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 1"

##################################
# ETAPA 2: BANCO DE DADOS DO SIM
##################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 2

# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015 com 1216475 linhas e 87 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sim


# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão (a informar)
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, CODMUNNATU, IDADE,  SEXO,  RACACOR,  ESTCIV, ESC2010, 
# CODMUNRES,  LOCOCOR, CODMUNOCOR, TPMORTEOCO,  OBITOGRAV, OBITOPUERP, CAUSABAS, CAUSABAS_O, TPOBITOCOR, MORTEPARTO



#####################################################
# ETAPA 3: OUTROS BANCOS DE DADOS: IBGE, SNIS, ...
#####################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 3

# Tarefa 1. Acesso aos bancos de dados e obtenção da informação



#####################################################################################################
# ETAPA 4: GERAR BANCO DE DADOS FINAL DO ESTADO, BASEADO NAS ANÁLISES DE SINASC, SIM, IBGE, SNIS,...
######################################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 4

# Cada aluno gerar um dataframe de uma única linha (referente ao seu estado) com as variáveis na ordem indicada pela professora



############################################################################################
# ETAPA 5: EMPILHAMENTO DOS DATAFRAMES DE CADA ESTADO, GERANDO UM DATAFRAME DE 27 LINHAS
############################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 5

# 1. Enviar arquivos para as pastas do repositório da Professora no GitHUb
# 2. A professora fará o empilhamentos dos dataframes

