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

# Tarefas 9 e 10 (reformulada) do script esqueleto:

# Crie um banco de dados, de nome SINASC_UF.csv (Exemplo: SINASC_RJ.csv), contendo as 103 variáveis listadas no arquivo “Variáveis - Projeto - Tarefas 9 e 10 da Etapa 1.pdf”

# O banco final deverá possuir:
# • 103 colunas, correspondentes às variáveis especificadas;
# • n + 1 linhas, onde:
# • n corresponde ao número de municípios distintos da UF em análise
# • a primeira linha corresponde aos valores agregados para a UF como um todo;
# • as demais linhas correspondem aos municípios da UF.
# As variáveis devem ser construídas a partir dos microdados do SINASC (dados_sinasc, dados_sinasc_1 e dados_sinasc_2), respeitando os nomes e a ordem especificados.

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
            IM_MD = round(mean(IDADEMAE, na.rm = TRUE),2),
            IM_DP = round(sd(IDADEMAE, na.rm = TRUE),2))
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
            DG_MD  = round(mean(SEMAGESTAC, na.rm = TRUE),2),
            DG_DP  = round(sd(SEMAGESTAC, na.rm = TRUE), 2))
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
            PESO_MD  = round(mean(PESO, na.rm = TRUE),2),
            PESO_DP  = round(sd(PESO, na.rm = TRUE),2))
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
  summarise(APG5_MD = round(mean(APGAR5, na.rm = TRUE),2),
            APG5_DP = round(sd(APGAR5, na.rm = TRUE),2))
base = base %>% left_join(tab, by = "CODMUNRES")

### 5.5 Anomalias

tab = dados_sinasc_2 %>% group_by(CODMUNRES) %>%
  summarise(TRAC  = sum(IDANOMAL == "Sim", na.rm = TRUE),
            TRSAC = sum(IDANOMAL == "Não", na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

# Adicionando a variável ANO, NIVEL e a linha de UF

# ANO

base = base %>%
  mutate(ANO = "2015")  %>%
  relocate(ANO, .before = 1)

# Criando df com dados de UF

estado_sp = dados_sinasc_2 %>%
  summarise(CODMUNRES = 35,
    
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
    IM_MD = round(mean(IDADEMAE, na.rm = TRUE),2),
    IM_DP = round(sd(IDADEMAE, na.rm = TRUE),2),
    
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
    DG_MD  = round(mean(SEMAGESTAC, na.rm = TRUE),2),
    DG_DP  = round(sd(SEMAGESTAC, na.rm = TRUE),2),
    
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
    PESO_MD  = round(mean(PESO, na.rm = TRUE),2),
    PESO_DP  = round(sd(PESO, na.rm = TRUE),2),
    
    ### PIG/AIG/GIG
    TRPIG_P = sum(F_PIG == "PIG", na.rm = TRUE),
    TRPIG_A = sum(F_PIG == "AIG", na.rm = TRUE),
    TRPIG_G = sum(F_PIG == "GIG", na.rm = TRUE),
    
    ### APGAR
    TRAPG5_B = sum(F_APGAR5 == "Baixo", na.rm = TRUE),
    TRAPG5_N = sum(F_APGAR5 == "Normal", na.rm = TRUE),
    
    APG5_MD = round(mean(APGAR5, na.rm = TRUE),2),
    APG5_DP = round(sd(APGAR5, na.rm = TRUE),2),
    
    ### Anomalias
    TRAC  = sum(IDANOMAL == "Sim", na.rm = TRUE),
    TRSAC = sum(IDANOMAL == "Não", na.rm = TRUE))
estado_sp$TNRC = sum(complete.cases(dados_sinasc))

estado_sp = estado_sp %>%
  mutate(ANO = "2015")  %>%
  relocate(ANO, .before = 1)

# Alinhando as colunas
estado_sp = estado_sp %>%
  select(names(base))

base = bind_rows(base, estado_sp)

# NIVEL

base = base %>% 
  mutate(NIVEL = if_else(CODMUNRES == 35, "UF", "MUNICIPIO")) %>% 
  relocate(NIVEL, .before = 2)

SINASC_SP = base %>%
  arrange(CODMUNRES != 35)

# Tarefa 11: Exporte o banco de dados com o nome SINASC_UF.csv

write.csv(SINASC_SP, "SINASC_SP.csv", row.names = FALSE)

# Ao terminar a ETAPA 1 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 1"

##################################
# ETAPA 2: BANCO DE DADOS DO SIM
##################################
# Só inicie esta Etapa quando a professora orientar
# Altere o script esqueleto nas partes que se refere a ETAPA 2 e envie para o repositório Extensao tendo feito o commit "Esqueleto atualizado na Etapa 2"
# A partir de main crie a branch SIM e vá para ela
# ESTANDO NA BRANCH SIM, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 1 e só insira comandos na ETAPA 2

# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015 com 1264175 linhas e 87 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sim

### Leitura do banco de dados

dados_sim = read.csv("Mortalidade_Geral_2015.csv", header = TRUE, sep = ";")

# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão: 1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, DTOBITO, DTNASC, IDADE, SEXO, RACACOR, ESC2010, 
# CODMUNRES, TPMORTEOCO, OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO

### Reduzindo as colunas

dados_sim_1 = dados_sim[, c(1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84)]

# Tarefa 3. Reduzir dados_sim_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de 
# CODMUNRES), nomeando este novo banco de dados como dados_sim_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF

### Reduzindo dados para SP

dados_sim_2 = dados_sim_1 %>%
  filter(substr(CODMUNRES, 1, 2) == "35")

### Gerando arquivo de dados de SP

write.csv(dados_sim_2, "dados_sim_2.csv", row.names = FALSE)

# observar abaixo o número de óbitos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 7948      12: 3517      13: 16675     14: 2091      15: 37365     16: 2946       17: 7402
# 21: 33666     22: 19366     23: 55258     24: 20153     25: 26422     26: 62556      27: 19756     28: 13453     29: 87083
# 31: 131274    32: 22332     33: 127714    35: 287645    
# 41: 70839     42: 37984     43: 82349
# 50: 15457     51: 17095     52: 38854     53: 11975

# Exportar o arquivo com o nome dados_sim_2.csv


# Ao concluir a Tarefa 3 da Etapa 2 commite e envie para o repositório REMOTO o script e dados_sim_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"


# Tarefa 4. Verificar em dados_sim_2 a frequência das categorias das seguintes variáveis: TIPOBITO, SEXO, RACACOR,
# TPMORTEOCO, OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO

### Verificando frequências e NA´s

table(dados_sim_2$TIPOBITO)
table(dados_sim_2$SEXO)
table(dados_sim_2$RACACOR)
table(dados_sim_2$ESC2010)
table(dados_sim_2$TPMORTEOCO)
table(dados_sim_2$OBITOGRAV)
table(dados_sim_2$OBITOPUERP)
table(dados_sim_2$CAUSABAS)
table(dados_sim_2$TPOBITOCOR)
table(dados_sim_2$MORTEPARTO)

table(dados_sim_2$IDADE)
sort(unique(dados_sim_2$IDADE))

table(dados_sim_2$ESC2010)

# Tarefa 5. Atribuir para cada variável de dados_sim_2 como sendo NA a categoria de "Não informado ou Ignorado", 
# geralmente com código 9
# veja o dicionário do SIM para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADE verificar se existem valores como 9999 para NA

### Atribuindo NA para cartegorias de "Não informado"/"Ignorado"

dados_sim_2 = dados_sim_2 %>% 
  mutate(IDADE = na_if(IDADE, 999))

dados_sim_2 = dados_sim_2 %>%
  mutate(SEXO = na_if(SEXO, 0))

dados_sim_2 = dados_sim_2 %>%
  mutate(ESC2010 = na_if(ESC2010, 9))

dados_sim_2 = dados_sim_2 %>%
  mutate(TPMORTEOCO = na_if(TPMORTEOCO, 9))

dados_sim_2 = dados_sim_2 %>%
  mutate(OBITOGRAV = na_if(OBITOGRAV, 9))

dados_sim_2 = dados_sim_2 %>%
  mutate(OBITOPUERP = na_if(OBITOPUERP, 9))

dados_sim_2 = dados_sim_2 %>%
  mutate(MORTEPARTO = na_if(MORTEPARTO, 9))

# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1,2), labels = c("Fetal", "Não fetal")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados

### Atribuindo legendas 

str(dados_sim_2$TIPOBITO)
dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1, 2),
                              labels = c("Fetal", "Não fetal"))

str(dados_sim_2$SEXO)
dados_sim_2$SEXO = factor(dados_sim_2$SEXO, levels = c(1, 2),
                              labels = c("Masculino", "Feminino"))

str(dados_sim_2$RACACOR)
dados_sim_2$RACACOR = factor(dados_sim_2$RACACOR, levels = c(1, 2, 3, 4, 5),
                              labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))

str(dados_sim_2$ESC2010)
dados_sim_2$ESC2010 = factor(dados_sim_2$ESC2010, levels = c(0, 1, 2, 3, 4, 5),
                             labels = c("Sem escolaridade", "Fundamental I (1a a 4a série)", 
                                        "Fundamental II (5a a 8a série)", "Médio (antigo 2o Grau)",
                                        "Superior incompleto", "Superior completo"))

str(dados_sim_2$TPMORTEOCO)
dados_sim_2$TPMORTEOCO = factor(dados_sim_2$TPMORTEOCO, levels = c(1, 2, 3, 4, 5, 8),
                             labels = c("Na gravidez", "No parto", "No abortamento",
                                        "Até 42 dias após o término do parto",
                                        "De 43 dias a 1 ano após o término da gestação",
                                        "Não ocorreu nestes períodos"))

str(dados_sim_2$OBITOGRAV)
dados_sim_2$OBITOGRAV = factor(dados_sim_2$OBITOGRAV, levels = c(1, 2),
                              labels = c("Sim", "Não"))

str(dados_sim_2$OBITOPUERP)
dados_sim_2$OBITOPUERP = factor(dados_sim_2$OBITOPUERP, levels = c(1, 2, 3),
                              labels = c("Sim, até 42 dias após o parto", "Sim, de 43 dias a 1 ano",
                                         "Não"))

str(dados_sim_2$TPOBITOCOR)
dados_sim_2$TPOBITOCOR = factor(dados_sim_2$TPOBITOCOR, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                labels = c("Durante a gestação", "Durante o abortamento",
                                           "Após o abortamento", "No parto ou até 1 hora após o parto",
                                           "No puerpério - até 42 dias após o parto", 
                                           "Entre 43 dias e até 1 ano após o parto", 
                                           "A investigação não identificou o momento do óbito",
                                           "Mais de um ano após o parto", 
                                           "O óbito não ocorreu nas circunstancias anteriores"))

str(dados_sim_2$MORTEPARTO)
dados_sim_2$MORTEPARTO = factor(dados_sim_2$MORTEPARTO, levels = c(1, 2, 3),
                                labels = c("Antes", "Durante", "Após"))

# Tarefa 7. Crie um banco de dados, de nome SIM_UF.csv (Exemplo: SIM_RJ.csv), contendo as 41 variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 7 da Etapa 2.pdf”
# Atenção:
# 1. Para informações gerais utilize CAUSABAS, SEXO e IDADE
# 2. Para informações fetais utilize TIPOBITO
# 3. Para informações neonatais utilize TIPOBITO não fetal e IDADE entre 0 e 27 dias e RACACOR
# 4. Para informações maternas utilize TPMORTEOCO, ESC e IDADE

### Criando a nova base

base = data.frame(CODMUNRES = sort(unique(dados_sim_2$CODMUNRES)))

### 1. Informações Gerais

### 1.1 Total de óbitos

base = dados_sim_2 %>% 
  count(CODMUNRES, name = "TO") %>% 
  right_join(base, by = "CODMUNRES")

### 1.2 Total de óbitos com registros completos nas 87 variáveis do SIM

dados_SP = dados_sim_2 %>% 
  filter(substr(CODMUNRES, 1, 2) == "35")
dados_SP_comp = dados_SP %>% group_by(CODMUNRES) %>% 
  summarise(TORC = sum(complete.cases(.)))
base = base %>% left_join(dados_SP_comp, by = "CODMUNRES")

### 1.3 Total de óbitos com registros completos nas 14 variáveis selecionadas do SIM

dados_SP1_comp = dados_sim_2 %>% 
  mutate(comp = complete.cases(.)) %>% 
  group_by(CODMUNRES) %>% 
  summarise(TORCR = sum(comp), .groups = "drop")
base = base %>% left_join(dados_SP1_comp, by = "CODMUNRES")

### 1.4 Total de óbitos Naturais/Não naturais

tab = dados_sim_2 %>% group_by(CODMUNRES) %>%
  summarise({ext = substr(CAUSABAS, 1, 1) %in% c("V", "W", "X", "Y")
  tibble(TO_NN = sum(ext, na.rm = TRUE),
        TO_N  = sum(!ext, na.rm = TRUE))})
base = base %>% left_join(tab, by = "CODMUNRES")

### 1.5 Total de óbitos por categoria

tab = dados_sim_2 %>%
  mutate(cid = substr(CAUSABAS, 1, 1)) %>%
  group_by(CODMUNRES) %>%
  summarise(TO_CB_I = sum(cid %in% c("A", "B"), na.rm = TRUE), # doenças infecciosas ou parasitárias
            TO_CB_N = sum(cid %in% c("C", "D"), na.rm = TRUE), # neoplasias ou doenças hematológicas
            TO_CB_C = sum(cid == "I", na.rm = TRUE), # doenças circulatórias
            TO_CB_R = sum(cid == "J", na.rm = TRUE), # doenças respiratórias
            TO_CB_O = sum(!(cid %in% c("A", "B", "C", "D", "I", "J", "V", "W", "X", "Y")), 
                          na.rm = TRUE)) # outras causas naturais
base = base %>% left_join(tab, by = "CODMUNRES")

### 1.6 Total de óbito por sexo

tab = dados_sim_2 %>% group_by(CODMUNRES) %>% 
  summarise(TO_M = sum(SEXO == "Masculino", na.rm = TRUE),
            TO_F = sum(SEXO == "Feminino", na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 1.7 Óbitos femininos em idade fértil (idade >=15 e <=49 anos)

tab = dados_sim_2 %>% group_by(CODMUNRES) %>% 
  summarise(TO_F_IF = sum(SEXO == "Feminino" & IDADE >= 415 & IDADE <= 449, na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 2. Informações fetais e neonatais

### 2.1 Total de óbitos fetais

tab = dados_sim_2 %>% group_by(CODMUNRES) %>% 
  summarise(TO_FT = sum(TIPOBITO == "Fetal", na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 2.2 Total de óbitos neonatais

tab = dados_sim_2 %>% group_by(CODMUNRES) %>% 
  summarise(TO_NT = sum(IDADE <= 123 | (IDADE >= 200 & IDADE <= 227), na.rm = TRUE), # neonatais
            TO_NT_P = sum(IDADE <= 123 | (IDADE >= 200 & IDADE <= 206), na.rm = TRUE), # neonatais preconces
            TO_NT_T = sum(IDADE >= 207 & IDADE <= 227, na.rm = TRUE), # neonatais tardios
            TO_PNT = sum(IDADE >= 228 & IDADE <= 311, na.rm = TRUE)) # pós-neonatais
base = base %>% left_join(tab, by = "CODMUNRES")

### 2.3 Óbitos maternos durante a gestação (antes do parto)

tab = dados_sim_2 %>% group_by(CODMUNRES) %>% 
  summarise(TO_MT_G = sum(TPMORTEOCO == "Na gravidez", na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 2.4 Óbitos neonatais por raça

tab = dados_sim_2 %>% group_by(CODMUNRES) %>% 
  summarise(TONT_B = sum(RACACOR == "Branca" & (IDADE <= 123 | (IDADE >= 200 & IDADE <= 227)), na.rm = TRUE),
            TONT_PT = sum(RACACOR == "Preta" & (IDADE <= 123 | (IDADE >= 200 & IDADE <= 227)), na.rm = TRUE),
            TONT_A = sum(RACACOR == "Amarela" & (IDADE <= 123 | (IDADE >= 200 & IDADE <= 227)), na.rm = TRUE),
            TONT_PD = sum(RACACOR == "Parda" & (IDADE <= 123 | (IDADE >= 200 & IDADE <= 227)), na.rm = TRUE),
            TONT_I = sum(RACACOR == "Indígena" & (IDADE <= 123 | (IDADE >= 200 & IDADE <= 227)), na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 3. Informações Maternas

### 3.1 Total de óbitos maternos 

tab = dados_sim_2 %>% group_by(CODMUNRES) %>% 
  summarise(TO_MT = sum(TPMORTEOCO != "Não ocorreu nestes períodos", na.rm = TRUE),
            TO_MT_DG = sum(TPMORTEOCO == "Na gravidez", na.rm = TRUE),
            TO_MT_PT = sum(TPMORTEOCO == "No parto", na.rm = TRUE),
            TO_MT_AB = sum(TPMORTEOCO == "No abortamento", na.rm = TRUE),
            TO_MT_42 = sum(TPMORTEOCO == "Até 42 dias após o término do parto", na.rm = TRUE),
            TO_MT_43 = sum(TPMORTEOCO == "De 43 dias a 1 ano após o término da gestação", na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 3.2 Total de óbitos maternos precoces

tab = dados_sim_2 %>% group_by(CODMUNRES) %>% 
  summarise(TO_MT_P = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento",
                                            "Até 42 dias após o término do parto"), 
                          na.rm = TRUE),
            TO_MT_P_I = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento",
                                              "Até 42 dias após o término do parto")
                            & SEXO == "Feminino" & IDADE >= 415 & IDADE <= 449, na.rm = TRUE),
            TO_MT_P_ES = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento",
                                               "Até 42 dias após o término do parto")
                             & ESC2010 == "Sem escolaridade", na.rm = TRUE),
            TO_MT_P_EFI = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento",
                                                "Até 42 dias após o término do parto")
                              & ESC2010 == "Fundamental I (1a a 4a série)", na.rm = TRUE),
            TO_MT_P_EFII = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento",
                                                 "Até 42 dias após o término do parto")
                               & ESC2010 == "Fundamental II (5a a 8a série)", na.rm = TRUE),
            TO_MT_P_EM = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento",
                                               "Até 42 dias após o término do parto")
                             & ESC2010 == "Médio (antigo 2o Grau)", na.rm = TRUE),
            TO_MT_P_ESI = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento",
                                                "Até 42 dias após o término do parto")
                              & ESC2010 == "Superior incompleto", na.rm = TRUE),
            TO_MT_P_ESC = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento",
                                                "Até 42 dias após o término do parto")
                              & ESC2010 == "Superior completo", na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### Adicionando a variável ANO, NIVEL e a linha de UF

### ANO

dados_sim_2 = dados_sim_2 %>%
  mutate(ANO = "2015")  %>%
  relocate(ANO, .before = 1)
base = base %>%
  mutate(ANO = "2015")  %>%
  relocate(ANO, .before = 1)

### Criando df com dados de SP

estado_sp = dados_sim_2 %>%
  summarise(ANO = unique(ANO)[1],
            CODMUNRES = 35,
    
    ### Total de óbitos e total de óbitos c/ registro completo
    
    TO = n(),
    TORC = sum(complete.cases(dados_sim)),
    TORCR = sum(complete.cases(.)),
    
    ### Total de óbitos Naturais/Não naturais
    
    {ext = substr(CAUSABAS, 1, 1) %in% c("V", "W", "X", "Y")
      tibble(TO_NN = sum(ext, na.rm = TRUE),
             TO_N  = sum(!ext, na.rm = TRUE))},
    
    ### Total de óbitos por categoria
    
    {cid = substr(CAUSABAS, 1, 1)
    tibble(TO_CB_I = sum(cid %in% c("A", "B"), na.rm = TRUE), # doenças infecciosas ou parasitárias
            TO_CB_N = sum(cid %in% c("C", "D"), na.rm = TRUE), # neoplasias ou doenças hematológicas
            TO_CB_C = sum(cid == "I", na.rm = TRUE), # doenças circulatórias
            TO_CB_R = sum(cid == "J", na.rm = TRUE), # doenças respiratórias
            TO_CB_O = sum(!(cid %in% c("A", "B", "C", "D", "I", "J", "V", "W", "X", "Y")),
                      na.rm = TRUE))},
            
            ### Total de óbitos por sexo
            TO_M = sum(SEXO == "Masculino", na.rm = TRUE),
            TO_F = sum(SEXO == "Feminino", na.rm = TRUE),
            
            ### Óbitos femininos em idade fértil
            TO_F_IF = sum(SEXO == "Feminino" & IDADE >= 415 & IDADE <= 449, na.rm = TRUE),
            
            ### Óbitos fetais
            TO_FT = sum(TIPOBITO == "Fetal", na.rm = TRUE),
            
            ### Óbitos neonatais
            TO_NT = sum(IDADE <= 123 | (IDADE >= 200 & IDADE <= 227), na.rm = TRUE), # neonatais
            TO_NT_P = sum(IDADE <= 123 | (IDADE >= 200 & IDADE <= 206), na.rm = TRUE), # neonatais preconces
            TO_NT_T = sum(IDADE >= 207 & IDADE <= 227, na.rm = TRUE), # neonatais tardios
            TO_PNT = sum(IDADE >= 228 & IDADE <= 311, na.rm = TRUE), # pós-neonatais
            
            ### Óbitos maternos durante a gestação (antes do parto)
            
            TO_MT_G = sum(TPMORTEOCO == "Na gravidez", na.rm = TRUE),
            
            ### Óbitos neonatais por raça
            
            TONT_B = sum(RACACOR == "Branca" & (IDADE <= 123 | (IDADE >= 200 & IDADE <= 227)), na.rm = TRUE),
            TONT_PT = sum(RACACOR == "Preta" & (IDADE <= 123 | (IDADE >= 200 & IDADE <= 227)), na.rm = TRUE),
            TONT_A = sum(RACACOR == "Amarela" & (IDADE <= 123 | (IDADE >= 200 & IDADE <= 227)), na.rm = TRUE),
            TONT_PD = sum(RACACOR == "Parda" & (IDADE <= 123 | (IDADE >= 200 & IDADE <= 227)), na.rm = TRUE),
            TONT_I = sum(RACACOR == "Indígena" & (IDADE <= 123 | (IDADE >= 200 & IDADE <= 227)), na.rm = TRUE),
            
            ### Óbitos maternos
            TO_MT = sum(TPMORTEOCO != "Não ocorreu nestes períodos", na.rm = TRUE),
            TO_MT_DG = sum(TPMORTEOCO == "Na gravidez", na.rm = TRUE),
            TO_MT_PT = sum(TPMORTEOCO == "No parto", na.rm = TRUE),
            TO_MT_AB = sum(TPMORTEOCO == "No abortamento", na.rm = TRUE),
            TO_MT_42 = sum(TPMORTEOCO == "Até 42 dias após o término do parto", na.rm = TRUE),
            TO_MT_43 = sum(TPMORTEOCO == "De 43 dias a 1 ano após o término da gestação", na.rm = TRUE),
            
            ### Óbitos maternos precoces
            TO_MT_P = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento",
                                            "Até 42 dias após o término do parto"), 
                          na.rm = TRUE),
            TO_MT_P_I = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento",
                                              "Até 42 dias após o término do parto")
                            & SEXO == "Feminino" & IDADE >= 415 & IDADE <= 449, na.rm = TRUE),
            TO_MT_P_ES = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento",
                                               "Até 42 dias após o término do parto")
                             & ESC2010 == "Sem escolaridade", na.rm = TRUE),
            TO_MT_P_EFI = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento",
                                                "Até 42 dias após o término do parto")
                              & ESC2010 == "Fundamental I (1a a 4a série)", na.rm = TRUE),
            TO_MT_P_EFII = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento",
                                                 "Até 42 dias após o término do parto")
                               & ESC2010 == "Fundamental II (5a a 8a série)", na.rm = TRUE),
            TO_MT_P_EM = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento",
                                               "Até 42 dias após o término do parto")
                             & ESC2010 == "Médio (antigo 2o Grau)", na.rm = TRUE),
            TO_MT_P_ESI = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento",
                                                "Até 42 dias após o término do parto")
                              & ESC2010 == "Superior incompleto", na.rm = TRUE),
            TO_MT_P_ESC = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento",
                                                "Até 42 dias após o término do parto")
                              & ESC2010 == "Superior completo", na.rm = TRUE))

### Certificando que as colunas estão alinhadas e adicionando linha da UF

estado_sp = estado_sp %>%
  select(names(base))

base = bind_rows(base, estado_sp)

# NIVEL

base = base %>% 
  mutate(NIVEL = if_else(CODMUNRES == 35, "UF", "MUNICIPIO")) %>% 
  relocate(NIVEL, .before = 2)

SIM_SP = base %>%
  arrange(CODMUNRES != 35)

# Tarefa 8: Exporte o banco de dados com o nome SIM_UF.csv

write.csv(base, "SIM_SP.csv", row.names = FALSE)

# Ao terminar a ETAPA 2 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 2"
# Faça um merge de script de SIM para main

#####################################################
# ETAPA 3: OUTROS BANCOS DE DADOS: IBGE, SNIS, ...
#####################################################
# Só inicie esta Etapa quando a professora orientar
# Abra um branch OUTROS
# Na branch OUTROS escreva os comandos das Tarefa 1 a 3  abaixo

# Tarefa 1. Acesso aos bancos de dados do SIDRA e obtenção da informação
# Leia os arquivos:
# 1. população residente estimada - UF e municípios - 2015 - SIDRA - tabela_6579.csv  
# 2. população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv  
# 3. população residente censo 2010 - por faixa etária -  UF - SIDRA - tabela_1552.csv
# 4. população residente censo 2010 - por faixa etária e sexo -  municípios - SIDRA - tabela_1552.csv

# A partir dos arquivos acima gere o banco de dados de nome SIDRA_UF com as seguintes variáveis:
# 1  ANO    
# 2  NIVEL
# 3  CODMUNRES
# 4 POPRE_T
# 5 POPRC_T
# 6 POPRC_M
# 7 POPRC_F
# 8 POPRC_15
# 9 POPRC_15_49
# 10 POPRC_50
# 11 POPRC_F_15
# 12 POPRC_F_15_49
# 13 POPRC_F_50

# Exporte o arquivo em formato CSV
# Faça o commit com a mensagem "Script e dados TAREFA 3 - SIDRA"

# Tarefa 2: Acesso aos bancos de dados do SINISA e obtenção da informação
# Escreva os comandos da Tarefa 2 estando na branch OUTROS# Leia o arquivo agua e esgoto - município - 2015.csv 
# A partir do arquivo acima gere o banco de dados de nome SINISA_UF com as seguintes variáveis:
# 1  ANO    
# 2  NIVEL
# 3  CODMUNRES
# 4 POPR_RA
# 5 POPR_RE

# Exporte o arquivo em formato CSV
# Faça o commit com a mensagem "Script e dados TAREFA 3 - SINISA"


# Tarefa 3: Acesso aos bancos de dados do ATLAS  e obtenção da informação
# Escreva os comandos da Tarefa 3 estando na branch OUTROS
# Leia os arquivos:
# 1. códigos dos municípios - 2010.csv      
# 2. IDHM - 2010 (CENSO) e 2015 (PNAD) - total e por sexo - UF - Atlas Brasil.csv
# 3. IDHM - 2010 - municípios - Atlas Brasil.csv
# A partir do arquivo acima gere o banco de dados de nome ATLAS_UF com as seguintes variáveis:
# 1  ANO    
# 2  NIVEL
# 3  CODMUNRES
# 4 IDHM_A
# 5 IDHM_CA
# 6 IDHM_CA_M
# 7 IDHM_CA_F

# Exporte o arquivo em formato CSV# Faça o commit com a mensagem "Script e dados TAREFA 3 - ATLAS"

### Leitura dos bancos de dados

sidra1 = read.csv("população residente estimada - UF e municípios - 2015 - SIDRA - tabela_6579.csv", header = TRUE, sep = ";")
sidra2 = read.csv("população residente censo 2010 - por faixa etária -  UF - SIDRA - tabela_1552.csv", header = TRUE, sep = ";")
sidra3 = read.csv("população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv",
                  header = TRUE, sep = ";")
sidra4 = read.csv("população residente censo 2010 - por faixa etária e sexo -  municípios - SIDRA - tabela_1552.csv", header = TRUE,
                  sep = ";")

### Gerando df

base = sidra1 %>%
  filter(substr(CODMUNRES, 1, 2) == "35")
base = data.frame(CODMUNRES = sort(unique(base$CODMUNRES)))

### Adicionando as variáveis ANO e NIVEL

base = base %>%
  mutate(ANO = "2015")  %>%
  relocate(ANO, .before = 1)

base = base %>%
  mutate(NIVEL = if_else(CODMUNRES == 35, "UF", "MUNICIPIO")) %>%
  relocate(NIVEL, .before = 2)

### 1. População total residente estimada

tab = sidra1 %>% select(-NOME)
base = base %>% left_join(tab, by = "CODMUNRES")

### 2. População total residente em CENSO anterior a ANO (CENSO 2010)

tab = sidra3 %>% select(-NOME, -POPRC_M, -POPRC_F)
base = base %>% left_join(tab, by = "CODMUNRES")

### 3. População residente em CENSO anterior a ANO (CENSO 2010) por sexo

tab = sidra3 %>% select(-NOME, -POPRC_T, -POPRC_F) #Masculina
base = base %>% left_join(tab, by = "CODMUNRES")

tab = sidra3 %>% select(-NOME, -POPRC_T, -POPRC_M) #Feminina
base = base %>% left_join(tab, by = "CODMUNRES")

### 4. População residente em CENSO anterior a ANO (CENSO 2010) por faixa etária

tab = sidra4 %>% group_by(CODMUNRES) %>%
  summarise(POPRC_15 = sum(case_when(F_IDADE %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos") ~ POP,
                           TRUE ~ 0), na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

tab = sidra4 %>% group_by(CODMUNRES) %>%
  summarise(POPRC_15_49 = sum(case_when(F_IDADE %in% c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos",
                                                    "30 a 34 anos", "35 a 39 anos", "40 a 44 anos",
                                                    "45 a 49 anos") ~ POP, TRUE ~ 0), na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

tab = sidra4 %>% group_by(CODMUNRES) %>%
  summarise(POPRC_50 = sum(case_when(F_IDADE %in% c("50 a 54 anos", "55 a 59 anos", "60 a 64 anos",
                                                    "65 a 69 anos", "70 a 74 anos", "75 a 79 anos",
                                                    "80 a 89 anos", "90 a 99 anos", "100 anos ou mais") ~ POP,
                                     TRUE ~ 0), na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 5. População feminina residente em CENSO anterior a ANO (CENSO 2010) por faixa etária

### 5.1 <15 anos
tab = sidra4 %>% group_by(CODMUNRES) %>%
  summarise(POPRC_F_15 = sum(case_when(F_IDADE %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos") ~ POPF,
                                     TRUE ~ 0), na.rm = TRUE)) 
base = base %>% left_join(tab, by = "CODMUNRES")

### 5.2 >=15 e <=49
tab = sidra4 %>% group_by(CODMUNRES) %>%
  summarise(POPRC_F_15_49 = sum(case_when(F_IDADE %in% c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos",
                                                       "30 a 34 anos", "35 a 39 anos", "40 a 44 anos",
                                                       "45 a 49 anos") ~ POPF, TRUE ~ 0), na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### 5.3 >=50

tab = sidra4 %>% group_by(CODMUNRES) %>%
  summarise(POPRC_F_50 = sum(case_when(F_IDADE %in% c("50 a 54 anos", "55 a 59 anos", "60 a 64 anos",
                                                    "65 a 69 anos", "70 a 74 anos", "75 a 79 anos",
                                                    "80 a 89 anos", "90 a 99 anos", "100 anos ou mais") ~ POPF,
                                     TRUE ~ 0), na.rm = TRUE))
base = base %>% left_join(tab, by = "CODMUNRES")

### Preenchendo linha do município

base[1, 8] = sum(base[2:646, 8])
base[1, 9] = sum(base[2:646, 9])
base[1, 10] = sum(base[2:646, 10])
base[1, 11] = sum(base[2:646, 11])
base[1, 12] = sum(base[2:646, 12])
base[1, 13] = sum(base[2:646, 13])

# Exporte o arquivo em formato CSV
# Faça o commit com a mensagem "Script e dados TAREFA 3 - SIDRA"

### Gerando e exportando arquivo

SIDRA_SP = base
write.csv(SIDRA_SP, "SIDRA_SP.csv", row.names = FALSE)

### Tarefa 2: Leitura dos bancos de dados

sinisa_esg = read.csv("agua e esgoto - município - 2015 - agua e esgoto - município - 2015.csv", header = TRUE, sep = ",") %>%
  filter(substr(CODMUNRES, 1, 2) == "35")

### Incluindo a variável ANO  e NIVEL e excluindo colunas

SINISA_SP = sinisa_esg %>%
  mutate(ANO = "2015")  %>%
  relocate(ANO, .before = 1)

SINISA_SP = SINISA_SP[, c(-3,-4,-5)]

total_ra = sum(SINISA_SP$POPR_RA, na.rm = TRUE)
total_re = sum(SINISA_SP$POPR_RE, na.rm = TRUE)
SINISA_SP = SINISA_SP %>% add_row(.before = 1,
                                  ANO = "2015",
                                  CODMUNRES = 35,
                                  POPR_RA = total_ra,
                                  POPR_RE = total_re)

SINISA_SP = SINISA_SP %>%
  mutate(NIVEL = if_else(CODMUNRES == 35, "UF", "MUNICIPIO")) %>%
  relocate(NIVEL, .before = 2)

### Exportando arquivo

write.csv(SINISA_SP, "SINISA_SP.csv", row.names = FALSE)

### Tarefa 3: Leitura dos bancos de dados

codmun = read.csv("códigos dos municípios - 2010.csv",
                  header = TRUE,
                  sep = ";") %>%
  select(-X) %>%
  filter(substr(CODMUNRES, 1, 2) == "35")

atlas_1 = read.csv("IDHM - 2010 - municípios - Atlas Brasil.csv",
                   header = TRUE, 
                   sep = ";") %>% 
  select(where(~ !all(is.na(.) | . == "")))

atlas_2 = read.csv("IDHM - 2010 (CENSO) e 2015 (PNAD) - total e por sexo - UF - Atlas Brasil.csv",
                   header = TRUE,
                   sep =  ";") %>% 
  select(where(~ !all(is.na(.) | . == "")))

### Gerando df e adicionando a variáveil ANO 

base = codmun %>%
  mutate(ANO = "2015")  %>%
  relocate(ANO, .before = 1)

### Adicionando linha da UF

base = base %>%
  add_row(ANO = "2015", CODMUNRES = 35, .before = 1) %>% 
  mutate(NIVEL = if_else(CODMUNRES == 35, "UF", "MUNICIPIO")) %>%
  relocate(NIVEL, .before = 2)

### Unindo código dos municípios aos dados

library(stringr)

atlas_1$municipiostr = str_sub(atlas_1$município, end = -6)
atlas_1 = atlas_1 %>% 
  select(-município) %>% 
  rename(município = municipiostr)

codmun$município = str_replace(codmun$município,
                               "Moji Mirim",
                               "Mogi Mirim") # Consertando erro de grafia

atlas_1 = atlas_1 %>% left_join(codmun, by = "município")
base = base %>% left_join(atlas_1, by = "CODMUNRES")

base = base %>% 
  select(-c(município.x, município.y)) %>% 
  rename(IDHM_A = IDHM_2010)

### Preenchendo linha da UF

base [1, 4] = atlas_2 [26, 3]

### Variáveis com informação apenas da UF

base = base %>% 
  mutate(IDHM_CA = NA, 
         IDHM_CA_M = NA,
         IDHM_CA_F = NA)
base [1, 5] = atlas_2 [26, 2] #IDHM da população residente em CENSO anterior a ANO (CENSO 2010)
base [1, 6] = atlas_2 [26, 4] #IDHM da população masculina residente em CENSO anterior a ANO (CENSO 2010)
base [1, 7] = atlas_2 [26, 6] #IDHM da população feminina residente em CENSO anterior a ANO (CENSO 2010)

ATLAS_SP = base

### Exportando arquivo

write.csv(ATLAS_SP, "ATLAS_SP.csv", row.names = FALSE)

################################################################
# ETAPA 4: GERAR BANCO DE DADOS FINAL DO ESTADO COM DADOS DO SIDRA, ATLAS, SINASC, SIM, SINISA E INDICADORES
################################################################


# Tarefa 1: Fazer o merge dos bancos de dados criados nas etapas anteriores (SIDRA_UF, ATLAS_ UF,  SINASC_UF, SIM_UF e SINISA_UF), 
# sendo que as variáveis deverão seguir a ordem

# ANO, NIVEL, CODMUNRES (uma única vez), variáveis do SIDRA, do ATLAS, do SINASC, do SIM e da SINISA. No merge deve constar 
# qualquer município que esteja em pelo menos um dos bancos

# Chamar o banco de dados de DA_UF

# Após o merge dos bancos, fazer commit “Script e dados agregados da UF”


# Tarefa 2: Acrescentar no banco DA_UF os indicadores TFG, TMG, RMM, TMM, TMM_P, TMN, TMN_P, TMN_T e TMI e chamar o banco 
# de BDEM_UF_2015

# Após a criação do banco, fazer commit “Script e dados BDEM_UF_2015”

### Unindo SIDRA e ATLAS

BD1 = merge(SIDRA_SP, ATLAS_SP, 
            by=c("ANO", "NIVEL", "CODMUNRES"), 
            all=TRUE)

### Unindo SINASC + SIM + SINISA

BD2 = merge(SINASC_SP, SIM_SP, 
            by=c("ANO", "NIVEL", "CODMUNRES"), 
            all=TRUE)

BD2 = merge(BD2, SINISA_SP, 
        by=c("ANO", "NIVEL", "CODMUNRES"), 
        all=TRUE)

############################################################################################
# ETAPA 5: EMPILHAMENTO DOS DATAFRAMES DE CADA ESTADO, GERANDO UM DATAFRAME DE 27 LINHAS
############################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 5

# 1. Enviar arquivos para as pastas do repositório da Professora no GitHUb
# 2. A professora fará o empilhamentos dos dataframes

