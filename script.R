# Bibliotecas ---------------------------------------------------------------
library(ggplot2)
library(ltm)
library(irtoys)

options(scipen = 5)

# Entrada dos Dados ---------------------------------------------------------
list.files()

# # Informações para o trabalho
# inf <- list()

# ?read.table
# Respondentes
resp <- read.csv2("data.csv", na.strings =  "")

# Gabarito
gab <- read.csv2("gab.csv",  head = FALSE)

# Fatores associados --------------------------------------------------------
fatores <- resp[, 27:29]

# Guarda questões dos fatores associados
fatores.cols <- names(fatores)
fatores.cols <- gsub("[.]", " ", fatores.cols)
fatores.cols <- paste0(fatores.cols, "?")

colnames(fatores) <- paste0("X", 1:3)

fatores$X1 <- forcats::fct_infreq(fatores$X1)
fatores$X2 <- forcats::fct_infreq(fatores$X2)
fatores$X3 <- forcats::fct_infreq(fatores$X3)


p1 <- ggplot(as.data.frame(table(fatores$X1, useNA = "always")),
             aes(x =  Var1, y =  Freq)) +
    geom_bar(stat =  "identity",  fill = "dodgerblue4") +
    labs(x = "", y = "Frequência") +
    geom_text(aes(y = Freq + 10, label = Freq), size = 5) +
    ggtitle(fatores.cols[1])

p2 <- ggplot(as.data.frame(table(fatores$X2, useNA = "always")),
             aes(x =  Var1, y =  Freq)) +
    geom_bar(stat =  "identity",  fill = "dodgerblue4") +
    labs(x = "", y = "Frequência") +
    geom_text(aes(y = Freq + 10, label = Freq), size = 5) +
    ggtitle(fatores.cols[2])

p3 <- ggplot(as.data.frame(table(fatores$X3, useNA = "always")),
             aes(x =  Var1, y =  Freq)) +
    geom_bar(stat =  "identity",  fill = "dodgerblue4") +
    labs(x = "", y = "Frequência") +
    geom_text(aes(y = Freq + 10, label = Freq), size = 5) +
    ggtitle(fatores.cols[3])

p1
p2
p3


# Tratamento dos Dados ------------------------------------------------------
resp <- resp[, 1:26]

# Guarda questões
resp.cols <- names(resp)

# Renomeia os itens
colnames(resp) <- paste0("i", 1:26)

table(resp$i1,useNA = "always") # i1

# Quantidade de NA por linha
na_per_row <- apply(resp, 1, function(x) sum(is.na(x)))
na_per_row

# Individuos com mais de 3 itens sem respostas descartados
exc <- na_per_row > 3
exc

resp <- resp[!exc, ]

# Tratamento Gabarito -------------------------------------------------------
# Renomeia os itens
colnames(gab) <- paste0("i", 1:26)


# Corrige o questionario ----------------------------------------------------
?mult.choice
str(resp)
str(gab)

# Adiciona os levels de resp ~> gab
lvls <- lapply(resp, levels)

for (i in 1:26) {
    gab[[i]] <- factor(gab[[i]],  lvls[[i]])
}

str(gab)

# Correção
quest <- mult.choice(resp, as.numeric(gab))
head(quest)

# Assume NA = 0, não respondente
quest <- ifelse(is.na(quest),0, quest)

?descript
desc <- descript(quest)
desc

# Frequência do total de ESCORE
## Consistencia razoavel
cronbach.alpha(quest)

# Histograma da frequência de acertos ---------------------------------------
acertos <- rowSums(quest)
ggplot(as.data.frame(acertos),  aes(x =  as.factor(acertos))) +
    geom_bar(fill = "dodgerblue4") +
    labs(x =  "Número de Acertos", y = "Frequência")

# Modelo 2 Parâmetros -------------------------------------------------------
m2p <- est(quest, model =  "2PL", engine = "ltm", nqp = 21)
m2p

m2p$est

# Curva de Informação do Item
plot(iif(m2p))

# Probabilidade de Responder Corretamente
plot(irf(m2p$est))

# Informação do Teste
plot(tif(m2p$est))

# Estima o traço latente posteriori
tlp <- eap(quest, m2p$est,qu=normal.qu())


final.rank <- data.frame('escore'= tlp[,1],
                       'posição'= rank(tlp[,1]),
                       'acertos'= sfsmisc::margin2table(quest)[-550,27])
head(final.rank)

final.acertos <- final.rank[order(final.rank$acertos),]
head(final.acertos)
tail(final.acertos)

final.escore <- final.rank[order(final.rank$escore),]
head(final.escore)
tail(final.escore)
