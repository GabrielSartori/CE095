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
fatores <- fatores[!exc, ]

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

# Modelo 3 Parâmetros -------------------------------------------------------
m3p <- est(quest, model =  "3PL", engine = "ltm", nqp = 21)
m3p

m3p$est

# Remove Itens com Discriminação maior que 3
itens <- rownames(m3p$est)[m3p$est[, 1] <= 2.5]

quest2 <- quest[, itens]

# Novo Modelo
m3p1 <- est(quest2, model =  "3PL", engine = "ltm", nqp = 21)

m3p1$est

# Ainda com itens com alto valor discriminatorio

# Remove Itens com Discriminação maior que3
itens2 <- rownames(m3p1$est)[m3p1$est[, 1] <= 3]

quest3 <- quest2[, itens2]

# Novo Modelo
m3p2 <- est(quest3, model =  "3PL", engine = "ltm", nqp = 21)

m3p2$est

# Curva de Informação do Item
plot(iif(m3p2))

# Probabilidade de Responder Corretamente
plot(irf(m3p2$est))

# Informação do Teste
plot(tif(m3p2$est))

# Estima o traço latente posteriori
tlp <- eap(quest3, m3p2$est, qu=normal.qu())

head(tlp)

?rank

final.rank <- data.frame('escore' = tlp[,1],
                         'posição' = rank(-tlp[,1]),
                         'acertos' = rowSums(quest3))

head(final.rank)

final.acertos <- final.rank[order(final.rank$acertos),]
head(final.acertos)
tail(final.acertos)

final.escore <- final.rank[order(final.rank$escore),]
head(final.escore)
tail(final.escore)

head(quest)

# Acrescenta \theta
quest4 <- cbind(quest3, theta = tlp[, 1])
quest4 <- as.data.frame(quest4)

# Itens Ancora --------------------------------------------------------------
colSums(quest4[, -ncol(quest4)]) / nrow(quest4)

# results <- data.frame(
#     item =  integer(),
#     Z =  integer(),
#     I =  character(),
#     II =  character(),
#     III =  character(),
#     status =  character(),
#     stringsAsFactors = FALSE
# )

itens <- colnames(quest4)[-ncol(quest4)]
itens

# args =  itens, theta, matrix de resposta (com ultima coluna theta)

# Theta
theta <- -3:3

cen <- expand.grid(item = 1:length(itens),
                   theta = theta)

# mapply(j = cen$item,
#       k = cen$theta,
#       FUN =  function(j, k) {
#           print(paste0("Item: ", j, " - \theta: ", k))
#       })

anchor <- mapply(j = cen$item,
       k = cen$theta,
       SIMPLIFY = FALSE,
       FUN =  function(j, k) {
           # Theta
           Z <- k
           Y <- Z - 1

           # Item
           item <- itens[j]


           # Condição I
           # P(U = u | \theta = Z) >= 0.65
           z <- quest4[quest4$theta <= Z, item]
           pz <- sum(z) / nrow(quest4)

           I <- pz >=  0.65

           # Condição II
           # P(U = u | \theta = Y) < 0.5
           y <- quest4[quest4$theta <= Y, item]
           py <- sum(y) / nrow(quest4)

           II <- py < 0.5

           # Condição III
           # P(U = u | \theta = Z) - P(U = u | \theta = Y) >= 0.3
           yz <- pz - py

           III <- yz >= 0.3

           status <- ifelse(I & II & III, "ancora",
                     ifelse((I & II) | (I & III) | (II & III),
                            "quase-ancora", "none"))
           data.frame(item, Z, I, II, III, status)
           # results <- rbind(results, data.frame(i, Z, I, II, III, status))

       })

anchor <- do.call(rbind, anchor)
anchor

table(anchor$status)

anchor[anchor$status !=  "none", ]

# Regressão Linear ----------------------------------------------------------
tfatores <- cbind(fatores, theta = tlp[, 1])
tfatores <- as.data.frame(tfatores)
head(tfatores)

str(tfatores)

rl <- lm(theta ~ ., data = tfatores)
summary(rl)

fatores.cols

plot(rl)
