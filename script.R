# Bibliotecas ---------------------------------------------------------------
library(ggplot2)


# Entrada dos Dados ---------------------------------------------------------
list.files()

# ?read.table
# Respondentes
resp <- read.csv2("data.csv", na.strings =  "")
dim(resp)

# Gabarito


# Fatores associados --------------------------------------------------------
fatores <- resp[, 27:29]

# Guarda questões dos fatores associados
fatores.cols <- names(fatores)

colnames(fatores) <- paste0("X", 1:3)

qplot(x =  X1, data = fatores, main =  fatores.cols[1])
qplot(x =  X2, data = fatores, main =  fatores.cols[2])
qplot(x =  X3, data = fatores, main =  fatores.cols[3])


# Tratamento dos Dados ------------------------------------------------------
resp <- resp[, 1:26]

# Guarda questões
resp.cols <- names(resp)

# Renomeia os itens
colnames(resp) <- paste0("i", 1:26)

table(resp$i1,useNA = "always") # i1


# Tratamento Gabarito -------------------------------------------------------
