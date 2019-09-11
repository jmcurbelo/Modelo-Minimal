library(foreign)

data <- read.spss("./Data/BASE MAJO GLUCOSA INSULINA.sav")

library(dplyr)

datos <- tbl_df(data)

indice <- datos %>%
    select(TG, HDL, IMC) %>%
    mutate(SPISE= (600 * (HDL^0.185))/(TG^0.2 * (IMC^1.338)) )

index <- read.csv("./Data/idices.csv")

Si <- index$Si

HOMA <- index$SiHOMA

QUICKI <- index$SiQUICKI

SPISE <- indice$SPISE

indexSI <- data.frame(SI=Si, HOMA=HOMA, QUICKI=QUICKI, SPISE=SPISE)

corMat <- cor(indexSI)

library(reshape2)

melted_corMat <- melt(corMat)

library(ggplot2)

ggplot(data = melted_corMat, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()
