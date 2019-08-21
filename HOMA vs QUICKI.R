est <- read.csv("./Estimations/all_Subjects_Bayes")

library(dplyr)

est <- tbl_df(est)

est <- est %>%
    select(V1:V7)

names(est) <- c("G0","Si","p2","k1","k2","k3","k4")

# SiHOMA = 22.5/(i*g)
 
# SiQUICKI = 1/(log(I0) +  log(G0))

library(foreign)
data <- read.spss("./Data/BASE.SAV",use.value.labels=TRUE, max.value.labels=TRUE,
                  to.data.frame=TRUE) 
library(dplyr)

data <- tbl_df(data)

glucose <- data %>%
    select(Gluc_basal:Gluc_120)

insuline <- data %>%
    select(Insulina_basal:Insulina_120)

glucoseMean <- apply(glucose, 1, mean)

insulineMean <- apply(insuline, 1, mean)

# SiHOMA = 22.5/(i*g)

SiHOMA = 22.5/(insulineMean*glucoseMean)

est$SiHOMA <- SiHOMA

Si.vs.SiHOMA <- est %>%
    select(Si, SiHOMA) %>%
    print

# SiQUICKI = 1/(log(I0) +  log(G0))

SiQUICKI <- 1/(log(insulineMean) +  log(glucoseMean))

est$SiQUICKI <- SiQUICKI

Si.vs.SiQUICKI <- est %>%
    select(Si, SiQUICKI) %>%
    print

comparision.Si <- est %>%
    select(Si, SiHOMA, SiQUICKI) %>%
    mutate(difference_Si.vs.SiHOMA = abs(Si-SiHOMA), 
           difference_Si.vs.SiQUICKI = abs(Si-SiQUICKI)) %>%
    print
