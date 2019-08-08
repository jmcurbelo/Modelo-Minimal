set.seed(12345)

# Reading Data base
library(foreign)
library(tools)

datos<- read.spss("./Data/BASE.SAV",use.value.labels=TRUE, max.value.labels=TRUE,
                  to.data.frame=TRUE)

# Creating the vectors with the observations of glucose
glu_basal<- datos$Gluc_basal
glu_30<- datos$Gluc_30
glu_60<- datos$Gluc_60
glu_90<- datos$Gluc_90
glu_120<- datos$Gluc_120

# Creating the matrix with the estimations
estimaciones<-matrix(nrow = 98,ncol = 7)
# "Si","p2","G0","k1","k2","k3","k4"

# Creating the status bar
pb<-txtProgressBar(min = 0,max = 98,style = 3)


#////////////////////////////////////
# for to tour all subjects
#////////////////////////////////////
for (w in 1:98) {
    # timeout to update status bar
    Sys.sleep(0.00000000000001)
    
    # Reading the insulin's data
    ins<-read.csv(paste("./Ins/ins",w,sep = ""))
    ins<-ins[,2]
    
    # Reading the glucose's data
    y<-rep(NA, times=120)
    
    y[1]<- glu_basal[w]
    y[30]<- glu_30[w]
    y[60]<- glu_60[w]
    y[90]<- glu_90[w]
    y[120]<- glu_120[w]
    
    
    #########################################
    #  THE MODEL
    #########################################  
    
    cat(
        "model
        {
        
        b[1]=G0
        c[1]<- 0
        
        
        y[1]~dnorm(b[1],tau) 
        
        ################## 2-30 ####################
        
        for (i in 2:30){
        
        y[i]~dnorm(b[i],tau)
        
        b[i] = b[i-1]+1/6*(-(0.014+c[i-1])*b[i-1]+0.014*120+((k1/30)*i)/1.7+2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+((k1/30)*i)/1.7)+0.014*120+((k1/30)*i)/1.7+2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+((k1/30)*i)/1.7)+0.014*120+((k1/30)*i)/1.7)+0.014*120+((k1/30)*i)/1.7 +-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+((k1/30)*i)/1.7)+0.014*120+((k1/30)*i)/1.7)+0.014*120+((k1/30)*i)/1.7 )+0.014*120+((k1/30)*i)/1.7)
        c[i] = c[i-1]+1/6*(-p2*(c[i-1]-Si*(ins[i]-10))+2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))+2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10))+-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10)))
        
        }
        
        ################## 31-60 ####################
        
        for (i in 31:60){
        
        y[i]~dnorm(b[i],tau)
        
        b[i] = b[i-1]+1/6*(-(0.014+c[i-1])*b[i-1]+0.014*120+(k1+(k2-k1)/(30)*(i-30))/1.7+2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+(k1+(k2-k1)/(30)*(i-30))/1.7)+0.014*120+(k1+(k2-k1)/(30)*(i-30))/1.7+2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+(k1+(k2-k1)/(30)*(i-30))/1.7)+0.014*120+(k1+(k2-k1)/(30)*(i-30))/1.7)+0.014*120+(k1+(k2-k1)/(30)*(i-30))/1.7 +-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+(k1+(k2-k1)/(30)*(i-30))/1.7)+0.014*120+(k1+(k2-k1)/(30)*(i-30))/1.7)+0.014*120+(k1+(k2-k1)/(30)*(i-30))/1.7 )+0.014*120+(k1+(k2-k1)/(30)*(i-30))/1.7)
        c[i] = c[i-1]+1/6*(-p2*(c[i-1]-Si*(ins[i]-10))+2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))+2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10))+-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10)))
        
        }
        
        ################## 61-90 ####################
        
        for (i in 61:90){
        
        y[i]~dnorm(b[i],tau)
        
        b[i] = b[i-1]+1/6*(-(0.014+c[i-1])*b[i-1]+0.014*120+(k2+(k3-k2)/(30)*(i-60))/1.7+2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+(k2+(k3-k2)/(30)*(i-60))/1.7)+0.014*120+(k2+(k3-k2)/(30)*(i-60))/1.7+2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+(k2+(k3-k2)/(30)*(i-60))/1.7)+0.014*120+(k2+(k3-k2)/(30)*(i-60))/1.7)+0.014*120+(k2+(k3-k2)/(30)*(i-60))/1.7 +-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+(k2+(k3-k2)/(30)*(i-60))/1.7)+0.014*120+(k2+(k3-k2)/(30)*(i-60))/1.7)+0.014*120+(k2+(k3-k2)/(30)*(i-60))/1.7 )+0.014*120+(k2+(k3-k2)/(30)*(i-60))/1.7)
        c[i] = c[i-1]+1/6*(-p2*(c[i-1]-Si*(ins[i]-10))+2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))+2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10))+-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10)))
        
        
        }
        
        ################## 91-120 ####################        
        
        for (i in 91:120){
        
        y[i]~dnorm(b[i],tau)
        
        b[i] = b[i-1]+1/6*(-(0.014+c[i-1])*b[i-1]+0.014*120+(k3+(k4-k3)/(30)*(i-90))/1.7+2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+(k3+(k4-k3)/(30)*(i-90))/1.7)+0.014*120+(k3+(k4-k3)/(30)*(i-90))/1.7+2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+(k3+(k4-k3)/(30)*(i-90))/1.7)+0.014*120+(k3+(k4-k3)/(30)*(i-90))/1.7)+0.014*120+(k3+(k4-k3)/(30)*(i-90))/1.7 +-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+(k3+(k4-k3)/(30)*(i-90))/1.7)+0.014*120+(k3+(k4-k3)/(30)*(i-90))/1.7)+0.014*120+(k3+(k4-k3)/(30)*(i-90))/1.7 )+0.014*120+(k3+(k4-k3)/(30)*(i-90))/1.7)
        c[i] = c[i-1]+1/6*(-p2*(c[i-1]-Si*(ins[i]-10))+2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))+2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10))+-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10)))
        
        
        }
        
        ###### Prior´s parameters #######
        
        G0~dunif(0,800)
        p2~dunif(0,1)
        Si~dunif(0,10E-4)
        k1~dunif(0,50)
        k2~dunif(-20,20)
        k3~dunif(0,30)
        k4~dunif(-20,30)
        
        tau ~ dgamma(1.0E-3, 1.0E-3)
        
        }",file="Modelo.jags",fill=T
 )
    
    
    b = rep(NA, times=121)
    c = rep(NA, times=121)
    
    b[121]<-0
    c[121]<-0
    
    dat<- list("y","ins","b","c")
    
    par<- c("G0","p2","Si","k1","k2","k3","k4")
    
    library(rjags)
    library(R2jags)
    library(R2WinBUGS)
    library(jagsUI)
    library(coda)
    
    est_MM<- autojags(data = dat,  
                      parameters.to.save = par, n.chains = 3, 
                      n.burnin = 0,n.thin = 3,
                      model.file = "Modelo.jags",parallel = TRUE,n.cores = 3,
                      iter.increment = 20000,Rhat.limit = 1.1,max.iter = 1000000)
    
    estimaciones_temp<-est_MM$mean
    
    estimaciones[w,1]<-estimaciones_temp$G0
    estimaciones[w,2]<-estimaciones_temp$Si
    estimaciones[w,3]<-estimaciones_temp$p2
    estimaciones[w,4]<-estimaciones_temp$k1
    estimaciones[w,5]<-estimaciones_temp$k2
    estimaciones[w,6]<-estimaciones_temp$k3
    estimaciones[w,7]<-estimaciones_temp$k4
    
    # Update the status bar
    setTxtProgressBar(pb,w)
    
}

# Closing the status bar
close(pb)

write.csv(estimaciones,
          file = "./Estimations/all_Subjects_Bayes")
