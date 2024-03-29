library(foreign)
library(ggplot2)

datos<- read.spss("./Data/BASE.SAV",use.value.labels=TRUE, max.value.labels=TRUE,
                  to.data.frame=TRUE)

# creando los vectores con las obs de glucosa
glu_basal<- datos$Gluc_basal
glu_30<- datos$Gluc_30
glu_60<- datos$Gluc_60
glu_90<- datos$Gluc_90
glu_120<- datos$Gluc_120

# Leyendo las estimaciones de Bayes
est_Bayes<-read.csv("./Estimations/all_Subjects_Bayes")
est_Bayes<-est_Bayes[,2:8]
#"G0","Si","p2","k1","k2","k3","k4"

#////////////////////////////////////
# for para recorrer todos los sujetos
#////////////////////////////////////
for (w in 1:98) {
    
    # leyendo los datos de insulina
    
    ins<-read.csv(paste("./Ins/ins",w,sep = ""))
    ins<-ins[,2]
    
    y<-rep(NA, times=120)
    
    y[1]<- glu_basal[w]
    y[30]<- glu_30[w]
    y[60]<- glu_60[w]
    y[90]<- glu_90[w]
    y[120]<- glu_120[w]
    
    
    b = rep(NA, times=120)
    c = rep(NA, times=120)
    
    # Parametros
    G0<- est_Bayes$V1[w]
    Si<- est_Bayes$V2[w]
    p2<- est_Bayes$V3[w]  
    k1<- est_Bayes$V4[w]
    k2<- est_Bayes$V5[w]
    k3<- est_Bayes$V6[w]
    k4<- est_Bayes$V7[w]
    
    
    # Condiciones Iniciales
    b[1]=G0
    c[1]<- 0
    
    
    # 0-30 min
    for (i in 2:30){
        
        
        b[i] = b[i-1]+1/6*(-(0.014+c[i-1])*b[i-1]+0.014*120+((k1/30)*i)/1.7+2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+((k1/30)*i)/1.7)+0.014*120+((k1/30)*i)/1.7+2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+((k1/30)*i)/1.7)+0.014*120+((k1/30)*i)/1.7)+0.014*120+((k1/30)*i)/1.7 +-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+((k1/30)*i)/1.7)+0.014*120+((k1/30)*i)/1.7)+0.014*120+((k1/30)*i)/1.7 )+0.014*120+((k1/30)*i)/1.7)
        c[i] = c[i-1]+1/6*(-p2*(c[i-1]-Si*(ins[i]-10))+2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))+2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10))+-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10)))
        
    }
    
    # 31-60 min
    for (i in 31:60){
        
        
        b[i] = b[i-1]+1/6*(-(0.014+c[i-1])*b[i-1]+0.014*120+(k1+(k2-k1)/(30)*(i-30))/1.7+2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+(k1+(k2-k1)/(30)*(i-30))/1.7)+0.014*120+(k1+(k2-k1)/(30)*(i-30))/1.7+2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+(k1+(k2-k1)/(30)*(i-30))/1.7)+0.014*120+(k1+(k2-k1)/(30)*(i-30))/1.7)+0.014*120+(k1+(k2-k1)/(30)*(i-30))/1.7 +-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+(k1+(k2-k1)/(30)*(i-30))/1.7)+0.014*120+(k1+(k2-k1)/(30)*(i-30))/1.7)+0.014*120+(k1+(k2-k1)/(30)*(i-30))/1.7 )+0.014*120+(k1+(k2-k1)/(30)*(i-30))/1.7)
        c[i] = c[i-1]+1/6*(-p2*(c[i-1]-Si*(ins[i]-10))+2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))+2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10))+-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10)))
        
    }
    
    # 61-90 min
    for (i in 61:90){
        
        
        b[i] = b[i-1]+1/6*(-(0.014+c[i-1])*b[i-1]+0.014*120+(k2+(k3-k2)/(30)*(i-60))/1.7+2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+(k2+(k3-k2)/(30)*(i-60))/1.7)+0.014*120+(k2+(k3-k2)/(30)*(i-60))/1.7+2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+(k2+(k3-k2)/(30)*(i-60))/1.7)+0.014*120+(k2+(k3-k2)/(30)*(i-60))/1.7)+0.014*120+(k2+(k3-k2)/(30)*(i-60))/1.7 +-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+(k2+(k3-k2)/(30)*(i-60))/1.7)+0.014*120+(k2+(k3-k2)/(30)*(i-60))/1.7)+0.014*120+(k2+(k3-k2)/(30)*(i-60))/1.7 )+0.014*120+(k2+(k3-k2)/(30)*(i-60))/1.7)
        c[i] = c[i-1]+1/6*(-p2*(c[i-1]-Si*(ins[i]-10))+2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))+2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10))+-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10)))
        
    }
    
    # 91-120 min
    for (i in 91:120){
        
        
        b[i] = b[i-1]+1/6*(-(0.014+c[i-1])*b[i-1]+0.014*120+(k3+(k4-k3)/(30)*(i-90))/1.7+2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+(k3+(k4-k3)/(30)*(i-90))/1.7)+0.014*120+(k3+(k4-k3)/(30)*(i-90))/1.7+2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+(k3+(k4-k3)/(30)*(i-90))/1.7)+0.014*120+(k3+(k4-k3)/(30)*(i-90))/1.7)+0.014*120+(k3+(k4-k3)/(30)*(i-90))/1.7 +-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10)))*(b[i-1]+1/2*-(0.014+c[i-1])*b[i-1]+0.014*120+(k3+(k4-k3)/(30)*(i-90))/1.7)+0.014*120+(k3+(k4-k3)/(30)*(i-90))/1.7)+0.014*120+(k3+(k4-k3)/(30)*(i-90))/1.7 )+0.014*120+(k3+(k4-k3)/(30)*(i-90))/1.7)
        c[i] = c[i-1]+1/6*(-p2*(c[i-1]-Si*(ins[i]-10))+2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))+2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10))+-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]+1/2*-p2*(c[i-1]-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10))-Si*(ins[i]-10)))
        
    }
    
    # Graficando las soluciones
    t<- seq(1,120)
    
    estimaciones<- data.frame(time=t, solucion=b, y=y)
    
    png(filename = paste("./PNG/Subject",w, sep = ""),
        width = 480, height = 480, units = "px")
    
    g<- ggplot(estimaciones, aes(time, solucion))+geom_line(col="blue")+geom_point(aes(time, y), col="red", cex=2)+labs(x="Time (min)", y="Glucose (mg/dl)")+labs(title = paste("Suject",w,sep = " "))
    print(g)
    
    dev.off()
    
}