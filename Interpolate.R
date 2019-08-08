
library(foreign)

datos<- read.spss("./Data/BASE.SAV",use.value.labels=TRUE, max.value.labels=TRUE,
                  to.data.frame=TRUE)

ins_basal<-datos$Insulina_basal
ins_30<- datos$Insulina_30
ins_60<- datos$Insulina_60
ins_90<- datos$Insulina_90
ins_120<- datos$Insulina_120


for (z in 1:98) {
    
    
    i<-numeric()
    #Pasar los tiempos (xi) y los datos en cada tiempo (yi)
    xi<-c(1,30,60,90,120)
    yi<-c(ins_basal[z],ins_30[z],ins_60[z],ins_90[z],ins_120[z])
    
    
    # Rellenando los datos conocidos
    for (k in 1:(length(xi))) {
        i[xi[k]]<- yi[k]
        
    }
    
    # Creando la funcion de Interpolacion
    Interpolacion<- function(x,x1,y1,x2,y2)
    {
        ((x-x1)/(x2-x1))*(y2-y1)+y1
    }
    
    # Realizando la primera interpolacion
    x1<-xi[1]      ;y1<-yi[1]
    x2<-xi[2]        ;y2<-yi[2]
    
    for (k in (x1+1):(x2-1)) {
        i[k]<-Interpolacion(k,x1,y1,x2,y2)
    }
    
    
    # Realizando el resto de las interpolaciones
    for (j in 2:(length(xi)-1)) {
        x1<-x2;     y1<-y2
        x2<-xi[j+1];  y2<-yi[j+1]
        
        for (k in (x1+1):(x2-1)) {
            i[k]<-Interpolacion(k,x1,y1,x2,y2)
        }
        
    }
    
    write.csv(i,file =paste("./Ins/ins",z,sep = "") )
    
    rm(i,xi,yi,x1,x2,y1,y2)
    
}

