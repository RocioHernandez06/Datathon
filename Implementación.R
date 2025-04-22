#install.packages("e1071")
#install.packages("lubridate")
#install.packages("rstudioapi")
library(lubridate)
library(e1071)
library(rstudioapi)
set.seed(123)


sim_colas <- function(n_retail,n_banco,n_afiliacion,nu_dias){
  for(dia in nu_dias){
    
    # Tiempo total de simulación
    tiempo_simulacion <- 0.8  
    
    
    
    # Inicialización de colas
    colas <- list(c(), c(), c())
    tiempo_actual <- 0
    servidores_por_cola <- c(n_retail, n_banco, n_afiliacion)
    
    tiempo_fin_servicio <- vector("list", 3)
    
    for (i in 1:3) {
      tiempo_fin_servicio[[i]] <- rep(NA, servidores_por_cola[i])  # Tiempo en que cada servidor estará libre
    }
    
    
    
    suma_tamanos_colas <- c(0, 0, 0)  # Para calcular el tamaño promedio de cada cola
    contador_tiempo <- 0  # Para normalizar el promedio
    
    t_llegada <- sample(dias_semana[[dia]]$llegadas,1)
    # Simulación de eventos
    
    while (tiempo_actual < tiempo_simulacion) {
      # Determinar el siguiente evento: llegada de cliente o salida de cliente atendido
      
      eventos_validos <- c(t_llegada, na.omit(unlist(tiempo_fin_servicio)))
      proximo_evento <- min(eventos_validos)
      tiempo_actual <- proximo_evento
      
      if (tiempo_actual > tiempo_simulacion){ break}
      
      
      # Procesar salidas de clientes
      for (i in 1:3) {
        for (j in 1:servidores_por_cola[i]) {
          while(!is.na(tiempo_fin_servicio[[i]][j]) && tiempo_actual >= tiempo_fin_servicio[[i]][j]) {
            if (length(colas[[i]]) > 0) {
              # Atender al siguiente cliente en la cola
              if(i==1){
                tiempo_servicio <- sample(dias_semana[[dia]]$retail,1)
              }
              if(i==2){
                tiempo_servicio <- sample(dias_semana[[dia]]$banco,1)
              }
              if(i==3){
                tiempo_servicio <- sample(dias_semana[[dia]]$afiliacion,1)
              }
              tiempo_fin_servicio[[i]][j] <- tiempo_actual + tiempo_servicio
              
              # Removemos al cliente de la cola
              colas[[i]] <- colas[[i]][-1]
            } else {
              tiempo_fin_servicio[[i]][j] <- NA  #Servidor libre
            }
          }
        }
      }
      
      
      
      
      # Si el evento es una llegada de cliente
      if (tiempo_actual >= t_llegada) {
        cola_elegida <- sample(1:3, 1, prob = pesos)
        
        # Si el servidor está libre, atiende inmediatamente
        servidor_libre <- which(is.na(tiempo_fin_servicio[[cola_elegida]]))[1]
        
        if (!is.na(servidor_libre)) {
          if(i==1){
            tiempo_servicio <- sample(dias_semana[[dia]]$retail,1)
          }
          if(i==2){
            tiempo_servicio <- sample(dias_semana[[dia]]$banco,1)
          }
          if(i==3){
            tiempo_servicio <- sample(dias_semana[[dia]]$afiliacion,1)
          }
          tiempo_fin_servicio[[cola_elegida]][servidor_libre] <- tiempo_actual + tiempo_servicio
        } else {
          # Agregar cliente a la cola si el servidor está ocupado
          colas[[cola_elegida]] <- c(colas[[cola_elegida]], tiempo_actual)
        }
        
        # Registrar el tamaño actual de la cola
        suma_tamanos_colas[cola_elegida] <- suma_tamanos_colas[cola_elegida] + length(colas[[cola_elegida]])
        contador_tiempo <- contador_tiempo + 1
        
        # Generar el tiempo de la próxima llegada
        t_llegada <- tiempo_actual + sample(dias_semana[[dia]]$llegadas,1)
      }
    }
    
    # Cálculo del tamaño promedio de las colas
    tamanos_promedio_colas[[dia]] <- suma_tamanos_colas / contador_tiempo
  }
 return(tamanos_promedio_colas) 
}

afluencia <- function(n){
  if(n==1){
    return("lunes")}
  if(n==2){
    return("martes")}
  if(n==3){
    return("miércoles")}
  if(n==4){
    return("jueves")}
  if(n==5){
    return("viernes")}
  if(n==6){
    return("sábado")}
  if(n==7){
  return("domingo")
  }
  
}


setwd(dirname(getActiveDocumentContext()$path))

###########
#Segmentar la base de datos original segun el Estado correspondiente


#Obtener el nombre de los archivos .csv ya segmentados
estados <- dir(pattern = ".csv")

#Creacion de los archivos donde se guardara el compilado de informacion obtenida
tienda_info <- list(lunes=data.frame(estado=character(),tienda=character(),llegadas=numeric(),retail=numeric(),banco=numeric(),afiliacion=numeric(),cola_retail=numeric(),cola_banco=numeric(),cola_afiliacion=numeric()),
                    martes=data.frame(estado=character(),tienda=character(),llegadas=numeric(),retail=numeric(),banco=numeric(),afiliacion=numeric(),cola_retail=numeric(),cola_banco=numeric(),cola_afiliacion=numeric()),
                    miércoles=data.frame(estado=character(),tienda=character(),llegadas=numeric(),retail=numeric(),banco=numeric(),afiliacion=numeric(),cola_retail=numeric(),cola_banco=numeric(),cola_afiliacion=numeric()),
                    jueves=data.frame(estado=character(),tienda=character(),llegadas=numeric(),retail=numeric(),banco=numeric(),afiliacion=numeric(),cola_retail=numeric(),cola_banco=numeric(),cola_afiliacion=numeric()),
                    viernes=data.frame(estado=character(),tienda=character(),llegadas=numeric(),retail=numeric(),banco=numeric(),afiliacion=numeric(),cola_retail=numeric(),cola_banco=numeric(),cola_afiliacion=numeric()),
                    sábado=data.frame(estado=character(),tienda=character(),llegadas=numeric(),retail=numeric(),banco=numeric(),afiliacion=numeric(),cola_retail=numeric(),cola_banco=numeric(),cola_afiliacion=numeric()),
                    domingo=data.frame(estado=character(),tienda=character(),llegadas=numeric(),retail=numeric(),banco=numeric(),afiliacion=numeric(),cola_retail=numeric(),cola_banco=numeric(),cola_afiliacion=numeric()))

tienda_info_mejora <- list(lunes=data.frame(estado=character(),tienda=character(),llegadas=numeric(),retail=numeric(),banco=numeric(),afiliacion=numeric(),cola_retail=numeric(),cola_banco=numeric(),cola_afiliacion=numeric()),
                    martes=data.frame(estado=character(),tienda=character(),llegadas=numeric(),retail=numeric(),banco=numeric(),afiliacion=numeric(),cola_retail=numeric(),cola_banco=numeric(),cola_afiliacion=numeric()),
                    miércoles=data.frame(estado=character(),tienda=character(),llegadas=numeric(),retail=numeric(),banco=numeric(),afiliacion=numeric(),cola_retail=numeric(),cola_banco=numeric(),cola_afiliacion=numeric()),
                    jueves=data.frame(estado=character(),tienda=character(),llegadas=numeric(),retail=numeric(),banco=numeric(),afiliacion=numeric(),cola_retail=numeric(),cola_banco=numeric(),cola_afiliacion=numeric()),
                    viernes=data.frame(estado=character(),tienda=character(),llegadas=numeric(),retail=numeric(),banco=numeric(),afiliacion=numeric(),cola_retail=numeric(),cola_banco=numeric(),cola_afiliacion=numeric()),
                    sábado=data.frame(estado=character(),tienda=character(),llegadas=numeric(),retail=numeric(),banco=numeric(),afiliacion=numeric(),cola_retail=numeric(),cola_banco=numeric(),cola_afiliacion=numeric()),
                    domingo=data.frame(estado=character(),tienda=character(),llegadas=numeric(),retail=numeric(),banco=numeric(),afiliacion=numeric(),cola_retail=numeric(),cola_banco=numeric(),cola_afiliacion=numeric()))



tienda_info_general <- data.frame(estado = character(),tienda = character(),total_de_clientes = numeric(0),dia_de_mayor_afluencia = numeric(0),
                                  dia_de_menor_afluencia = numeric(0), tasa_de_atencion = numeric(0))


#Codigo general para la obtencion de los tiempos promedio de atencion



#Recorrer cada estado
for (estado in estados){
  
  #Carga de la bd del estado actual
  estado_actual <- read.csv(file = estado)
  
  #Obtencion de la lista de tiendas presentes en dicho estado
  tiendas <- unique(estado_actual$tienda)

  #Recorrer cada tienda
  for (tienda in tiendas){
    
    #Filtrado de la bd segun la tienda
    tienda_actual <- estado_actual[estado_actual$tienda %in% tienda,]
    
    #Obtencion de los dias de los cuales se tienen informacion
    dias <- unique(tienda_actual$Fecha)
    
    #Creacion del data frame de la tienda con las medias de los dias
    
    
    
    
    
    #Vectores muestra para las distribuciones empiricas segun el dia de la semana
    dias_semana <- list(lunes=list(llegadas = numeric(0), retail = numeric(0),banco = numeric(0),afiliacion = numeric(0)),
                        martes=list(llegadas = numeric(0), retail = numeric(0),banco = numeric(0),afiliacion = numeric(0)),
                        miércoles=list(llegadas = numeric(0), retail = numeric(0),banco = numeric(0),afiliacion = numeric(0)),
                        jueves=list(llegadas = numeric(0), retail = numeric(0),banco = numeric(0),afiliacion = numeric(0)),
                        viernes=list(llegadas = numeric(0), retail = numeric(0),banco = numeric(0),afiliacion = numeric(0)),
                        sábado=list(llegadas = numeric(0), retail = numeric(0),banco = numeric(0),afiliacion = numeric(0)),
                        domingo=list(llegadas = numeric(0), retail = numeric(0),banco = numeric(0),afiliacion = numeric(0)))
    
    
    
    #Vector de pesos para las llegadas
    pesos <- table(tienda_actual$Segmento)
    #pesos(retail,banco,afiliacion)
    pesos <- c(pesos[[3]],pesos[[2]],pesos[[1]])/length(tienda_actual$Segmento)
    
    #Numero de servidores abiertos
    n_retail <- c()
    n_banco <- c()
    n_afiliacion <- c()
    
    #Recorrer los dias
    for (dia in dias) {
      #Obtener dia de la semana
      
      #Checar que el formato de los dias sea el correcto
      if(is.na(dmy(dia))){
        dia_semana <- weekdays(ymd(dia))
      }else{
        dia_semana <- weekdays(dmy(dia))
      }
      
      
      #Filtrado de la db de la tienda segun el dia
      tienda_dia <- tienda_actual[tienda_actual$Fecha %in% dia,]
      
      #Filtrado de la bd segun el segmento al que pertenecen, donde se atendio a alguien
      retail <- tienda_dia[tienda_dia$Segmento %in% "retail" & tienda_dia$status %in% "Atendido",]
      banco <- tienda_dia[tienda_dia$Segmento %in% "banco" & tienda_dia$status %in% "Atendido",]
      afiliacion <- tienda_dia[tienda_dia$Segmento %in% "afiliacion" & tienda_dia$status %in% "Atendido",]
      
      
      #Numero de servidores abiertos
      n_retail <- c(n_retail,length(grep("caja",unique(retail$caja))))
      n_banco <- c(n_banco,length(grep("ventanilla",unique(banco$caja))))
      n_afiliacion <- c(n_afiliacion,length(grep("p",unique(afiliacion$caja))))
      
      #Calculo de los tiempos de llegada
      tiempo_llegada <- tienda_dia$hora_llegada
      tiempo_llegada <- c(tiempo_llegada,0)-c(0,tiempo_llegada)
      
      #Se remueve el primer valor al no conocer exactamente el tiempo donde la tienda abre
      tiempo_llegada <- tiempo_llegada[-length(tiempo_llegada)]
      #Se comprueba que no haya ningun valor atipíco
      tiempo_llegada <- tiempo_llegada[tiempo_llegada>0]
      
      
      #Calculo de las muestras de atencion segun el segmento
      
      #Tiempo de atencion de retail, banco y afiliacion
      retail <- retail[retail$hora_salida > retail$hora_llamado,]
      retail <- retail$hora_salida-retail$hora_llamado
        
      banco <- banco[banco$hora_salida > banco$hora_llamado,]
      banco <- banco$hora_salida-banco$hora_llamado
        
      afiliacion <- afiliacion[afiliacion$hora_salida > afiliacion$hora_llamado,]
      afiliacion <- afiliacion$hora_salida-afiliacion$hora_llamado
      
      #Guardado de las muestras
      dias_semana[[dia_semana]]$llegadas <- c(dias_semana[[dia_semana]]$llegadas,tiempo_llegada)
      
      dias_semana[[dia_semana]]$retail <- c(dias_semana[[dia_semana]]$retail,retail)
      dias_semana[[dia_semana]]$banco <- c(dias_semana[[dia_semana]]$banco,banco)
      dias_semana[[dia_semana]]$afiliacion <- c(dias_semana[[dia_semana]]$afiliacion,afiliacion)
      
      
      
      
      
    }
    
    #Se comprueba el sesgo de los datos para ver que medida de tendencia central usar
    for (dia in c("lunes","martes","miércoles","jueves","viernes","sábado","domingo")){
      
      #LLegadas
      if(abs(skewness(dias_semana[[dia]]$llegadas))>1){
        #Muestras sesgadas
        tiempo_llegada <- median(dias_semana[[dia]]$llegadas)
      }else{
        #Muestras no sesgadas
        tiempo_llegada <- mean(dias_semana[[dia]]$llegadas)
      }
      
      #Retail
      if(abs(skewness(dias_semana[[dia]]$retail))>1){
        #Muestras sesgadas
        tiempo_retail <- median(dias_semana[[dia]]$retail)
      }else{
        #Muestras no sesgadas
        tiempo_retail <- mean(dias_semana[[dia]]$retail)
      }
      
      #Banco
      if(abs(skewness(dias_semana[[dia]]$banco))>1){
        #Muestras sesgadas
        tiempo_banco <- median(dias_semana[[dia]]$banco)
      }else{
        #Muestras no sesgadas
        tiempo_banco <- mean(dias_semana[[dia]]$banco)
      }
      
      #Afiliacion
      if(abs(skewness(dias_semana[[dia]]$afiliacion))>1){
        #Muestras sesgadas
        tiempo_afiliacion <- median(dias_semana[[dia]]$afiliacion)
      }else{
        #Muestras no sesgadas
        tiempo_afiliacion <- mean(dias_semana[[dia]]$afiliacion)
      }
    }
    
    #Graficas de barra del total de clientes
    
    #Dias
    dias <- c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo")
    #Total de clientes en toda la tienda segun el dia
    total <- c(length(dias_semana[["lunes"]]$llegadas),
               length(dias_semana[["martes"]]$llegadas),
               length(dias_semana[["miércoles"]]$llegadas),
               length(dias_semana[["jueves"]]$llegadas),
               length(dias_semana[["viernes"]]$llegadas),
               length(dias_semana[["sábado"]]$llegadas),
               length(dias_semana[["domingo"]]$llegadas))
    
    #Creacion del directorio
    if (!dir.exists("graficas")) {
      dir.create("graficas")
      dir.create("graficas/afluencia")
      dir.create("graficas/llegadas")
    }
    
    #Generacion y guardado de las graficas resultantes)
    
    png(paste("graficas/llegadas/",tienda,"_horario_llegadas.png",sep=""),width=800 , height = 600)
    hist(tienda_actual$hora_llegada,main="Horarios de llegada",col="skyblue",xlab="Horario",
         ylab="Cantidad",breaks=80)
    dev.off()
    
    png(paste("graficas/afluencia/",tienda,"_clientes_total.png",sep=""), width = 800, height = 600)
    barplot(total,names.arg = dias , main = "Clientes recibidos segun el dia",col = "skyblue",xlab = "Días de la Semana", ylab = "Cantidad", border = "black")
    dev.off()
    
    
    
    atencion <- tienda_actual[tienda_actual$status %in% c("Atendido"),]  
    atencion <- length(atencion$status)/length(tienda_actual$status)*100
    
    #Mayor afluencia
    temp <- substr(estado, 1, nchar(estado) - 4)
    tienda_info_general <- rbind(tienda_info_general,data.frame(estado=temp,tienda=tienda,total_de_clientes=sum(total),
                          dia_de_mayor_afluencia=afluencia(which.max(total)),dia_de_menor_afluencia=afluencia(which.min(total)),
                          tasa_de_atencion=atencion))
    
    
    
    ###Simulacion de colas
    
    #Cantidad de personas en la fila maxima permitida como para permitir una mejora
    tolerancia <- 5
    
    
    tamanos_promedio_colas <- list(lunes=numeric(0),martes=numeric(0),miércoles=numeric(0),jueves=numeric(0),viernes=numeric(0),sábado=numeric(0),domingo=numeric(0))
    
    dias <- c("lunes","martes","miércoles","jueves","viernes","sábado","domingo")
    
    #Promedio de cajas que se abrieron en la tienda
    n_retail <- ceiling(mean(n_retail))
    n_banco <- ceiling(mean(n_banco))
    n_afiliacion <- ceiling(mean(n_afiliacion))
    
    # Número de simulaciones (ej. 250, para mayor precisión pueden ser más)
    n_simulaciones <- 250  
    
    # Ejecutar múltiples simulaciones y promediar resultados
    resultados <- replicate(n_simulaciones, sim_colas(n_retail, n_banco, n_afiliacion, dias), simplify = FALSE)
    
    # Calcular promedio por día y tipo de cola (retail, banco, afiliación)
    tamanos_promedio_colas <- list(
      lunes     = colMeans(do.call(rbind, lapply(resultados, `[[`, "lunes"))),
      martes    = colMeans(do.call(rbind, lapply(resultados, `[[`, "martes"))),
      miércoles = colMeans(do.call(rbind, lapply(resultados, `[[`, "miércoles"))),
      jueves    = colMeans(do.call(rbind, lapply(resultados, `[[`, "jueves"))),
      viernes   = colMeans(do.call(rbind, lapply(resultados, `[[`, "viernes"))),
      sábado    = colMeans(do.call(rbind, lapply(resultados, `[[`, "sábado"))),
      domingo   = colMeans(do.call(rbind, lapply(resultados, `[[`, "domingo")))
    )
    
    #########
    
    nueva_n_retail <- n_retail
    nueva_n_banco <- n_banco
    nueva_n_afiliacion <- n_afiliacion
    
    
    nueva_sim <- tamanos_promedio_colas
    
    flag <- 1
    flag2 <- 1
    while(flag2==1){
  
      if(tolerancia<nueva_sim[["lunes"]][1] ||
         tolerancia<nueva_sim[["martes"]][1] ||
         tolerancia<nueva_sim[["miércoles"]][1] ||
         tolerancia<nueva_sim[["jueves"]][1] ||
         tolerancia<nueva_sim[["viernes"]][1] ||
         tolerancia<nueva_sim[["sábado"]][1] ||
         tolerancia<nueva_sim[["domingo"]][1]
         ){
        
        nueva_n_retail <- nueva_n_retail+1
        flag <- 0
        
        }else{
          if(tolerancia<nueva_sim[["lunes"]][2] ||
             tolerancia<nueva_sim[["martes"]][2] ||
             tolerancia<nueva_sim[["miércoles"]][2] ||
             tolerancia<nueva_sim[["jueves"]][2] ||
             tolerancia<nueva_sim[["viernes"]][2] ||
             tolerancia<nueva_sim[["sábado"]][2] ||
             tolerancia<nueva_sim[["domingo"]][2]
          ){
          
          nueva_n_banco <- nueva_n_banco+1
          flag <- 0
          
          }else{if(tolerancia<nueva_sim[["lunes"]][3] || 
                   tolerancia<nueva_sim[["martes"]][3] ||
                   tolerancia<nueva_sim[["miércoles"]][3] ||
                   tolerancia<nueva_sim[["jueves"]][3] ||
                   tolerancia<nueva_sim[["viernes"]][3] ||
                   tolerancia<nueva_sim[["sábado"]][3] ||
                   tolerancia<nueva_sim[["domingo"]][3]
          ){
          
          nueva_n_afiliacion <- nueva_n_afiliacion+1
          flag <- 0
        
          }else{
              flag2 <- 2
          }
          }
        }
      # Usamos el mismo número de simulaciones que antes (ej. 250, puden ser más para mayor presición)
      n_simulaciones <- 250  
      
      # Ejecutamos múltiples simulaciones para la configuración mejorada
      resultados_mejora <- replicate(
        n_simulaciones, 
        sim_colas(nueva_n_retail, nueva_n_banco, nueva_n_afiliacion, dias), 
        simplify = FALSE
      )
      
      # Calculamos el promedio de las colas para cada día
      nueva_sim <- list(
        lunes     = colMeans(do.call(rbind, lapply(resultados_mejora, `[[`, "lunes"))),
        martes    = colMeans(do.call(rbind, lapply(resultados_mejora, `[[`, "martes"))),
        miércoles = colMeans(do.call(rbind, lapply(resultados_mejora, `[[`, "miércoles"))),
        jueves    = colMeans(do.call(rbind, lapply(resultados_mejora, `[[`, "jueves"))),
        viernes   = colMeans(do.call(rbind, lapply(resultados_mejora, `[[`, "viernes"))),
        sábado    = colMeans(do.call(rbind, lapply(resultados_mejora, `[[`, "sábado"))),
        domingo   = colMeans(do.call(rbind, lapply(resultados_mejora, `[[`, "domingo")))
      ) 
    

     
    }
    
    
    
    if(flag==0){
    
    #Guardado de la informacion recopilada
    for (dia in c("lunes","martes","miércoles","jueves","viernes","sábado","domingo")){
      temp <- substr(estado, 1, nchar(estado) - 4)
      tienda_info[[dia]] <- rbind(tienda_info[[dia]],data.frame(Estado=temp,Tienda=tienda,Tiempo_llegada=tiempo_llegada,
                            Tiempo_retail=tiempo_retail,Tiempo_banco=tiempo_banco,Tiempo_afiliacion=tiempo_afiliacion,
                            
                            No._Servidores_retail=n_retail,
                            Cola_retail=tamanos_promedio_colas[[dia]][1],Tiempo_Cola_Retail=tamanos_promedio_colas[[dia]][1]*tiempo_retail,
                            No._Servidores_banco=n_banco,Cola_banco=tamanos_promedio_colas[[dia]][2],Tiempo_Cola_Banco=tamanos_promedio_colas[[dia]][2]*tiempo_banco,
                            No._Servidores_afiliacion=n_afiliacion,Cola_afiliacion=tamanos_promedio_colas[[dia]][3],Tiempo_Cola_Afiliacion=tamanos_promedio_colas[[dia]][3]*tiempo_afiliacion,
                            
                            Nuevo_No._Servidores_retail=nueva_n_retail,
                            Nuevo_Cola_retail=nueva_sim[[dia]][1],Tiempo_Cola_Retail=nueva_sim[[dia]][1]*tiempo_retail,
                            Nuevo_No._Servidores_banco=nueva_n_banco,Nuevo_Cola_banco=nueva_sim[[dia]][2],Nuevo_Tiempo_Cola_Banco=nueva_sim[[dia]][2]*tiempo_banco,
                            Nuevo_No._Servidores_afiliacion=nueva_n_afiliacion,Nuevo_Cola_afiliacion=nueva_sim[[dia]][3],Nuevo_Tiempo_Cola_Afiliacion=nueva_sim[[dia]][3]*tiempo_afiliacion))
    }
    }else{
    
    
    #Guardado de la informacion recopilada
    for (dia in c("lunes","martes","miércoles","jueves","viernes","sábado","domingo")){
      temp <- substr(estado, 1, nchar(estado) - 4)
      tienda_info[[dia]] <- rbind(tienda_info[[dia]],data.frame(Estado=temp,Tienda=tienda,Tiempo_llegada=tiempo_llegada,
                                                                Tiempo_retail=tiempo_retail,Tiempo_banco=tiempo_banco,Tiempo_afiliacion=tiempo_afiliacion,
                                                                
                                                                No._Servidores_retail=n_retail,
                                                                Cola_retail=tamanos_promedio_colas[[dia]][1],Tiempo_Cola_Retail=tamanos_promedio_colas[[dia]][1]*tiempo_retail,
                                                                No._Servidores_banco=n_banco,Cola_banco=tamanos_promedio_colas[[dia]][2],Tiempo_Cola_Banco=tamanos_promedio_colas[[dia]][2]*tiempo_banco,
                                                                No._Servidores_afiliacion=n_afiliacion,Cola_afiliacion=tamanos_promedio_colas[[dia]][3],Tiempo_Cola_Afiliacion=tamanos_promedio_colas[[dia]][3]*tiempo_afiliacion,
                                                                
                                                                Nuevo_No._Servidores_retail=NA,Nuevo_Cola_retail=NA,Tiempo_Cola_Retail=NA,
                                                                Nuevo_No._Servidores_banco=NA,Nuevo_Cola_banco=NA,Nuevo_Tiempo_Cola_Banco=NA,
                                                                Nuevo_No._Servidores_afiliacion=NA,Nuevo_Cola_afiliacion=NA,Nuevo_Tiempo_Cola_Afiliacion=NA))

    
    }
    
      }  
      
  }

}

#Guardado de la informacion en archivos separados segun el dia de la semana

write.csv(tienda_info_general,file="Informacion_general.csv")
for (dia in c("lunes","martes","miércoles","jueves","viernes","sábado","domingo")){
  
  write.csv(tienda_info[[dia]],file=paste("Info_tienda_",dia,".csv",sep = ""))
}

