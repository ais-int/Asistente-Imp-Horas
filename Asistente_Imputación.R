### ASISTENTE IMPUTACIÓN MENSUAL DE HORAS ###

library(dplyr)
library(lubridate)
#library(rvest)
#library(polite)
#library(wordcloud)
library(tidytext)
library(tm)


# PARA HACERLO PRÓXIMAMENTE......
###== SCRAPING FERIADOS 2024 ==##
#url_feriados_gob <- 'https://www.gob.cl/noticias/feriado-2024-chile-dias-festivos-irrenunciables/'
#
## Se comprueban los permisos para scrap.
#url_feriados_gob %>% bow() # se puede =)


# Vector con nombre de meses según posición
Meses <- c("Enero", "Febrero", "Marzo",
           "Abril", "Mayo", "Junio",
           "Julio", "Agosto", "Septiembre",
           "Octubre", "Noviembre", "Diciembre")

# Feriados Chile 2024
feriados <- c("2024-01-01", "2024-03-29", "2024-05-01", "2024-05-21",
              "2024-06-20", "2024-07-16", "2024-08-15", "2024-09-18",
              "2024-09-19", "2024-09-20", "2024-10-31", "2024-11-01",
              "2024-12-25") 

while (TRUE) {
  ## Preguntar el mes en que se desea imputar las horas ##
  while (TRUE) {
    month <- readline('¿En qué mes desea imputar sus horas trabajadas? (1-12): ')
    # verificar respuesta
    if (!is.na(as.numeric(month)) & (as.numeric(month) %in% c(1:12))) {
      month <- as.numeric(month)
      break
    } else {
      print('Respuesta inválida, por favor ingrese un valor entre 1 y 12.')
    }
  }
  # creamos una versión de month con 0
  if (month < 10) {
    month_0 <- paste0('0',as.character(month))
  } else {
    month_0 <- month
  }
  
  ## Preguntar año de imputación ##
  year <- '2024'
  while (TRUE) {
    resp <- readline(paste0('¿Desea imputar sus horas en el año ', year, ' ? (1=Sí, 0=No): '))
    # Verificar respuesta
    if (!is.na(as.numeric(resp)) & (as.numeric(resp) %in% c(0,1))) {
      if (as.numeric(resp) == 1) {
        break
      } else {
        year <- readline('Ingrese el año en el que desea imputar (ej: 2025): ')
        break
      }
      
    } else {
      print('Respuesta inválida, por favor ingrese el valor 0 o 1.')
    }
  }
  
  ## Crear Data frame vacío ## 
  df <- data.frame(matrix(ncol = 7, nrow = 0))
  names(df) <- c('dia', 'mes', 'año', 'numHoras', 'codProyecto', 'codTarea', 'codProducto')
  
  ## Filtros días del mes ##
  
  # 1ro Se obtienen todos los días del mes
  list_all_days <- 1:days_in_month(make_date(year, month))
  # 2do Se obtienen los días de la semana
  week_days <- list_all_days[wday(make_date(year, month, list_all_days)) %in% 2:6]
  #
  ## Preguntar si los feriados asignados a cada mes son correctos.
  feriados_mes <- feriados[substr(feriados,6,7) == as.character(month_0)]
  #
  if (length(feriados_mes) == 0) {
    while (TRUE) {
      resp <- readline(paste('No tengo registros de festividades en', Meses[as.numeric(month)], 'del', paste0(year,','),'desea agregar algun(os) día(s)? (1=Sí, 0=No): '))
      # Verificar respuesta
      if (!is.na(as.numeric(resp)) & as.numeric(resp) %in% c(0,1)) {
        if (as.numeric(resp) == 0) {
          break
        } else { # Preguntar días a excluir
          while (TRUE) {
            flag_exclude <- TRUE
            resp <- gsub(' ', '', readline('¿Qué días desea excluir de su imputación? (Si es más de un día separe por comas, ej: 1, 21): '))
            to_exclude <- unlist(strsplit(resp, ","))
            # Verificar respuesta
            if (sum(as.numeric(to_exclude) > as.numeric(days_in_month(make_date(year, month)))) == 0) {
              break
            } else {
              print(paste('El mes en el que estás imputando tiene', as.numeric(days_in_month(make_date(year, month))), 'días.'))
            }
          }
        }
      }
    }
  } else { # preguntar si son correctos los días feriados que disponemos
    while (TRUE) {
      resp <- readline(paste0('Se tiene registro de los siguientes días festivos: ', paste(as.character(as.numeric(substr(feriados_mes, 9, 11))), collapse = ', '),'. ¿Son correctos y no faltan? (1=Sí, 0=No): '))
      # Verificar respuesta
      if (!is.na(as.numeric(resp)) & as.numeric(resp) %in% c(0,1)) {
        if (as.numeric(resp) == 1) { # si son correctos, salir del bucle
          break
        } else { # Si no, preguntar en primer lugar si existen
          while (TRUE) {
            resp <- readline('¿Existen días festivos? (1=Sí, 0:No): ')
            # Verificar respuesta
            if (!is.na(as.numeric(resp)) & as.numeric(resp) %in% c(0,1)) {
              if (as.numeric(resp) == 0) {
                break
              } else {
                while (TRUE) {
                  flag_exclude <- TRUE
                  resp <- gsub(' ', '', readline('¿Qué días desea excluir de su imputación? (Si es más de un día separe por comas, ej: 1, 21): '))
                  to_exclude <- unlist(strsplit(resp, ","))
                  # Verificar respuesta
                  if (sum(as.numeric(to_exclude) > as.numeric(days_in_month(make_date(year, month)))) == 0) {
                    break
                  } else {
                    print(paste('El mes en el que estás imputando tiene', as.numeric(days_in_month(make_date(year, month))), 'días.'))
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  # Hacemos efectivo el filtro de días festivos, o que simplemente se excluyen
  if (flag_exclude == TRUE) {
    week_days_filt <- setdiff(week_days, as.numeric(to_exclude))
  } else {
    week_days_filt <- setdiff(week_days, as.numeric(substr(feriados_mes, 9, 11)))
  }
  ## Guardamos vectores diferenciados para guardar los días viernes y el resto
  fechas_mes <- seq.Date(as.Date(paste(as.numeric(year), as.numeric(month), "01", sep = "-")), 
                         as.Date(paste(as.numeric(year), as.numeric(month)+1, "01", sep = "-")) - 1, 
                         by = "day")
  
  viernes_mes <- fechas_mes[weekdays(fechas_mes) == "viernes"]
  
  # Días viernes
  imp_viernes <- as.numeric(substr(viernes_mes, 9, 11))
  #
  ## Imputación de horas ##
  hrs_to_imp <- length(imp_viernes)*5 + length(setdiff(week_days_filt, imp_viernes))*9.5
  print(paste("Horas a imputar:", as.character(hrs_to_imp)))
  
  contador = 0
  while(contador < hrs_to_imp) {
    print(paste("Horas imputadas:", as.character(contador)))
    ## Preguntar especificaciones del proyecto
    cod_Proy <- readline('Ingrese el código del proyecto: ')
    cod_Tarea <- readline('Ingrese código de la tarea: ')
    cod_Prod <- readline('Ingrese el código del producto: ')
    while (TRUE) {
      horas_Proy <- readline('Ingrese la cantidad de horas que usted trabajó en el mes en el proyecto: ')  
      # Verificar que el total de las horas no supere el total a imputar
      if (!is.na(as.numeric(horas_Proy)) & as.numeric(horas_Proy) <= (hrs_to_imp - contador)) {
        horas_Proy <- horas_Proy %>% as.numeric()
        break
      } else {
        print(paste0('Has imputado ', as.character(contador), ' horas de ', as.character(hrs_to_imp), '. Te quedan por completar ', as.character(hrs_to_imp - contador), '.'))
      }
    }
    #
    ## Imputar horas del proyecto ingresado
    if (contador == 0) {
      flag_pendiente <- FALSE
    }
    # Si luego de imputar un proyecto, quedan horas disponibles en un día, se
    # imputará en este bloque
    contador_proy = 0
    if (flag_pendiente == TRUE) {
      print("Inicio Pendiente")
      if (horas_Proy < hrs_pendientes) {
        df_imp <- data.frame(c(week_days_filt[1]), c(month), c(year), c(horas_Proy), c(cod_Proy), c(cod_Tarea), c(cod_Prod))
        names(df_imp) <- names(df)
        df <- rbind(df, df_imp)
        contador_proy <- horas_Proy
      } else {
        df_imp <- data.frame(c(week_days_filt[1]), c(month), c(year), c(hrs_pendientes), c(cod_Proy), c(cod_Tarea), c(cod_Prod))
        names(df_imp) <- names(df)
        df <- rbind(df, df_imp)
        contador_proy <- hrs_pendientes  
      }
      #contador <- contador + hrs_pendientes ###
      week_days_filt <- week_days_filt %>% tail(-1)
      flag_pendiente <- FALSE
      print("Fin pendiente")
    }
    
    # Imputador de horas del proyecto
    while (contador_proy < horas_Proy) { # iteramos hasta completar las horas del proyecto
      print(week_days_filt[1])
      while (length(week_days_filt) != 0) {
        if (week_days_filt[1] %in% imp_viernes) {
          # Si las horas que quedan por imputar superan las del día completo, se imputa todo el día
          if (horas_Proy - contador_proy >= 5) {
            print("Inicio Viernes completo")
            df_imp <- data.frame(c(week_days_filt[1]), c(month), c(year), c(5), c(cod_Proy), c(cod_Tarea), c(cod_Prod))
            names(df_imp) <- names(df)
            df <- rbind(df, df_imp) 
            contador_proy <- contador_proy + 5
            # eliminamos el día ya imputado
            week_days_filt <- week_days_filt %>% tail(-1)
            print("Fin Viernes completo")
            break
          } else {
            print("Inicio Viernes parcial")
            df_imp <- data.frame(c(week_days_filt[1]), c(month), c(year), c(horas_Proy - contador_proy), c(cod_Proy), c(cod_Tarea), c(cod_Prod))
            names(df_imp) <- names(df)
            df <- rbind(df, df_imp)
            hrs_pendientes <- 5 - (horas_Proy - contador_proy)
            #dia_pend <- week_days_filt[1]
            contador_proy <- horas_Proy
            flag_pendiente <- TRUE
            print("Fin Viernes parcial")
            break
          }
        } else { # Imputar días que no son viernes
          # Imputar el día completo si se puede
          if (horas_Proy - contador_proy >= 9.5) {
            print("Inicio LuJu completo")
            df_imp <- data.frame(c(week_days_filt[1]), c(month), c(year), c(9.5), c(cod_Proy), c(cod_Tarea), c(cod_Prod))
            names(df_imp) <- names(df)
            df <- rbind(df, df_imp) 
            contador_proy <- contador_proy + 9.5
            # eliminamos el día ya imputado
            week_days_filt <- week_days_filt %>% tail(-1)
            print("Fin LuJu completo")
            break
          } else {
            print("Inicio LuJu parcial")
            df_imp <- data.frame(c(week_days_filt[1]), c(month), c(year), c(horas_Proy - contador_proy), c(cod_Proy), c(cod_Tarea), c(cod_Prod))
            names(df_imp) <- names(df)
            df <- rbind(df, df_imp)
            hrs_pendientes <- 9.5 - (horas_Proy - contador_proy)
            #dia_pend <- week_days_filt[1]
            contador_proy <- horas_Proy
            flag_pendiente <- TRUE
            print("Fin LuJu parcial")
            break
          }
        }
      }
      
    }
    contador <- contador + horas_Proy
  }
  break
}

## Exportar csv
write.csv(df, "ruta_de_destino")

###########################################################################################################################################################
