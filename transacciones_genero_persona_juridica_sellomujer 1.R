rm(list=ls())

gc()

library(RODBC)
library(odbc)
library(zip)
library(dplyr)
library(lubridate)
library(AzureStor)
library(sqldf)
library(data.table)


con <- odbcConnect("dw", uid = "datawarehouse", pwd = "datawarehouse")
con1 <- odbcConnect("aq", uid = "datawarehouse", pwd = "datawarehouse")


#setwd("C:\\Users\\cl15002627k\\AirflowCodes\\R-Studio\\MEI_Mujer")



fecha <- Sys.Date()-months(1)
anio <- as.integer(format(fecha, "%Y"))
mes <- as.integer(format(fecha, "%m"))
fechaAct <- gsub("-","",Sys.Date())

if(mes <10){ 
  mesC <- paste0(0,mes)}else{mesC <- mes}



#Proveedores con sello mujer
# PSM <- sqlQuery(con1,"  select distinct s.entcode,-- replace(replace(RUT,'.',''),'-','') RUT,
#         '1' SelloMujer
#     from [DCCPMantenedor].[MSello].[SelloProveedor] s 
# 	  where entcode not in ('N/A') and
# 	  (([TipoSello]= 3 and persona =1) or  -- persona natural con sello mujer
# 		  ([TipoSello]= 3 and 
#       persona=2 and
#       year(FechaCaducidad) >= 2023 )) --Persona jurnidica con sello mujer vigente durante el anio de consulta ")

#Proveedores con sello mujer Solo RUT
PSM <- sqlQuery(con1,"  select distinct s.entcode,
 LOWER(replace(replace(RUT,'.',''),'-','')) RUT,
        '1' SelloMujer
    from [DCCPMantenedor].[MSello].[SelloProveedor] s 
	  where entcode not in ('N/A') and
	  (([TipoSello]= 3 and persona =1) or  -- persona natural con sello mujer
		  ([TipoSello]= 3 and 
      persona=2 and
      year(FechaCaducidad) >= 2023 )) --Persona jurnidica con sello mujer vigente durante el anio de consulta ")

#query
txt1 <-  paste0("select 
    t.year Anio
   ,t.month Mes
    ,p.entcode 
    ,LOWER(replace(replace(p.rutsucursal,'.',''),'-','')) RUTsucursal
    ,sum(montoUSD + isnull(impuestoUSD,0)) MontoTotalUSD
    ,sum(montoCLP + isnull(impuestoCLP,0)) MontoTotalCLP
    ,count(distinct oc.porid) CantOC
    
FROM DM_transaccional.dbo.THOrdenesCompra OC INNER JOIN
DM_transaccional.dbo.DimProveedor p on p.IDSucursal=oc.IDSucursal inner join
DM_transaccional.dbo.DimTiempo T ON OC.[IDFechaEnvioOC] = T.DateKey

  where t.year=", anio," --and t.month=",mes,"
                group by  t.year,t.month,p.entcode,p.rutsucursal")



#extraccion de datos  
montosMP <- sqlQuery(con, txt1)

#--------------------------------filtrar por PJ
montosMP_PJ <- montosMP[!(nchar(montosMP$RUTsucursal)>9 | nchar(montosMP$RUTsucursal)<9),]
montosMP_PJ$Rut_numero <- substr(montosMP_PJ$RUTsucursal,1,nchar(montosMP_PJ$RUTsucursal)-1)
montosMP_PJ$Rut_dv <- substr(montosMP_PJ$RUTsucursal,nchar(montosMP_PJ$RUTsucursal),nchar(montosMP_PJ$RUTsucursal))

#eliminar letras de la columna numero y dv (diferente a k)
montosMP_PJ <- montosMP_PJ[!montosMP_PJ$Rut_numero%like%"[a-z]",]
montosMP_PJ$Rut_numero <- as.integer(montosMP_PJ$Rut_numero)
montosMP_PJ <- montosMP_PJ[!is.na(montosMP_PJ$Rut_numero),]
montosMP_PJ <- montosMP_PJ[!(montosMP_PJ$Rut_dv!="k" & montosMP_PJ$Rut_dv%like%"[a-z]"),]

montosMP_PJ <- montosMP_PJ[montosMP_PJ$Rut_numero> 39999999,]
montosMP_PJ$TipoP <- "PJ"


#-----------------------------filtrar por PN
montosMP_PN <- montosMP[!(nchar(montosMP$RUTsucursal)>9 | nchar(montosMP$RUTsucursal)<8),]
montosMP_PN$Rut_numero <- substr(montosMP_PN$RUTsucursal,1,nchar(montosMP_PN$RUTsucursal)-1)
montosMP_PN$Rut_dv <- substr(montosMP_PN$RUTsucursal,nchar(montosMP_PN$RUTsucursal),nchar(montosMP_PN$RUTsucursal))

#eliminar letras de la columna numero y dv (diferente a k)
montosMP_PN <- montosMP_PN[!montosMP_PN$Rut_numero%like%"[a-z]",]
montosMP_PN$Rut_numero <- as.integer(montosMP_PN$Rut_numero)
montosMP_PN <- montosMP_PN[!is.na(montosMP_PN$Rut_numero),]
montosMP_PN <- montosMP_PN[!(montosMP_PN$Rut_dv!="k" & montosMP_PN$Rut_dv%like%"[a-z]"),]

montosMP_PN <- montosMP_PN[montosMP_PN$Rut_numero< 39999999 & montosMP_PN$Rut_numero>1000000,]
montosMP_PN$TipoP <- "PN"


#Unir proveedores PJ y PN y eliminar duplicados por entcode, dejando a las PJ por sobre las PN.
provP <- rbind(montosMP_PJ[c("entcode","TipoP")],montosMP_PN[c("entcode","TipoP")])
provP <- provP[!duplicated(provP$entcode),]



#-------------------------------------
# PSM$entcode <- as.integer(PSM$entcode)
# 
# montos1 <- left_join(montosMP,
#                      PSM,
#                      join_by(entcode==entcode) )
# 
# montos2 <- left_join(montos1,
#                      )
# class(PSM$entcode)


#union entre proveedores sello mujer con montos transados
montosPSM <- sqldf("select a.Anio,
                           a.Mes,
                           a.RUTsucursal,
                          -- a.entcode,
                         ---  case b.SelloMujer when '1' then 1 else 0 end SelloMujer,
                           b.SelloMujer,
                           p.TipoP Persona,
                           sum(a.MontoTotalUSD) MontoTotalUSD,
                            sum(a.MontoTotalCLP) MontoTotalCLP,
                             sum(a.CantOC) CantOC
                    from montosMP a left join
                          PSM b on a.RUTsucursal=b.RUT left join --a.entcode=b.entcode left join
                          provP p on p.entcode=a.entcode
                   group by a.Anio,
                           a.Mes,
                          a.RUTsucursal, --a.entcode,
                         --  case b.SelloMujer when '1' then 1 else 0 end ,
                           p.TipoP ")


#A todos los proveedores sin sello se asigna el valor 0 en el campo SelloMujer
montosPSM$SelloMujer[is.na(montosPSM$SelloMujer)] <- 0
#Proveedores sin RUT vnilido se asigna valor "SinInfo"
montosPSM$Persona[which(is.na(montosPSM$Persona))] <- 'SinInfo'


#adherir campo RUT. Se asigna un RUT por entcode
# montosPSM <- sqldf(" select a.*,
#                   case Persona 
#                          when 'PJ' then (select  b.RutSucursal
#                                           from montosMP_PJ b 
#                                           where a.entcode=b.entcode
#                                           Limit 1)
#                          when 'PN' then (select  c.RutSucursal
#                                           from montosMP_PN c 
#                                           where a.entcode=c.entcode
#                                           Limit 1)
#                          when 'SinInfo' then (select  d.RutSucursal
#                                           from montosMP d 
#                                           where a.entcode=d.entcode
#                                           Limit 1) 
#                       END RUT
#               from montosPSM a
#               ")


#enlistar RUTs y entcode y eliminar RUTs duplicados. Se prefiere el entcode que tenga sello mujer.
#Desde esta tabla se seleccionarnin los entcode que saldrnin en el reporte final

entcodeRUT <- select(montosMP,
                     entcode,
                     RUTsucursal)
entcodeRUT <- left_join(entcodeRUT,PSM[c("RUT","SelloMujer")], by= join_by("RUTsucursal"=="RUT"))

entcodeRUT <- arrange(entcodeRUT, desc(SelloMujer))

entcodeRUT <- entcodeRUT[!duplicated(entcodeRUT[c("RUTsucursal")]),]



montosPSM <- sqldf(" select a.*,
                  case Persona 
                         when 'PJ' then (select  b.entcode
                                          from entcodeRUT b 
                                          where a.RutSucursal=b.RutSucursal
                                          Limit 1)
                         when 'PN' then (select  c.entcode
                                          from entcodeRUT c 
                                          where a.RutSucursal=c.RutSucursal
                                          Limit 1)
                         when 'SinInfo' then (select  d.entcode
                                          from entcodeRUT d 
                                          where a.RutSucursal=d.RutSucursal
                                          Limit 1) 
                      END entcode
              from montosPSM a
              ")


#----------------------------------------
#Revisiones de montosPSM
#Revisar RUTs duplicados con distinto valor de sellomujer
# test <- montosPSM[!duplicated(montosPSM[c("RUTsucursal","SelloMujer")]),]
# test <- test[duplicated(test$RUTsucursal),]
# 
# #Revisar entcode duplicados con distinto valor de sellomujer
# test <- montosPSM[!duplicated(montosPSM[c("entcode","SelloMujer")]),]
# test <- test[duplicated(test$entcode),]

#----------------------------------------
#Para los casos con RUT diplucados en diferentes entcode, se procede a sumar los montos
#de ambos entcode, mantiene el RUT, y se queda el entcode que registra sellomujer
#casos detectados al 17/11/2023:
# entcode	SelloMujer	Persona	RUT
# 1235607	1	PN	89751829
# 219257	0	PN	89751829
# 1281866	1	PN	111155313
# 162743	0	PN	111155313
# 1496419	1	PJ	760149438
# 1593420	0	PJ	760149438
# 1246712	1	PJ	761115146
# 1246639	0	PJ	761115146
# 250345	1	PJ	762849208
# 258844	0	PJ	762849208
# 9207	1	PJ	766880398
# 1549443	0	PJ	766880398
# 1592794	1	PJ	769503609
# 273829	0	PJ	769503609
# 41476	1	PJ	965082107
# 1269579	0	PJ	965082107


#identificar los entcode con RUT duplicados
# RUTs_dupl<-  montosPSM$RUT[
#                 which(
#                   duplicated(
#                     montosPSM[c("RUT","Mes")]
#                     ))]
# 
# #Datos de montos por mes para RUTs duplicados
# montosPSM_RD <- montosPSM[montosPSM$RUT %in% RUTs_dupl,]
# 
# 
# #Datos de montos por mes para RUTs duplicados agrupados (suma de montos)
# montosPSM_RD_agrup <- montosPSM_RD %>%
#                         group_by(Anio,Mes,Persona,RUT)%>%
#                         summarise(MontoTotalUSD=sum(MontoTotalUSD),
#                                   MontoTotalCLP=sum(MontoTotalCLP),
#                                   CantOC=sum(CantOC))
# 
# 
# montosPSM_RD_agrup2 <- sqldf("
#                              select B.Anio,
#                                     B.Mes,
#                                     A.entcode,
#                                     A.SelloMujer,
#                                     B.Persona,
#                                     B.MontoTotalUSD,
#                                     B.MontoTotalCLP,
#                                     B.CantOC,
#                                     B.RUT
#                                     
#                              From montosPSM A inner join
#                                   montosPSM_RD_agrup B on A.RUT=B.RUT and A.Mes=B.Mes
#                                   
#                              ")
#   
#   
# montosPSM_RD_agrup2 <- arrange(montosPSM_RD_agrup2,
#                                Mes,
#                                RUT,
#                                desc(SelloMujer) ) 
#   
# 
# montosPSM_RD_agrup3 <- montosPSM_RD_agrup2[
#                                     !duplicated(
#                                       montosPSM_RD_agrup2[c("Mes","RUT")]),]
# 
# 
# 
# #eliminar registros de RUTs duplicados en la tabla de montos
# montosPSM2 <- montosPSM[!(montosPSM$RUT %in% RUTs_dupl),]
# 
# montosPSM3 <- rbind(montosPSM2,montosPSM_RD_agrup3)
# 
# 
# which(
#   duplicated(
#     montosPSM3[c("RUT","Mes")]))


#montosPSM <- montosPSM[!montosPSM$Mes==10,]
##########################################################
#Escribir y cargar datos
nombre <- paste0("transacciones_sello_mujer_",anio,mesC)
nombreCSV <- paste0(nombre,".csv")
nombreZIP <- paste0(nombre,".zip")

write.csv2(montosPSM,nombreCSV, row.names = FALSE)
##comprimir resultados
zipr(nombreZIP,nombreCSV)


##-----------Cargar datos a BLOB "indicadores-mensuales" de AZURE----------------------
#Token
bl_endp_key <- storage_endpoint("https://transparenciachc.blob.core.windows.net", key="YAnO9hVnk3w5Pi4JlwiBm7eLTbzRDAwzGjiazzavDmosdinXT3sMVnixw2uAVBG22/1WwPXUc1F3QUd63nmBgw==")
##nombre del contenedor
cont <- storage_container(bl_endp_key, "indicadores-mensuales")

##Cargar datos a BLOB "indicadores-mensuales" de AZURE
arch <- nombreZIP
storage_multiupload(cont, src=arch)



print(paste0("https://transparenciachc.blob.core.windows.net/indicadores-mensuales/",nombreZIP))

unlink(nombreZIP)
unlink(nombreCSV)






