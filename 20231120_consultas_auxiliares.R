#Quita archivos del WorkSpace ===========================================================
#
rm(list = ls())

#Fija el directorio de trabajo ==========================================================
#

wd_path = "C:/o/OneDrive - DCCP/Escritorio/Proyectos/IPPG en Mercado Público/datos"

setwd(wd_path)


#Carga de paquetes necesarios para el análisis ==========================================
#
load_pkg <- function(pack){
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE)
  sapply(pack, require, character.only = TRUE)
}

packages = c("tidyverse" #Conjunto integral de paquetes para manipular y analizar datos de manera coherente y eficiente.
             , "RODBC" #facilita la conexión y manipulación de bases de datos a través de ODBC (Open Database Connectivity).
             , "plotly" #proporciona herramientas interactivas para la creación de gráficos dinámicos y visualizaciones interactivas
             , "data.table" #Paquete optimizado para manipulación eficiente de grandes conjuntos de datos, destacando por su velocidad y funcionalidades avanzadas.
             , "formattable"
             , "hutils"
             , "readr"
             , "VennDiagram"
             , "RColorBrewer")

load_pkg(packages)


# #Establece conexiones a los diferentes servidores =======================================
# 
# #con = RODBC::odbcConnect("aquiles", uid = "datawarehouse", pwd = "datawarehouse") #TIVIT

con2 = RODBC::odbcConnect("aq", uid = "datawarehouse", pwd = "datawarehouse") #Aquiles

con3 = RODBC::odbcConnect("dw", uid = "datawarehouse", pwd = "datawarehouse") #Datawarehouse



THOportunidadesNegocioCriterio = sqlQuery(con3, "
      SELECT * FROM [DM_Transaccional].[dbo].[THOportunidadesNegocioCriterio]
                                          ")

prcBIDStatus = sqlQuery(con2, 
                        "
                        SELECT * 
                        
                        FROM [DCCPProcurement].[dbo].[prcBIDStatus] 
                        ")


#Tabla con Status de las órdenes de compra 
q_3 = sqlQuery(con2, "SELECT * FROM [DCCPProcurement].[dbo].[prcPOBuyerStatus]")

#Tabla con Status de las licitaciones 
q_4 = sqlQuery(con2, "SELECT * FROM [DCCPProcurement].[dbo].[prcRFBStatus]")

#q_5 = sqlQuery(con2, "SELECT * FROM [DCCPProcurement].[dbo].[prcRFBHeader]")

q_6 = sqlQuery(con2, "SELECT * FROM [DCCPProcurement].[dbo].[prcRFBAwardType]")

q_7 = sqlQuery(con2, "SELECT * FROM [DCCPProcurement].[dbo].[prcRFBContractType]")

q_8 = sqlQuery(con3,"SELECT * FROM [DM_Transaccional].[dbo].[DimProducto]")

q_9 = sqlQuery(con3,"SELECT * FROM [DM_Transaccional].[dbo].[DimRubro]")



q_11 = sqlQuery(con3, "SELECT * FROM [DM_Transaccional].[dbo].[DimTipoProd]")




adjudican = function(x,y,window = -36) sqlQuery(con2, paste0(
  "
      DECLARE @YEAR AS INT;
      DECLARE @MONTH AS INT;
      
      SET @YEAR = ",y,";
      SET @MONTH = ",x,";
      
      DECLARE @CURRENTMONTH datetime = datetimefromparts(@YEAR, @MONTH, 1,0,0,0,0);
      DECLARE @startDate datetime = dateadd(month,",window,", @currentMonth)
      , @endDate datetime = dateadd(month, 1, @currentMonth);
      
      /*Reciben una orden de compra*/
        
        SELECT DISTINCT 
        UPPER(O.orgTaxID)  [Rut Proveedor]
        , O.orgEnterprise [EntCode]
        , C.entName [Razon Social]
        ,(CASE S.TipoSello WHEN 3 THEN 1 ELSE 0 END) [Sello Mujer]
        , @MONTH [Mes Central]
        , @YEAR [Anio Central]
        , @startDate [Comienzo]
        , @endDate [Final]
        , A.porSendDate [Fecha de emisión orden de compra]
      
      FROM DCCPProcurement.dbo.prcPOHeader A with(nolock)
      INNER JOIN DCCPPlatform.dbo.gblOrganization O with(nolock) ON A.porSellerOrganization = O.orgCode
      INNER JOIN DCCPPlatform.dbo.gblEnterprise C with(nolock) ON O.orgEnterprise = C.entCode
      LEFT JOIN (SELECT distinct s.EntCode, s.TipoSello
                FROM [DCCPMantenedor].[MSello].[SelloProveedor] s
                WHERE (s.[TipoSello]= 3 and s.persona =1) or  -- persona natural con sello mujer
                (s.[TipoSello]= 3 and s.persona=2 and year(s.FechaCaducidad) >= @YEAR) and
                (year(s.fechacreacion)<= @YEAR)
                ) s on C.EntCode collate Modern_Spanish_CI_AI =s.EntCode
      WHERE (A.porBuyerStatus IN (4, 5, 6, 7, 12)) AND /* Estados que validan una OC*/
        (A.porSendDate < @endDate) AND
      (A.porSendDate >= @startDate)

")
)

start <- Sys.time()
adjudican_ = lapply(((month(today()))-1), function(x) adjudican(x, year(today()))) %>%
   data.table::rbindlist()
end <- Sys.time()
difftime(end, start, units="mins")

#
saveRDS(adjudican_, file = paste0(gsub("-", "", today()),gsub(" ","_"," reciben una orden de compra 2023.rds")))
#




