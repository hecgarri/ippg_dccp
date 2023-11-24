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

#
# INSCRITOS EN LA PLATAFORMA ÚLTIMO AÑO ===============================================================
# 

inscritos = function(x,y) sqlQuery(con2,paste0(
  "
                    DECLARE @YEAR AS INT;
                    DECLARE @MONTH AS INT;
                    
                    SET @YEAR = ",y,";
                    SET @MONTH = ",x,";
                    
                    DECLARE @CURRENTMONTH datetime = datetimefromparts(@YEAR, @MONTH, 1,0,0,0,0);
                    DECLARE @startDate datetime = dateadd(month, -12, @currentMonth)
                    , @endDate datetime = dateadd(month, +1, @currentMonth);
                    
                    --with temp as (
                    SELECT DISTINCT
                        UPPER([orgTaxID]) as [Rut Proveedor]
                        ,[orgEnterprise] [EntCode]
                        ,UPPER([orgLegalName]) as [Razon Social]
                        , (CASE S.TipoSello WHEN 3 THEN 'Mujeres' ELSE 'Hombres' END) [Sello Mujer]
                        ,min(cast([orgCreationDate] as date)) [Fecha de creación empresa]
                        , @MONTH [Mes Central]
        				        , @YEAR [Anio Central]
        				        , @endDate [Final]
                    FROM [DCCPPlatform].[dbo].[gblOrganization] O
                    LEFT JOIN (SELECT distinct s.EntCode, s.TipoSello
                            FROM [DCCPMantenedor].[MSello].[SelloProveedor] s
                            WHERE (s.[TipoSello]= 3 and s.persona =1) or  -- persona natural con sello mujer
                            (s.[TipoSello]= 3 and s.persona=2 and year(s.FechaCaducidad) >= @YEAR) and
                            (year(s.fechacreacion)<= @YEAR)
                            ) s on O.orgEnterprise=s.EntCode collate Modern_Spanish_CI_AI
                    WHERE orgCreationDate  <= @endDate 
                        AND orgClass = 1 -- proveedores o proveedoras
                        AND orgIsActive = 1
                        AND orgIsTest = 0
                    GROUP BY UPPER([orgTaxID]),[orgEnterprise],UPPER([orgLegalName]), s.TipoSello
                    --)
                    
                    --SELECT 
                    
         ")
) 


# start <- Sys.time()
# 
# ins_2023 = lapply((month(today())-1), function(x) inscritos(x,year(today()))) %>%
#   data.table::rbindlist()
# 
# end <- Sys.time()
# difftime(end, start, units="mins")

# 
#saveRDS(ins_2023, file = paste0(gsub("-", "", today()),gsub(" ","_"," inscritos históricos en la plataforma.rds")))
# #



#
# LOGUEADOS EN LA PLATAFORMA ÚLTIMO AÑO ===============================================================
#


login = function(x,y, window = -24) sqlQuery(con2, paste0(
  "
                DECLARE @YEAR AS INT;
                DECLARE @MONTH AS INT;
                
                SET @YEAR = ",y,";
                SET @MONTH = ",x,";
                
                DECLARE @CURRENTMONTH datetime = datetimefromparts(@YEAR, @MONTH, 1,0,0,0,0);
                DECLARE @startDate datetime = dateadd(month,", window ,", @currentMonth)
                    , @endDate datetime = dateadd(month, +1, @currentMonth);

                SELECT
                UPPER([orgTaxID]) as [Rut Proveedor]
                ,O.orgEnterprise [EntCode]
                ,UPPER(E.entname) [Razon social]
                ,(CASE S.TipoSello WHEN 3 THEN 'Mujeres' ELSE 'Hombres' END) [Sello Mujer]
                ,COUNT(DISTINCT U.usrTaxID) [Usuarios]
                --, U.usrLastLogin
			        	, MONTH(U.usrLastLogin) [Mes]
                , YEAR(U.usrLastLogin) [Anio]
		            , @MONTH [Mes Central]
				        , @YEAR [Anio Central]
				        , @startDate [Comienzo]
				        , @endDate [Final]
                
                FROM  [DCCPPlatform].[dbo].[gblSecUserRole] as UR 
                INNER JOIN  [DCCPPlatform].[dbo].gblOrganization as O ON UR.uroOrganization      = O.orgCode
                INNER JOIN  [DCCPPlatform].[dbo].GblUser as U ON UR.uroUser              = U.usrCode
                LEFT JOIN   [DCCPPlatform].[dbo].gblEnterprise  as E  ON O.orgEnterprise         = E.entcode
                LEFT JOIN (SELECT distinct s.EntCode, s.TipoSello
                FROM [DCCPMantenedor].[MSello].[SelloProveedor] s
                WHERE (s.[TipoSello]= 3 and s.persona =1) or  -- persona natural con sello mujer
                (s.[TipoSello]= 3 and s.persona=2 and year(s.FechaCaducidad) >= @YEAR) and
                (year(s.fechacreacion)<= @YEAR)
                ) s on E.entcode=s.EntCode collate Modern_Spanish_CI_AI
                WHERE  U.usrIsActive       = 1
                AND O.orgIsActive   = 1
                AND E.entIsActive   = 1
                and o.orgistest = 0
                AND (U.usrEmail NOT LIKE '%ontraloria.cl' OR (U.usrEmail LIKE '%ontraloria.cl' AND E.entCode = '7231'))  -- 7231 Codigo de la Contraloria
                AND (U.usrEmail NOT LIKE '%ontroloria.cl' OR (U.usrEmail LIKE '%ontroloria.cl' AND E.entCode = '7231'))
                AND E.entName NOT IN ('MERCADOPUBLICOTEST','MPCOMPRADORTEST_SKY','MPCOMPRADORTEST_SKY2','DCCP-OPERACIONES-PRUEBA COMPRADOR') -- Usuarios de Prueba
                AND U.usrPosition != ''             -- No consideramos contactos sin usrPosition
                AND U.usrEmail  != ''        -- No consideramos contactos sin mail
                AND YEAR(U.usrLastLogin) = @YEAR
                AND U.usrLastLogin <= @endDate AND U.usrLastLogin >= @startDate
                AND O.orgClass = 1
                AND o.orgtaxid not in ('0-0','0.000.000-0','1-9','A.t21-125','yyyyyyyyyy')
                GROUP BY E.entcode
                , E.entName
                , S.TipoSello
                --, U.usrLastLogin
                , O.orgTaxId
                , O.OrgEnterprise
                , YEAR(U.usrLastLogin)
                , MONTH(U.usrLastLogin)   
               "
))


# start <- Sys.time()
# log_ = lapply((month(today())-1), function(x) login(x, year(today()))) %>%
#   data.table::rbindlist()
# end <- Sys.time()
# difftime(end, start, units="mins")
# 
# 
# saveRDS(log_, file = paste0(gsub("-", "", today()),gsub(" ","_"," logueados en la plataforma 2023.rds")))

# NOTA: AL VOLVER MÁS ANCHA LA VENTANA MÓVIL (ROLLING WINDOW, NO SE ACTULIZAN LOS DATOS
#                                             , SOSPECHO QUE LO BORRAN COMO HACEN CON EL
#                                             TEMA DE LAS SESIONES)

# 
# 
# 

ofertan = function(x,y, window = -12) sqlQuery(con2, paste0(
  "
              DECLARE @YEAR AS INT;
              DECLARE @MONTH AS INT;
                
              SET @YEAR = ",y,";
              SET @MONTH = ",x,";
                
              DECLARE @CURRENTMONTH datetime = datetimefromparts(@YEAR, @MONTH, 1,0,0,0,0);
              DECLARE @startDate datetime = dateadd(month,",window,", @currentMonth)
              , @endDate datetime = dateadd(month, 1, @currentMonth);
              
              WITH TEMP as(
          
              SELECT DISTINCT
                    UPPER(C.orgTaxID) [Rut Proveedor]
                    ,C.orgEnterprise [EntCode]
                    ,C.orgLegalName [Razon Social]
                    ,'Oferta en licitaciones (o convenio Marco)' as [Tipo de participacion]
              FROM DCCPProcurement.dbo.prcBIDQuote A with(nolock) 
              INNER JOIN DCCPProcurement.dbo.prcRFBHeader B with(nolock) ON A.bidRFBCode = B.rbhCode
              INNER JOIN DCCPPlatform.dbo.gblOrganization C with(nolock) ON A.bidOrganization = C.orgCode
              WHERE (A.bidDocumentStatus IN (3, 4, 5)) AND
                    (A.bidEconomicIssueDate <= @endDate) AND
                    (A.bidEconomicIssueDate >= @startDate) 
          
               UNION
              
              SELECT DISTINCT 
                    UPPER(A.proveedorRut) [Rut Proveedor]
                    ,B.orgEnterprise [EntCode]
                    ,B.orgLegalName [Razon Social]
                    ,'Entrega cotización para una consulta al mercado' as [Tipo de participacion]
              FROM DCCPProcurement.dbo.prcPOCotizacion A
              INNER JOIN DCCPPlatform.dbo.gblOrganization B ON A.proveedorRut=B.orgTaxID
              INNER JOIN DCCPProcurement.dbo.prcPOHeader C ON A.porId = C.porID
              WHERE (C.porSendDate <= @endDate) AND
              (C.porSendDate >= @startDate)
      
              UNION
          
              SELECT DISTINCT
              UPPER(B.orgTaxID) [Rut Proveedor]
              ,COTI.CodigoEmpresa collate Modern_Spanish_100_CI_AI [EntCode]
              ,B.orgLegalName [Razon Social]
              ,'Entrega cotización para Compra ágil' as [Tipo de participacion]
              FROM DCCPCotizacion.dbo.SolicitudCotizacion as SOLI
              INNER JOIN [DCCPCotizacion].[dbo].[Cotizacion] as COTI ON SOLI.Id = COTI.SolicitudCotizacionId
              INNER JOIN DCCPPlatform.dbo.gblOrganization B ON COTI.CodigoEmpresa collate Modern_Spanish_100_CI_AI =B.orgEnterprise
              WHERE SOLI.FechaCierre BETWEEN @startDate AND @endDate
            	AND EstadoId = 2 -- enviada
            	)
            	
            	SELECT DISTINCT
                  T.[Rut Proveedor]
                  ,T.EntCode
                  ,T.[Razon Social]
                  , (CASE s.TipoSello WHEN 3 THEN 'Mujeres' ELSE 'Hombres' END) [Sello Mujer]
                  , @MONTH [Mes Central]
                  , @YEAR [Anio Central]
                  , @startDate [Comienzo]
                  , @endDate [Final]
            	FROM TEMP T
            	LEFT JOIN (SELECT distinct s.EntCode, s.TipoSello
                FROM [DCCPMantenedor].[MSello].[SelloProveedor] s
                WHERE (s.[TipoSello]= 3 and s.persona =1) or  -- persona natural con sello mujer
                (s.[TipoSello]= 3 and s.persona=2 and year(s.FechaCaducidad) >= @YEAR) and
                (year(s.fechacreacion)<= @YEAR)
                ) s on T.EntCode=s.EntCode 
                     "
)
)


start <- Sys.time()
ofertan_2023 = lapply(1:((month(today()))-1), function(x) ofertan(x, year(today()))) %>%
  data.table::rbindlist()
ofertan_2022 = lapply(1:12, function(x) ofertan(x, 2022)) %>%
  data.table::rbindlist()
ofert = rbind(ofertan_2022
              ,ofertan_2023)
end <- Sys.time()
difftime(end, start, units="mins")

#
saveRDS(ofert, file = paste0(gsub("-", "", today()),gsub(" ","_"," ofertan en algún proceso de compra 2023.rds")))



# 
#


adjudican = function(x,y,window = -12) sqlQuery(con2, paste0(
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


# start <- Sys.time()
# adjudican_2023 = lapply(1:(month(today())-1), function(x) adjudican(x, year(today()))) %>%
#    data.table::rbindlist()
# adjudican_2022 = lapply(1:12, function(x) adjudican(x, 2022)) %>%
#   data.table::rbindlist()
# adjudican_ = rbind(adjudican_2022
#                    ,adjudican_2023)
# end <- Sys.time()
# difftime(end, start, units="mins")

# #
#saveRDS(adjudican_, file = paste0(gsub("-", "", today()),gsub(" ","_"," reciben una orden de compra 2023.rds")))
# #
#


#Detalles sobre los archivos guardados en el directorio de trabajo ======================
#
#detalles = function(){
  details = file.info(path = wd_path, list.files(pattern="*.rds"))
  
  details = details[with(details, order(as.POSIXct(mtime), decreasing = TRUE)), ] %>% 
    filter(isdir==FALSE)
  
  details$files = rownames(details)
  
  rownames(details) = NULL
#}

#detalles()

# Carga de datos históricos==============================

inscritos_ = readr::read_rds(details$files[grep("inscritos", details$files)][1])

#login_ = readr::read_rds(details$files[grep("logueados", details$files)][1])

ofertan_ = readr::read_rds(details$files[grep("ofertan", details$files)][1])

adjudican_ = readr::read_rds(file = details$files[grep("reciben", details$files)][1])

# CÁLCULO DEL ÍNDICE DE CARÁCTER TEMPORAL ======================================

data_index = login_ %>% 
  left_join(ofertan_, by = c("EntCode", "Mes Central", "Anio Central", "Sello Mujer")) %>%
  left_join(adjudican_ %>% 
              mutate(`Sello Mujer` = ifelse(`Sello Mujer`== 1, "Mujeres", "Hombres"))
            , by = c("EntCode", "Mes Central", "Anio Central", "Sello Mujer")) %>% 
  mutate(ofrece = ifelse(!is.na(`Rut Proveedor.y`), 1, 0), 
         gana = ifelse(!is.na(`Rut Proveedor`),1,0))
  

indice =  data_index %>% 
    group_by(`Sello Mujer`, `Mes Central`) %>%
    summarise(participantes = n()
              ,oferentes = sum(ofrece)
              ,ganadores = sum(gana)) %>% 
    setDT() %>% 
    dcast(formula = ...~`Sello Mujer`, value.var = c("participantes", "oferentes", "ganadores")) %>% 
    mutate(r_participa = (participantes_Mujeres/participantes_Hombres)*100
           ,r_oferta = (oferentes_Mujeres/oferentes_Hombres)*100 
           ,r_adjudica = (ganadores_Mujeres/ganadores_Hombres)*100, 
           ) %>% 
    rowwise() %>% 
    mutate(indicador = ((r_participa^(1/6))*(r_oferta^(2/6))*(r_adjudica^(3/6)))) %>% 
    ungroup() %>% 
    mutate(indice = (indicador/median(indicador))*100,
           var_ind = ((indice-lag(indice))/lag(indice))*100
           , indice_part = (r_participa/median(r_participa))*100
           ,indice_oferta= (r_oferta/median(r_oferta))*100
           ,indice_adjudica = (r_adjudica/median(r_adjudica))*100
           ,var_part = ((indice_part-lag(indice_part))/lag(indice_part))*100
           ,var_ofert = ((indice_oferta-lag(indice_oferta))/lag(indice_oferta))*100
           ,var_adjudica = ((indice_adjudica-lag(indice_adjudica))/lag(indice_adjudica))*100
)

ts.plot(indice$indice)

ofertan_inst = function(x,y) sqlQuery(con2, paste0(
  "
              DECLARE @YEAR AS INT;
              DECLARE @MONTH AS INT;
                
              SET @YEAR = ",y,";
              SET @MONTH = ",x,";
                
              DECLARE @CURRENTMONTH datetime = datetimefromparts(@YEAR, @MONTH, 1,0,0,0,0);
              DECLARE @startDate datetime = dateadd(month, -12, @currentMonth)
              , @endDate datetime = dateadd(month, 1, @currentMonth);
              
              WITH TEMP as(
          
              SELECT DISTINCT
              UPPER(C.orgTaxID) collate Modern_Spanish_CI_AI [Rut Proveedor] 
              ,C.orgEnterprise collate Modern_Spanish_CI_AI [EntCode]
              ,E.entname [Organismo]
              ,B.rbhOrganization collate Modern_Spanish_CI_AI [CodigoOrganismo]
              ,'Oferta en licitaciones (o convenio Marco)' collate Modern_Spanish_CI_AI [Tipo de participacion] 
              FROM DCCPProcurement.dbo.prcBIDQuote A with(nolock) 
              INNER JOIN DCCPProcurement.dbo.prcRFBHeader B with(nolock) ON A.bidRFBCode = B.rbhCode
              INNER JOIN DCCPPlatform.dbo.gblOrganization C with(nolock) ON A.bidOrganization = C.orgCode
              INNER JOIN DCCPPlatform.dbo.gblOrganization D with(nolock) ON B.rbhOrganization = D.orgCode
              INNER JOIN DCCPPlatform.dbo.gblEnterprise E ON D.orgEnterprise=E.entCode
              WHERE (A.bidDocumentStatus IN (3, 4, 5)) AND
              (A.bidEconomicIssueDate < @endDate) AND
              (A.bidEconomicIssueDate >= @startDate)
              
              UNION
              
              
              
              SELECT DISTINCT 
              UPPER(A.proveedorRut) [Rut Proveedor]
              ,B.orgEnterprise [EntCode]
              ,E.entname [Organismo]
              --,B.orgLegalName [Razon Social]
              ,B.orgCode [CodigoOrganismo]
              ,'Entrega cotización para una consulta al mercado' as [Tipo de participacion]
              FROM DCCPProcurement.dbo.prcPOCotizacion A
              INNER JOIN DCCPPlatform.dbo.gblOrganization B ON A.proveedorRut=B.orgTaxID
              INNER JOIN DCCPProcurement.dbo.prcPOHeader C ON A.porId = C.porID
              INNER JOIN DCCPPlatform.dbo.gblOrganization D ON C.porBuyerOrganization = D.orgCode
              INNER JOIN DCCPPlatform.dbo.gblEnterprise E ON D.orgEnterprise=E.entCode
              WHERE (C.porSendDate < @endDate) AND
              (C.porSendDate >= @startDate)

              UNION
              
              SELECT DISTINCT
              UPPER(B.orgTaxID) [Rut Proveedor]
              ,COTI.CodigoEmpresa collate Modern_Spanish_CI_AI [EntCode]
              ,E.entname [Organismo]
              ,SOLI.CodigoOrganismo [CodigoOrganismo]
              ,'Entrega cotización para Compra ágil' [Tipo de participacion]
              FROM DCCPCotizacion.dbo.SolicitudCotizacion  SOLI
              INNER JOIN [DCCPCotizacion].[dbo].[Cotizacion] as COTI ON SOLI.Id = COTI.SolicitudCotizacionId
              INNER JOIN DCCPPlatform.dbo.gblOrganization B ON COTI.CodigoEmpresa collate Modern_Spanish_CI_AI =B.orgEnterprise collate Modern_Spanish_CI_AI
              INNER JOIN DCCPPlatform.dbo.gblOrganization D ON SOLI.CodigoOrganismo  = D.orgCode collate Modern_Spanish_CI_AI
              INNER JOIN DCCPPlatform.dbo.gblEnterprise E ON D.orgEnterprise=E.entCode 
              /* Unir e.entCode con gblEnterprise como O de organismo unirlo on T.entCode*/
              /* O.entname de la institución*/
              WHERE SOLI.FechaCierre <= @endDate AND EstadoId = 2 
            	)
            	
            	SELECT DISTINCT
            	    UPPER(T.Organismo) [Organismo]
                  ,T.[Rut Proveedor]
                  ,T.EntCode
                  --,T.[Razon Social]
                  , (CASE s.TipoSello WHEN 3 THEN 'Mujeres' ELSE 'Hombres' END) [Sello Mujer]
                  , @MONTH [Mes Central]
                  , @YEAR [Anio Central]
                  , @startDate [Comienzo]
                  , @endDate [Final]
            	FROM TEMP T
            	LEFT JOIN (SELECT distinct s.EntCode, s.TipoSello
                FROM [DCCPMantenedor].[MSello].[SelloProveedor] s
                WHERE (s.[TipoSello]= 3 and s.persona =1) or  -- persona natural con sello mujer
                (s.[TipoSello]= 3 and s.persona=2 and year(s.FechaCaducidad) >= @YEAR) and
                (year(s.fechacreacion)<= @YEAR)
                ) s on T.EntCode=s.EntCode
			       -- INNER JOIN [DCCPPlatform].[dbo].[gblEnterprise] O ON  T.CodigoOrganismo = O.entCode
                     "
)
)

# start <- Sys.time()
# ofertan_inst_ = lapply(10, function(x) ofertan_inst(x,2023)) %>% 
#   data.table::rbindlist()
# end <- Sys.time()
# ofertan_inst_t = difftime(end, start, units="mins")
# 
# # # 
# saveRDS(ofertan_inst_, file = paste0(gsub("-", "", today()),gsub(" ","_"," institución recibe una oferta de compra 2023.rds")))
# # # 


adjudican_inst = function(x,y) sqlQuery(con2, paste0(
  "
      DECLARE @YEAR AS INT;
      DECLARE @MONTH AS INT;
      
      SET @YEAR = ",y,";
      SET @MONTH = ",x,";
      
      DECLARE @CURRENTMONTH datetime = datetimefromparts(@YEAR, @MONTH, 1,0,0,0,0);
      DECLARE @startDate datetime = dateadd(month, -12, @currentMonth)
      , @endDate datetime = dateadd(month, 1, @currentMonth);
      
      /*Reciben una orden de compra*/
        
        SELECT DISTINCT 
        UPPER(E.entName) [Organismo]  
        --,[Rut Proveedor]
        , O.orgEnterprise [EntCode]
        , C.entName [Razon Social]
        ,(CASE S.TipoSello WHEN 3 THEN 1 ELSE 0 END) [Sello Mujer]
        , @MONTH [Mes Central]
        , @YEAR [Anio Central]
        , @startDate [Comienzo]
        , @endDate [Final]
      
      FROM DCCPProcurement.dbo.prcPOHeader A with(nolock)
      INNER JOIN DCCPPlatform.dbo.gblOrganization O with(nolock) ON A.porSellerOrganization = O.orgCode
      INNER JOIN DCCPPlatform.dbo.gblEnterprise C with(nolock) ON O.orgEnterprise = C.entCode
      INNER JOIN DCCPPlatform.dbo.gblOrganization D with(nolock) ON A.porBuyerOrganization = D.orgCode
      INNER JOIN DCCPPlatform.dbo.gblEnterprise E ON D.orgEnterprise=E.entCode
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


# start <- Sys.time()
# adjudican_inst_ = lapply(10, function(x) adjudican_inst(x, 2023)) %>% 
#     data.table::rbindlist()
# end <- Sys.time()
# adjudican_inst_t = difftime(end, start, units="mins")
# 
# # # 
# saveRDS(adjudican_inst_, file = paste0(gsub("-", "", today()),gsub(" ","_"," institución recibe una orden de compra 2023.rds")))
# # # 
# # 

#detalles()
#
#

# data_index_inst = ofertan_inst_ %>% 
#   left_join(adjudican_inst_, by = c("Organismo", "Mes Central", "Anio Central")) %>% 
#   group_by(Organismo, `Mes Central`, `Anio Central`, `Sello Mujer`) %>%
#   mutate(ofrece = ifelse())
#   


ofertan_instituciones = readr::read_rds(file = "20231124_institución_recibe_una_oferta_de_compra_2023.rds")

adjudican_instituciones = readr::read_rds(file = "20231124_institución_recibe_una_orden_de_compra_2023.rds")



(
  of_inst = ofertan_instituciones %>% 
    group_by(`Mes Central`, `Anio Central`, `Organismo`, `Sello Mujer`) %>% 
    summarise(n = n()) %>% 
    setDT() %>% 
    data.table::dcast(formula = `Organismo`~`Sello Mujer`, value.var = "n")  %>% 
    mutate(r_ofer = (Mujeres/Hombres))
)


(
  adj_inst = adjudican_instituciones %>% 
    group_by(`Mes Central`, `Anio Central`, `Organismo`, `Sello Mujer`) %>% 
    mutate(`Sello Mujer`=ifelse(`Sello Mujer`==1, "Mujeres", "Hombres")) %>% 
    summarise(n = n()) %>% 
    setDT() %>% 
    data.table::dcast(formula = `Organismo`~`Sello Mujer`, value.var = "n")  %>% 
    mutate(r_adj = (Mujeres/Hombres))
)


data_index_inst = 
  of_inst %>% 
  left_join(adj_inst, by = c("Organismo")) %>% 
  mutate(indicador = sqrt(r_ofer*r_adj))