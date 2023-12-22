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

packages = c("tidyverse" # Conjunto integral de paquetes para manipular y analizar datos de manera coherente y eficiente.
             , "RODBC" # Facilita la conexión y manipulación de bases de datos a través de ODBC (Open Database Connectivity).
             , "plotly" # Proporciona herramientas interactivas para la creación de gráficos dinámicos y visualizaciones interactivas.
             , "data.table" # Paquete optimizado para la manipulación eficiente de grandes conjuntos de datos, destacando por su velocidad y funcionalidades avanzadas.
             , "formattable" # Permite dar formato a tablas de datos de manera elegante, facilitando la presentación visual de la información.
             , "hutils" # Contiene funciones de utilidad diversas para simplificar tareas comunes en análisis de datos y programación.
             , "readr" # Ofrece funciones eficientes para la lectura de datos en R, facilitando la importación de diversos formatos de archivos.
             , "VennDiagram" # Permite la creación de diagramas de Venn para visualizar las intersecciones entre conjuntos de datos.
             , "RColorBrewer" # Proporciona paletas de colores predefinidas y de alta calidad para mejorar la estética de los gráficos en R.
             )
             

load_pkg(packages)


# #Establece conexiones a los diferentes servidores =======================================
# 
# #con = RODBC::odbcConnect("aquiles", uid = "datawarehouse", pwd = "datawarehouse") #TIVIT

con2 = RODBC::odbcConnect("aq", uid = "datawarehouse", pwd = "datawarehouse") #Aquiles

con3 = RODBC::odbcConnect("dw", uid = "datawarehouse", pwd = "datawarehouse") #Datawarehouse

#
# INSCRITOS EN LA PLATAFORMA ÚLTIMO AÑO ===============================================================
# 

data_path = "C:/o/OneDrive - DCCP/Escritorio/Proyectos/IPPG en Mercado Público/datos"


#Detalles sobre los archivos guardados en el directorio de trabajo 
#

detalles = function(path = wd_path, pattern = "*.rds"){
  require(dplyr)
  
  details = file.info(path = paste0(wd_path), list.files(pattern=pattern))
  
  details = details[with(details, order(as.POSIXct(mtime), decreasing = TRUE)), ] %>% 
    filter(isdir==FALSE)
  
  details$files = rownames(details)
  
  rownames(details) = NULL
  
  return(details)
}


consultar_y_guardar <- function(x,y, window = -11
                                ,wd_path = data_path
                                ,tipoConsulta = "ofertan"
                                ,depurar = TRUE) {
  
  if (length(years) == 0 || !tipoConsulta %in% c('ofertan'
                                                 , 'adjudican'
                                                 ,'login'
                                                 ,'inscritos'
                                                 ,'ofertan_inst'
                                                 ,'adjudican_inst')) {
    mensaje <- "Parámetros inválidos. Asegúrate de proporcionar años y/o tipo de consulta válidos."
    return(mensaje)
  }
  
  
  require(lubridate)
  
  # Es importante notar que la función switch es sensible al uso del operador de asignación
  # porque al usar <- en lugar de = arroja un error. OJO 
  
  ejecutarConsulta <- switch (tipoConsulta,
                  'ofertan' = function(x, y, window ) {
                    sqlQuery(con2, sprintf(
                                  "
                  
                  DECLARE @MONTH AS INT;
                  DECLARE @YEAR AS INT;
                  
                  SET @MONTH = %s;
                  SET @YEAR = %s;
                    
                  DECLARE @CURRENTMONTH datetime = datetimefromparts(@YEAR, @MONTH, 1,0,0,0,0);
                  DECLARE @startDate datetime = dateadd(month,%s, @currentMonth)
                  , @endDate datetime = dateadd(month, 1, @currentMonth);
                  
                  WITH TEMP as(
              
                  SELECT DISTINCT
                        UPPER(C.orgTaxID) [Rut Proveedor]
                        ,C.orgEnterprise [EntCode]
                        ,C.orgLegalName [Razon Social]
                        -- Oferta en licitaciones (o convenio Marco)' as [Tipo de participacion]
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
                        -- 'Entrega cotización para una consulta al mercado' as [Tipo de participacion]
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
                  --,'Entrega cotización para Compra ágil' as [Tipo de participacion]
                  FROM DCCPCotizacion.dbo.SolicitudCotizacion as SOLI
                  INNER JOIN [DCCPCotizacion].[dbo].[Cotizacion] as COTI ON SOLI.Id = COTI.SolicitudCotizacionId
                  INNER JOIN DCCPPlatform.dbo.gblOrganization B ON COTI.CodigoEmpresa collate Modern_Spanish_100_CI_AI =B.orgEnterprise
                  WHERE SOLI.FechaCierre BETWEEN @startDate AND @endDate
                	AND EstadoId = 2 -- enviada
                	)
                	
                	SELECT DISTINCT
                      LOWER(REPLACE(REPLACE(T.[Rut Proveedor],'.',''),'-','')) [Rut Proveedor]
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
                         ",x,y, window)
                                ) 
                              },
                  'adjudican' = function(x,y,window){
                    sqlQuery(con2, sprintf("
                  DECLARE @MONTH AS INT;
                  DECLARE @YEAR AS INT;
                  
                  SET @MONTH = %s;
                  SET @YEAR = %s;
                  
                  
                  DECLARE @CURRENTMONTH datetime = datetimefromparts(@YEAR, @MONTH, 1,0,0,0,0);
                  DECLARE @startDate datetime = dateadd(month,%s, @currentMonth)
                  , @endDate datetime = dateadd(month, 1, @currentMonth);
                  
                  /*Reciben una orden de compra*/
                    
                    SELECT DISTINCT 
                    LOWER(REPLACE(REPLACE(O.orgTaxID,'.',''),'-','')) [Rut Proveedor] 
                    , O.orgEnterprise [EntCode]
                    , C.entName [Razon Social]
                    ,(CASE WHEN S.TipoSello=3 THEN 'Mujeres' ELSE 'Hombres' END) [Sello Mujer]
                    , @MONTH [Mes Central]
                    , @YEAR [Anio Central]
                    , @startDate [Comienzo]
                    , @endDate [Final]
                  
                  FROM DCCPProcurement.dbo.prcPOHeader A with(nolock)
                  INNER JOIN DCCPPlatform.dbo.gblOrganization O with(nolock) ON A.porSellerOrganization = O.orgCode
                  INNER JOIN DCCPPlatform.dbo.gblEnterprise C with(nolock) ON O.orgEnterprise = C.entCode
                  LEFT JOIN (SELECT distinct s.EntCode, s.TipoSello
                            FROM [DCCPMantenedor].[MSello].[SelloProveedor] s
                            WHERE EntCode NOT IN ('N/A') AND
                            ((s.[TipoSello]= 3 and s.persona =1) or  -- persona natural con sello mujer
                            (s.[TipoSello]= 3 and s.persona=2 and year(s.FechaCaducidad) >= @YEAR) and
                            (year(s.fechacreacion)<= @YEAR))
                            ) s on C.EntCode collate Modern_Spanish_CI_AI =s.EntCode
                  WHERE (A.porBuyerStatus IN (4, 5, 6, 7, 12)) AND /* Estados que validan una OC*/
                    (A.porSendDate < @endDate) AND
                  (A.porSendDate >= @startDate)
                  ",x,y,window)
                    )
                    },
                  'inscritos' = function(x,y,window) {
                    sqlQuery(con2, sprintf(
                      "
                  DECLARE @MONTH AS INT;
                  DECLARE @YEAR AS INT;
                  
                  SET @MONTH = %s;
                  SET @YEAR = %s;
                  
                  
                  DECLARE @CURRENTMONTH datetime = datetimefromparts(@YEAR, @MONTH, 1,0,0,0,0);
                  DECLARE @startDate datetime = dateadd(month, %s, @currentMonth)
                  , @endDate datetime = dateadd(month, +1, @currentMonth);
                  
                  SELECT DISTINCT
                       LOWER(REPLACE(REPLACE([orgTaxID],'.',''),'-','')) [Rut Proveedor]
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
                          WHERE EntCode NOT IN ('N/A') AND
                          ((s.[TipoSello]= 3 and s.persona =1) or  -- persona natural con sello mujer
                          (s.[TipoSello]= 3 and s.persona=2 and year(s.FechaCaducidad) >= @YEAR) and
                          (year(s.fechacreacion)<= @YEAR))
                          ) s on O.orgEnterprise=s.EntCode collate Modern_Spanish_CI_AI
                  WHERE orgCreationDate  <= @endDate 
                      AND orgClass = 1 -- proveedores o proveedoras
                      AND orgIsActive = 1
                      AND orgIsTest = 0
                  GROUP BY O.orgTaxID,[orgEnterprise],UPPER([orgLegalName]), s.TipoSello
                  ",x,y, window)
        
                    )
                  },
                  'login' = function(x,y, window){
                    sqlQuery(con2, sprintf(
                      "
                DECLARE @MONTH AS INT;
                  DECLARE @YEAR AS INT;
                  
                  SET @MONTH = %s;
                  SET @YEAR = %s;
                  
                
                DECLARE @CURRENTMONTH datetime = datetimefromparts(@YEAR, @MONTH, 1,0,0,0,0);
                DECLARE @startDate datetime = dateadd(month,%s, @currentMonth)
                    , @endDate datetime = dateadd(month, +1, @currentMonth);

                SELECT
                 LOWER(REPLACE(REPLACE([orgTaxID],'.',''),'-','')) [Rut Proveedor]
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
                WHERE EntCode NOT IN ('N/A') AND
			        	((s.[TipoSello]= 3 and s.persona =1) or  -- persona natural con sello mujer
                (s.[TipoSello]= 3 and s.persona=2 and year(s.FechaCaducidad) >= @YEAR) and
                (year(s.fechacreacion)<= @YEAR))
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
			        	,x,y,window)
			        	)  
                  },
			        	"ofertan_inst" = function(x,y, window){sqlQuery(con2, sprintf(
			        	  "
              DECLARE @YEAR AS INT;
              DECLARE @MONTH AS INT;
                
              SET @MONTH = %s;
              SET @YEAR = %s;
                
              DECLARE @CURRENTMONTH datetime = datetimefromparts(@YEAR, @MONTH, 1,0,0,0,0);
              DECLARE @startDate datetime = dateadd(month, %s, @currentMonth)
              , @endDate datetime = dateadd(month, 1, @currentMonth);
              
              WITH TEMP as(
          
			        SELECT DISTINCT
              UPPER(C.orgTaxID) collate Modern_Spanish_CI_AI [Rut Proveedor] 
              ,C.orgEnterprise collate Modern_Spanish_CI_AI [EntCode]
              ,E.entname [Organismo]
              ,B.rbhOrganization collate Modern_Spanish_CI_AI [CodigoOrganismo]
              --,'Oferta en licitaciones (o convenio Marco)' collate Modern_Spanish_CI_AI [Tipo de participacion] 
              , COUNT(DISTINCT A.bidRFBCode) [Cantidad ofertas]
              FROM DCCPProcurement.dbo.prcBIDQuote A with(nolock) 
              INNER JOIN DCCPProcurement.dbo.prcRFBHeader B with(nolock) ON A.bidRFBCode = B.rbhCode
              INNER JOIN DCCPPlatform.dbo.gblOrganization C with(nolock) ON A.bidOrganization = C.orgCode
              INNER JOIN DCCPPlatform.dbo.gblOrganization D with(nolock) ON B.rbhOrganization = D.orgCode
              INNER JOIN DCCPPlatform.dbo.gblEnterprise E ON D.orgEnterprise=E.entCode
              WHERE (A.bidDocumentStatus IN (3, 4, 5)) AND
              (A.bidEconomicIssueDate < @endDate) AND
              (A.bidEconomicIssueDate >= @startDate)
              GROUP BY 
			        C.orgTaxID
              , C.orgEnterprise
              , E.entname
              , B.rbhOrganization
              
              UNION 
              
              SELECT DISTINCT 
              UPPER(A.proveedorRut) [Rut Proveedor]
              ,B.orgEnterprise [EntCode]
              ,E.entname [Organismo]
              --,B.orgLegalName [Razon Social]
              ,B.orgCode [CodigoOrganismo]
              --,'Entrega cotización para una consulta al mercado' as [Tipo de participacion]
              , COUNT(DISTINCT C.porID) [Cantidad ofertas]
              FROM DCCPProcurement.dbo.prcPOCotizacion A
              INNER JOIN DCCPPlatform.dbo.gblOrganization B ON A.proveedorRut=B.orgTaxID
              INNER JOIN DCCPProcurement.dbo.prcPOHeader C ON A.porId = C.porID
              INNER JOIN DCCPPlatform.dbo.gblOrganization D ON C.porBuyerOrganization = D.orgCode
              INNER JOIN DCCPPlatform.dbo.gblEnterprise E ON D.orgEnterprise=E.entCode
              WHERE (C.porSendDate < @endDate) AND
              (C.porSendDate >= @startDate)
              GROUP BY 
              A.proveedorRut
              ,B.orgEnterprise
			        , E.entname
              ,B.orgCode
              UNION
              
              SELECT DISTINCT
              UPPER(B.orgTaxID) [Rut Proveedor]
              ,COTI.CodigoEmpresa collate Modern_Spanish_CI_AI [EntCode]
              ,E.entname [Organismo]
              ,SOLI.CodigoOrganismo [CodigoOrganismo]
              --,'Entrega cotización para Compra ágil' [Tipo de participacion]
              ,COUNT(DISTINCT SOLI.Id) [Cantidad ofertas]
              FROM DCCPCotizacion.dbo.SolicitudCotizacion  SOLI
              INNER JOIN [DCCPCotizacion].[dbo].[Cotizacion] as COTI ON SOLI.Id = COTI.SolicitudCotizacionId
              INNER JOIN DCCPPlatform.dbo.gblOrganization B ON COTI.CodigoEmpresa collate Modern_Spanish_CI_AI =B.orgEnterprise collate Modern_Spanish_CI_AI
              INNER JOIN DCCPPlatform.dbo.gblOrganization D ON SOLI.CodigoOrganismo  = D.orgCode collate Modern_Spanish_CI_AI
              INNER JOIN DCCPPlatform.dbo.gblEnterprise E ON D.orgEnterprise=E.entCode 
              /* Unir e.entCode con gblEnterprise como O de organismo unirlo on T.entCode*/
              /* O.entname de la institución*/
              WHERE SOLI.FechaCierre <= @endDate AND EstadoId = 2 
            	GROUP BY
            	B.orgTaxID
				      ,COTI.CodigoEmpresa
            	,E.entname
            	,SOLI.CodigoOrganismo
            	)
            	
            	SELECT DISTINCT
            	    UPPER(T.Organismo) [Organismo]
                  ,LOWER(REPLACE(REPLACE(T.[Rut Proveedor],'.',''),'-','')) [Rut Proveedor]
                  ,T.EntCode
                  --,T.[Razon Social]
                  , (CASE s.TipoSello WHEN 3 THEN 'Mujeres' ELSE 'Hombres' END) [Sello Mujer]
                  ,T.[Cantidad ofertas]
                  , @MONTH [Mes Central]
                  , @YEAR [Anio Central]
                  , @startDate [Comienzo]
                  , @endDate [Final]
            	FROM TEMP T
            	LEFT JOIN (SELECT distinct s.EntCode, s.TipoSello
                FROM [DCCPMantenedor].[MSello].[SelloProveedor] s
                WHERE EntCode NOT IN ('N/A') AND
				        ((s.[TipoSello]= 3 and s.persona =1) or  -- persona natural con sello mujer
                (s.[TipoSello]= 3 and s.persona=2 and year(s.FechaCaducidad) >= @YEAR) and
                (year(s.fechacreacion)<= @YEAR))
                ) s on T.EntCode=s.EntCode
                       "
  				      ,x,y,window)
			        	)},
			        	"adjudican_inst" = function(x,y, window){
			        	  sqlQuery(con2, sprintf(
			        	    "
                DECLARE @MONTH AS INT;
                DECLARE @YEAR AS INT;
                
                SET @MONTH = %s;
                SET @YEAR = %s;
                
                
                DECLARE @CURRENTMONTH datetime = datetimefromparts(@YEAR, @MONTH, 1,0,0,0,0);
                DECLARE @startDate datetime = dateadd(month, %s, @currentMonth)
                , @endDate datetime = dateadd(month, 1, @currentMonth);
                
                /*Reciben una orden de compra*/
                  
                  SELECT DISTINCT 
                  UPPER(E.entName) [Organismo]  
                  , LOWER(REPLACE(REPLACE(O.orgTaxID,'.',''),'-','')) [Rut Proveedor]
                  , O.orgEnterprise [EntCode] -- Código Empresa
                  , D.orgEnterprise [OrgCode] -- Código Institución 
                  , C.entName [Razon Social]
                  ,(CASE  WHEN S.TipoSello=3 THEN 'Mujeres' ELSE 'Hombres' END) [Sello Mujer]
                  , COUNT(DISTINCT A.porID) [Cantidad OC]
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
                          WHERE EntCode NOT IN ('N/A') AND 
                          ((s.[TipoSello]= 3 and s.persona =1) or  -- persona natural con sello mujer
                          (s.[TipoSello]= 3 and s.persona=2 and year(s.FechaCaducidad) >= @YEAR) and
                          (year(s.fechacreacion)<= @YEAR))
                          ) s on C.EntCode collate Modern_Spanish_CI_AI =s.EntCode
                WHERE (A.porBuyerStatus IN (4, 5, 6, 7, 12)) AND /* Estados que validan una OC*/
                  (A.porSendDate < @endDate) AND
                (A.porSendDate >= @startDate)
                GROUP BY 
                E.entName
                ,O.orgTaxID
            	  ,O.orgEnterprise
                ,D.orgEnterprise
                ,C.entName
                ,S.TipoSello
          
          ", x, y, window)
          			        	  )
          			        	  
          			        	}
       	
                  )
  
  
  descargar_guardar <- function(x, y, window) {
    
    data <- ejecutarConsulta(x = x, y = y, window = window) 
    
    grupo1 = c('ofertan'
              , 'adjudican'
              ,'login'
              ,'inscritos')
    
    if (depurar){
      if (tipoConsulta%in%grupo1){
        data <- data %>% 
          mutate(sello = ifelse(`Sello Mujer`=="Mujeres",1,0)) %>% 
          arrange(desc(sello)) %>% 
          filter(!duplicated(EntCode)) %>% 
          select(-sello)
      } else {
        data <- data %>% 
          mutate(sello = ifelse(`Sello Mujer`=="Mujeres",1,0)
                 ,codigo = paste0(Organismo,EntCode)) %>% 
          group_by(Organismo) %>% 
          arrange(desc(sello)) %>% 
          filter(!duplicated(codigo)) %>% 
          select(-sello)
      }
    }
    
    
    return(data)
  }
  
  
  total <- data.table::data.table()
  
  for (year in y) {
    if (year == year(today())) {
      start <- Sys.time()
      x <- month(today()) - 1
      y <- year
      window<- -(x-1)
      
      data <- descargar_guardar(x, y, window)
      total <- rbind(total, data)
      
      end <- Sys.time()
      
      tiempo_transcurrido <- difftime(end, start, units = "mins")
      
      cat("Descarga para el año", year, "completada en", round(tiempo_transcurrido,1), "minutos.", "\n")
      
    } else {
      start <- Sys.time()
      y <- year
      
      data <- descargar_guardar(x, y, window)
      total <- rbind(total, data)
      
      end <- Sys.time()
      tiempo_transcurrido <- difftime(end, start, units = "mins")
      
      cat("Descarga para el año", year, "completada en", round(tiempo_transcurrido,1), "minutos.", "\n")
      }
  }
  
  # Guarda el objeto data en un archivo con nombre diferente según el tipo de consulta
  saveRDS(total, file = paste0(gsub("-", "", today()), gsub(" ", "_", tipoConsulta), " en algún procedimiento de compra ", y, ".rds"))
  
  return(total)
  
  }
  

details = detalles()

# CARGA DE DATOS ===============================================================

years <- c(2022, 2023)

ofertan_ <-  consultar_y_guardar(wd_path = data_path
                                 , x = 12
                                 , y = years
                                 ,tipoConsulta = "ofertan"
                                 , depurar = TRUE)

adjudican_ <-  consultar_y_guardar(wd_path = data_path
                                 , x = 12
                                 , y = years
                                 ,tipoConsulta = "adjudican"
                                 ,depurar = TRUE)

inscritos_ <-  consultar_y_guardar(wd_path = data_path
                                 , x = 12
                                 , y = years
                                 ,tipoConsulta = "inscritos"
                                 ,depurar = TRUE)


logueados_ <-  consultar_y_guardar(wd_path = data_path
                                   , x = 12
                                   , y = years
                                   ,tipoConsulta = "login"
                                   ,depurar = TRUE)


inscritos_2022 <- inscritos_ %>% 
  mutate(`Fecha de creación empresa` = as.Date(`Fecha de creación empresa`)) %>% 
  filter(`Fecha de creación empresa`>=as.Date("2022-01-01"))


#
# CÁLCULO DEL ÍNDICE DE CARÁCTER TEMPORAL ======================================
# 

# Esta parte del código la he comentado porque la utilizo para construir un panel
# sin embargo, para efectos de la pedida de las autoridades, se requiere sólo los
# totales anuales, por lo que el tema quedará pendiente. Probablemente desarrolle
# otra rama del proyecto más adelante. 

# comienzo = ofertan_ %>% 
#   group_by(Comienzo) %>% 
#   summarise(comienzos = n_distinct(Comienzo), 
#             `Mes Central` = `Mes Central`[1]
#             ,`Anio Central`=`Anio Central`[1]) %>% 
#   select(Comienzo, `Mes Central`, `Anio Central`)
# 
# final = ofertan_ %>% 
#   group_by(Final) %>% 
#   summarise(finales = n_distinct(Final)) %>% 
#   select(Final)
# 
# intervalo = comienzo %>% 
#   mutate(Final = as.Date(final$Final)) %>% data.frame()
# 
# 
# start <- Sys.time()
# 
# inscritos_ = lapply(1:nrow(intervalo), function(x){
#   inscritos_ %>%
#     filter(data.table::between(as.Date(`Fecha de creación empresa`),
#                                min(as.Date(`Fecha de creación empresa`)),
#                                as.Date(intervalo[x,4]))) %>% 
#     mutate(Final=intervalo[x,4],
#            `Mes Central` = intervalo[x,2]
#            ,`Anio Central` = intervalo[x,3])
# }) %>% data.table::rbindlist()
# end <- Sys.time()
# 
# difftime(end, start, units="mins")


data_index = inscritos_ %>% 
  left_join(ofertan_, by = c("EntCode", "Mes Central", "Anio Central", "Sello Mujer")) %>%
  left_join(adjudican_, by = c("EntCode", "Mes Central", "Anio Central", "Sello Mujer")) %>% 
  mutate(ofrece = ifelse(!is.na(`Rut Proveedor.y`), 1, 0), 
         gana = ifelse(!is.na(`Rut Proveedor`),1,0))

data_cohorte_2022 = inscritos_2022 %>% 
  left_join(ofertan_, by = c("EntCode", "Mes Central", "Anio Central", "Sello Mujer")) %>%
  left_join(adjudican_, by = c("EntCode", "Mes Central", "Anio Central", "Sello Mujer")) %>% 
  mutate(ofrece = ifelse(!is.na(`Rut Proveedor.y`), 1, 0), 
         gana = ifelse(!is.na(`Rut Proveedor`),1,0))

# Gráfico del Índice de Participación con Perspectiva de Género =========================  
(
indice =  data_index %>% 
    group_by(`Sello Mujer`, `Mes Central`, `Anio Central`) %>%
    summarise(participantes = n()
              ,oferentes = sum(ofrece)
              ,ganadores = sum(gana)) %>% 
    mutate(fecha = as.Date(paste(`Anio Central`
                               , `Mes Central`, "1", sep = "-"), format = "%Y-%m-%d")) %>% 
    setDT() %>% 
    dcast(formula = ...~`Sello Mujer`, value.var = c("participantes", "oferentes", "ganadores")) %>% 
    mutate(
      r_participa = (participantes_Mujeres/participantes_Hombres)
     ,r_oferta = (oferentes_Mujeres/oferentes_Hombres) 
     ,r_adjudica = (ganadores_Mujeres/ganadores_Hombres), 
     ) %>% 
    rowwise() %>% 
    mutate(indicador = sqrt(r_oferta*r_adjudica))
)

saveRDS(indice,
        file = paste0(gsub("-", "", today()),
                      gsub(" ","_"," datos indice agregado.rds")))

#indice = readRDS(file = "20231219_datos_indice_agregado.rds")


(
  indice_plot = ggplot(indice, aes(x = fecha)) +
    geom_line(aes(y = indicador, color = "General"), size = 1) +
   # geom_line(aes(y = r_participa, color = "Participación"), size = 1) +
    geom_line(aes(y = r_oferta, color = "Oferta"), size = 1) +
    geom_line(aes(y = r_adjudica, color = "Adjudicación"), size = 1) +
    geom_text(aes(x = fecha, y = indicador, label = round(indicador*100,1)))+
    #geom_text(aes(x = fecha, y = r_participa, label = round(r_participa*100,1)))+
    geom_text(aes(x = fecha, y = r_oferta, label = round(r_oferta*100,1)))+
    geom_text(aes(x = fecha, y = r_adjudica, label = round(r_adjudica*100,1)))+
    labs(title = "Índice de Participación con Perspectiva de Género para todo el sistema, 2022-2023",
         y = "Índice",
         x = "Fecha", 
         color = "Categoría") +
    theme_minimal()
)

# (
#   indice_plotly = ggplotly(indice_plot)
# )
# 
# htmlwidgets::saveWidget(indice_plotly,
#                         file = paste0(gsub("datos", "ippg_dccp", wd_path),"/indice_interactivo.html"))



# Gráfico del Índice de Participación con Perspectiva de Género =========================  
(
  indice_c_2022 =  data_cohorte_2022 %>% 
    group_by(`Sello Mujer`, `Mes Central`, `Anio Central`) %>%
    summarise(participantes = n()
              ,oferentes = sum(ofrece)
              ,ganadores = sum(gana)) %>% 
    mutate(fecha = as.Date(paste(`Anio Central`
                                 , `Mes Central`, "1", sep = "-"), format = "%Y-%m-%d")) %>% 
    setDT() %>% 
    dcast(formula = ...~`Sello Mujer`, value.var = c("participantes", "oferentes", "ganadores")) %>% 
    mutate(r_participa = (participantes_Mujeres/participantes_Hombres)
           ,r_oferta = (oferentes_Mujeres/oferentes_Hombres) 
           ,r_adjudica = (ganadores_Mujeres/ganadores_Hombres), 
    ) %>% 
    rowwise() %>% 
    mutate(indicador = ((r_participa^(1/3))*(r_oferta^(1/3))*(r_adjudica^(1/3))))
)

saveRDS(indice_c_2022,
        file = paste0(gsub("-", "", today()),
                      gsub(" ","_"," datos indice agregado_cohorte_2022.rds")))

indice_c_2022 = readRDS(file = "20231220_datos_indice_agregado_cohorte_2022.rds")


(
  indice_plot_c_2022 = ggplot(indice_c_2022, aes(x = fecha)) +
    geom_line(aes(y = indicador, color = "General"), size = 1) +
    geom_line(aes(y = r_participa, color = "Participación"), size = 1) +
    geom_line(aes(y = r_oferta, color = "Oferta"), size = 1) +
    geom_line(aes(y = r_adjudica, color = "Adjudicación"), size = 1) +
    geom_text(aes(x = fecha, y = indicador, label = round(indicador*100,1)))+
    geom_text(aes(x = fecha, y = r_participa, label = round(r_participa*100,1)))+
    geom_text(aes(x = fecha, y = r_oferta, label = round(r_oferta*100,1)))+
    geom_text(aes(x = fecha, y = r_adjudica, label = round(r_adjudica*100,1)))+
    labs(title = "Índice de Participación con Perspectiva de Género para todo el sistema, 2022-2023",
         y = "Índice",
         x = "Fecha", 
         color = "Categoría") +
    theme_minimal()
)

# OFERENTES DE CADA INSTITUCIÓN DEL ESTADO  ===============================================

years <- c(2022, 2023)

ofertan_inst <-  consultar_y_guardar(wd_path = data_path
                                 , x = 12
                                 , y = years
                                 ,tipoConsulta = "ofertan_inst", 
                                 ,depurar = TRUE)


# PROVEEDORES QUE RECIBEN ÓRDENES DE COMPRA DE CADA INSTITUCIÓN DEL ESTADO ===========================

years <- c(2022, 2023)

adjudican_inst <-  consultar_y_guardar(wd_path = data_path
                                   , x = 12
                                   , y = years
                                   ,tipoConsulta = "adjudican_inst")

# CÁLCULO DEL ÍNDICE POR INSTITUCIONES =========================================


details = detalles()
#
#

# data_index_inst = ofertan_inst_ %>% 
#   left_join(adjudican_inst_, by = c("Organismo", "Mes Central", "Anio Central")) %>% 
#   group_by(Organismo, `Mes Central`, `Anio Central`, `Sello Mujer`) %>%
#   mutate(ofrece = ifelse())
#   


# ofertan_instituciones = readr::read_rds(file = "20231215_institución_recibe_una_oferta.rds")
# 
# adjudican_instituciones = readr::read_rds(file = "20231215_institución_adjudica_una_oc.rds")


(
  of_inst = ofertan_inst %>% 
    group_by(`Mes Central`, `Anio Central`, `Organismo`, `Sello Mujer`) %>% 
    summarise(n = n()) %>% 
    setDT() %>% 
    data.table::dcast(formula = `Organismo`+`Anio Central`~`Sello Mujer`, value.var = c("n"))  %>% 
    mutate(r_ofer = (Mujeres/Hombres))
)


(
  adj_inst = adjudican_inst %>% 
    mutate(`Sello Mujer`=ifelse(`Sello Mujer`=="Mujeres", "Mujeres", "Hombres")) %>% 
    group_by(`Mes Central`, `Anio Central`, `Organismo`, `Sello Mujer`) %>% 
    summarise(n = n()) %>% 
    setDT() %>% 
    data.table::dcast(formula = `Organismo`+`Anio Central`~`Sello Mujer`, value.var = c("n"))  %>% 
    mutate(r_adj = (Mujeres/Hombres))
)

set.seed(123)

data_index_inst = 
  of_inst %>% 
  left_join(adj_inst, by = c("Organismo", "Anio Central")) %>% 
  mutate(indicador = sqrt(r_ofer*r_adj)) %>% 
  filter(!is.na(indicador)) 

medias <- aggregate(indicador ~ `Anio Central`, data = data_index_inst, mean)


set.seed(123)

numero_filas_vacias <- round(length(data_index_inst$Organismo)/2,0)

data_plot <- data_index_inst %>% 
  select(`Anio Central`, indicador)

for (i in 1:numero_filas_vacias) {
  data_plot <-  data_plot %>% 
    bind_rows(data.frame(indicador = rnorm(1,1,.04))) 
}

saveRDS(data_plot, file = paste0(wd_path, "/data_plot.rds"))

data_plot = readr::read_rds(file = paste0(wd_path,"/data_plot.rds"))

(
  ind_hist = ggplot(data_plot, aes(indicador,fill = as.factor(`Anio Central`))) +
    geom_density(alpha = .5) +
#    geom_density(color = "red", size = 1) +  # Agrega la curva de densidad
    # geom_vline(aes(xintercept = indicador, color = `Anio Central`),
    #            linetype = "dashed", size = 1, data = medias) +
    # geom_text(aes(x = indicador, label = round(indicador, 2)),
    #           vjust = -0.5, data = medias)+
    #geom_vline(aes(xintercept = mean(indicador, na.rm = TRUE)), linetype = "dashed", linewidth = 1) +
    labs(title = "Histograma del IPPG",
         x = "IPPG",
         y = "Frecuencia", 
         fill = 'Año')+
    theme_minimal()+
  guides(color = FALSE)+
    xlim(c(0,1.2))+
    scale_fill_manual(values = c('2022' = "red"
                                 ,'2023' = "blue")
                      , breaks = c('2022', '2023'), labels = c('2022','2023'))
)


data_inst <- data_index_inst %>%
  data.table::setDT() %>%
  data.table::dcast(formula = `Organismo`~`Anio Central`
                    , value.var = c("Hombres.x"
                                    , "Mujeres.x"
                                    , "r_ofer"
                                    , "Hombres.y"
                                    , "Mujeres.y"
                                    ,"r_adj"
                                    , "indicador")) %>%
  mutate(var = (indicador_2023-indicador_2022)/indicador_2022) %>%
  rename(`Organismo comprador` = `Organismo`
         ,`Oferta Hombres 2022` = Hombres.x_2022
         ,`Oferta Hombres 2023` = Hombres.x_2023
         ,`Oferta Mujeres 2022` = Mujeres.x_2022
         , `Oferta Mujeres 2023` = Mujeres.x_2023
         ,`Ratio Oferta 2022` = r_ofer_2022
         , `Ratio Oferta 2023` = r_ofer_2023
         , `Adjudican Hombres 2022` = Hombres.y_2022
         ,`Adjudican Hombres 2023`  = Hombres.y_2023
         ,`Adjudican Mujeres 2022` = Mujeres.y_2022
         ,`Adjudican Mujeres 2023` = Mujeres.y_2023
         ,`Ratio adjudicación 2022` = r_adj_2022
         ,`Ratio adjudicación 2023` = r_adj_2023
         ,`IPPG 2022` = indicador_2022
         ,`IPPG 2023` = indicador_2023) %>% 
  arrange(desc(`IPPG 2023`)) %>%
  mutate_at(vars(starts_with('Ratio') | starts_with('IPPG')), ~round(., 2))

write.csv(data_inst, file = "20231218_data_index_inst.csv")

# %>% 
#   data.table::setDT() %>% 
#   data.table::dcast(formula = `Organismo`~`Anio Central`
#                     , value.var = c("Hombres.x"
#                                     , "Mujeres.x"
#                                     , "r_ofer"
#                                     , "Hombres.y"
#                                     , "Mujeres.y"
#                                     ,"r_adj"
#                                     , "indicador")) %>%
#   mutate(var = (indicador_2023-indicador_2022)/indicador_2022) %>% 
#   rename(`Organismo comprador` = `Organismo`
#          ,`Oferta Hombres 2022` = Hombres.x_2022
#          ,`Oferta Hombres 2023` = Hombres.x_2023
#          ,`Oferta Mujeres 2022` = Mujeres.x_2022
#          , `Oferta Mujeres 2023` = Mujeres.x_2023
#          ,`Ratio Oferta 2022` = r_ofer_2022
#          , `Ratio Oferta 2023` = r_ofer_2023
#          , `Adjudican Hombres 2022` = Hombres.y_2022
#          ,`Adjudican Hombres 2023`  = Hombres.y_2023
#          ,`Adjudican Mujeres 2022` = Mujeres.y_2022
#          ,`Adjudican Mujeres 2023` = Mujeres.y_2023
#          ,`Ratio adjudicación 2022` = r_adj_2022
#          ,`Ratio adjudicación 2023` = r_adj_2023
#          ,`IPPG 2022` = indicador_2022
#          ,`IPPG 2023` = indicador_2023)


saveRDS(data_index_inst, file = "20231218_data_index_inst.rds")

# ANÁLISIS Y VISUALIZACIÓN DE LOS DATOS ==========================================

quitar_outliers <- function(data, variable, coeficiente = 1.5) {
  variable <- enquo(variable)  # Convertir el nombre de la variable en una expresión quosure
  
  q1 <- quantile(data %>% pull(!!variable), 0.25)
  q3 <- quantile(data %>% pull(!!variable), 0.75)
  iqr <- q3 - q1
  
  limite_inferior <- q1 - coeficiente * iqr
  limite_superior <- q3 + coeficiente * iqr
  
  data_filtrados <- data %>% filter((!!variable) >= limite_inferior, (!!variable) <= limite_superior)
  return(data_filtrados)
}

# Llamada a la función con la variable indicador
datos_ind_fil <- quitar_outliers(data_index_inst, indicador)



saveRDS(datos_ind_fil,
file = paste0(gsub("-", "", today()),
              gsub(" ","_"," datos índice desagregado por instituciones.rds")))

  # Proposición: no hay relación entre el número de OC y el índice ======
# Este gráfico queda para nuestro propio análisis

(
  ind_inst_plot = ggplot(datos_ind_fil, aes(x = total_oc, y = indicador, size = total_n)) +
    geom_point() +
    geom_vline(xintercept = mean(datos_ind_fil$total_oc), linetype = "dashed", color = "blue", size = 1) +
    geom_hline(yintercept = mean(datos_ind_fil$indicador), linetype = "dashed", color = "red", size = 1) +
    labs(title = "IPPG vs Cantidad de OC's",
         x = "Empresas ",
         y = "IPPG",
         size = "Tercera Variable") +
    theme_minimal()
)

cor.test(datos_ind_fil$total_oc , datos_ind_fil$indicador)

cor.test(datos_ind_fil$total_n,datos_ind_fil$indicador)



(
  ind_inst_plotly = ggplotly(ind_inst_plot)
)

htmlwidgets::saveWidget(ind_inst_plotly,
                        file = paste0(gsub("datos", "ippg_dccp", wd_path),"/indice_instituciones.html"))



# Proposición: Excluidos los Outliers los datos se comportan según una normal
# 

(
  ind_hist = ggplot(datos_ind_fil %>% 
                      arrange(indicador), aes(indicador, , y = ..density..)) +
    geom_histogram(binwidth = 0.01, fill = "blue", color = "black") +
    geom_density(color = "red", size = 1) +  # Agrega la curva de densidad
    geom_hline(yintercept = mean(datos_ind_fil$indicador), linetype = "dashed", color = "red", size = 1) +
    labs(title = "Histograma del IPPG",
         x = "IPPG",
         y = "Frecuencia")+
    theme_minimal()
  
)

ind_hist_plotly = ggplotly(ind_hist)


htmlwidgets::saveWidget(ind_hist_plotly,
                        file = paste0(gsub("datos", "ippg_dccp", wd_path),"/histogram_instituciones.html"))



# Resumir texto 
# Cambiar de tablas a gráficos
# Revisar tabla de montos y oc según procedimiento de compra
# Simplificar nomenclatura matemática
# Explicar el proceso de participación en el sistema 
# Cambiar el tipo de OC: Licitación, trato directo, trato directo,
#  convenio marco. Agrupar según un CASE
# Especificar la importancia de las instituciones no sólo
#  en términos de las cantidades 
#  Presentar Scatterplot, entre los montos y la proporción de esos montos 