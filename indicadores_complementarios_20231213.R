#Quita archivos del WorkSpace
rm(list = ls())

#Fija el directorio de trabajo 
setwd("C:/o/OneDrive - DCCP/Escritorio/Proyectos/IPPG en Mercado Público/ippg_dccp")

#Carga de paquetes necesarios para el análisis
load_pkg <- function(pack){
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE)
  sapply(pack, require, character.only = TRUE)
}

packages = c("tidyverse", "RODBC", "plotly", "data.table", "formattable")

load_pkg(packages)


# #Establece conexiones a los diferentes servidores 
# 
# #con = RODBC::odbcConnect("aquiles", uid = "datawarehouse", pwd = "datawarehouse") #TIVIT

con2 = RODBC::odbcConnect("aq", uid = "datawarehouse", pwd = "datawarehouse") #Aquiles

con3 = RODBC::odbcConnect("dw", uid = "datawarehouse", pwd = "datawarehouse") #Datawarehouse

# #Queries =====================
# 
# 
stats_oc_by_seal <- function(x,y, window = -12){
  sqlQuery(con3,sprintf("
              DECLARE @MONTH AS INT;          
              DECLARE @ANIO AS INT;
              
              SET @MONTH = %s;
              SET @ANIO = %s;
              
              DECLARE @CURRENTMONTH datetime = datetimefromparts(@ANIO, @MONTH,1,0,0,0,0);
              DECLARE @startDate datetime = dateadd(month,%s, @CURRENTMONTH)
                , @endDate datetime = dateadd(month,1,@currentMonth);

              SELECT T.year 'Año'
              ,(CASE sp.Id_Tipo_Sello WHEN 3 THEN 'Mujeres' ELSE 'Hombres' END)  [Sello Mujer]
              ,SUM(OC.MontoUSD+OC.ImpuestoUSD)/1000000 [Monto anual USD]
              ,COUNT(DISTINCT OC.porID) [Cantidad OC]
              FROM [DM_Transaccional].[dbo].[THOrdenesCompra] OC
              INNER JOIN DM_Transaccional.dbo.DimProveedor p on p.IDSucursal=oc.IDSucursal
              INNER JOIN [DM_Transaccional].[dbo].[DimTiempo] T ON T.DateKey=OC.IDFechaEnvioOC
              INNER JOIN [DM_Transaccional].[dbo].[DimComprador] C ON OC.IDUnidaddeCompra=C.IDUnidaddeCompra
              INNER JOIN [DM_Transaccional].[dbo].[DimInstitucion] I ON C.entCode=I.entCode
              LEFT JOIN (select distinct SP.EntCode, SP.Id_Tipo_Sello
                                       from [DM_RegistroProveedores].[dbo].[Sello_Proveedor] SP
                                       where (SP.[Id_Tipo_Sello]= 3) or  -- persona natural con sello mujer
                                      (SP.[Id_Tipo_Sello]= 3 and year(SP.Fecha_Caducidad) >= @ANIO) and
                                        (year(SP.Fecha_Creacion)<= @ANIO)
                                        ) SP on SP.EntCode = cast(p.entcode as nvarchar(50))
              WHERE T.year >= @ANIO
              GROUP BY T.year, sp.Id_Tipo_Sello
              ",x,y,window)
)
  
} 

q_1 = stats_oc_by_seal(x = 12, y = 2022)


saveRDS(q_1, file = "q1_monto_cantidad_oc_segun_sello_2018_2020.rds")



q_1 = readr::read_rds(file = "q1_monto_cantidad_oc_segun_sello_2018_2020.rds")


stats_oc_by_buyer = function(x,y, window = -12){
  sqlQuery(con3,sprintf("DECLARE @MONTH AS INT;          
              DECLARE @ANIO AS INT;
              
              SET @MONTH = %s;
              SET @ANIO = %s;
              
              DECLARE @CURRENTMONTH datetime = datetimefromparts(@ANIO, @MONTH,1,0,0,0,0);
              DECLARE @startDate datetime = dateadd(month,%s, @CURRENTMONTH)
                , @endDate datetime = dateadd(month,1,@currentMonth);

              SELECT T.year 'Anio',
              (CASE WHEN SP.Id_Tipo_Sello = 3 THEN 'Mujeres' ELSE 'Hombres' END) [Sello Mujer],
              S.Sector,
              I.NombreInstitucion,
              (SUM([MontoUSD]+[ImpuestoUSD])/1000000) 'Monto anual USD',
              COUNT(DISTINCT porID) 'Cantidad OC'
              FROM [DM_Transaccional].[dbo].[THOrdenesCompra] OC
              INNER JOIN DM_Transaccional.dbo.DimProveedor p on p.IDSucursal=oc.IDSucursal
              inner join [DM_Transaccional].[dbo].[DimTiempo] T on T.DateKey=OC.IDFechaEnvioOC
              inner join [DM_Transaccional].[dbo].[DimComprador] C ON OC.IDUnidaddeCompra=C.IDUnidaddeCompra
              inner join [DM_Transaccional].[dbo].[DimInstitucion] I ON C.entCode=I.entCode
              inner join [DM_Transaccional].[dbo].[DimSector] S ON I.IdSector=S.IdSector
              LEFT JOIN (select distinct SP.EntCode, SP.Id_Tipo_Sello
                     from  [DM_RegistroProveedores].[dbo].[Sello_Proveedor] SP
                                  where (SP.[Id_Tipo_Sello]= 3) or  -- persona natural con sello mujer
                                 (SP.[Id_Tipo_Sello]= 3 and year(SP.Fecha_Caducidad) >= @ANIO) and
                                   (year(SP.Fecha_Creacion)<= @ANIO)
                      ) sp on sp.EntCode = cast(p.entcode as nvarchar(50))
              Where T.Year >= @ANIO
              group by T.year, S.Sector, I.NombreInstitucion,  SP.Id_Tipo_Sello
              ORDER BY Anio ASC, 'Monto anual USD' DESC, NombreInstitucion"
  ,x,y,window)
  )
  }

q_2 = stats_oc_by_buyer(x = 12, y = 2022) %>% 
  setDT() %>% 
  data.table::dcast(formula = NombreInstitucion ~ `Sello Mujer`+Anio,
                    value.var = c("Monto anual USD", "Cantidad OC")) %>% 
  arrange(desc(`Monto anual USD_Mujeres`)) %>% 
  mutate(total_oc = `Cantidad OC_Hombres`+`Cantidad OC_Mujeres`,
         perc_muj=`Cantidad OC_Mujeres`/total_oc,
         total_usd = `Monto anual USD_Mujeres`+`Monto anual USD_Hombres`,
         perc_usd = `Monto anual USD_Mujeres`/total_usd)

saveRDS(q_2, file = "q2_monto_cantidad_oc_segun_sello_institucion.rds")


q_2 = readr::read_rds(file = "q2_monto_cantidad_oc_segun_sello_institucion.rds")


q_3 =  sqlQuery(con3,
                    "
    DECLARE @ANIO AS INT;
    SET @ANIO = 2023;

    SELECT 
    --T.year 'Anio',
    SUM(MontoOC)/1000000 [Monto anual USD],
    COUNT(DISTINCT porID) 'Cantidad OC',
    [Sello Mujer],
    [Procedimiento]
    FROM (
                SELECT 
                    T.year,
                    OC.MontoUSD + OC.ImpuestoUSD AS MontoOC,
                    porID,
                    (CASE 
                        WHEN Sp.[Id_Tipo_Sello] = 3 THEN 'Mujeres' 
                        ELSE 'Hombres' 
                    END) AS [Sello Mujer],
                    (CASE 
                        WHEN OC.porIsIntegrated = 3 THEN 'CA'
                        WHEN [IDProcedenciaOC] IN (701, 702) THEN 'LC' 
                        WHEN [IDProcedenciaOC] IS NULL AND OCExcepcional = 1 THEN 'LC'
                        WHEN [IDProcedenciaOC] IN (703) THEN 'CM'
                        ELSE 'TD' 
                    END) AS [Procedimiento]
                    FROM [DM_Transaccional].[dbo].[THOrdenesCompra] OC
                    INNER JOIN DM_Transaccional.dbo.DimProveedor p on p.IDSucursal=oc.IDSucursal
                    INNER JOIN [DM_Transaccional].[dbo].[DimTiempo] T on T.DateKey=OC.IDFechaEnvioOC
                    INNER JOIN [DM_Transaccional].[dbo].[DimComprador] C ON OC.IDUnidaddeCompra=C.IDUnidaddeCompra
                    INNER JOIN [DM_Transaccional].[dbo].[DimInstitucion] I ON C.entCode=I.entCode 
                    
                    LEFT JOIN (select distinct SP.EntCode, SP.Id_Tipo_Sello
                           from  [DM_RegistroProveedores].[dbo].[Sello_Proveedor] SP
                                        where (SP.[Id_Tipo_Sello]= 3) or  -- persona natural con sello mujer
                                       (SP.[Id_Tipo_Sello]= 3 and year(SP.Fecha_Caducidad) >= @ANIO) and
                                         (year(SP.Fecha_Creacion)<= @ANIO)
                            ) sp on sp.EntCode = cast(C.entcode as nvarchar(50))
                    Where T.Year = @ANIO
    ) AS Subquery
    GROUP BY  [Sello Mujer], [Procedimiento]
    ORDER BY  [Monto anual USD] DESC;
    "
                )



saveRDS(q_3, file = "q3_monto_cantidad_oc_segun_tipo_OC.rds")

#

q_3 = readr::read_rds(file = "q3_monto_cantidad_oc_segun_tipo_OC.rds")


q_4 = function(x,y, window){
  sqlQuery(con2, sprintf("
                             
   DECLARE @MONTH AS INT;          
              DECLARE @ANIO AS INT;
              
              SET @MONTH = %s;
              SET @ANIO = %s;
              
              DECLARE @CURRENTMONTH datetime = datetimefromparts(@ANIO, @MONTH,1,0,0,0,0);
              DECLARE @startDate datetime = dateadd(month,%s, @CURRENTMONTH)
                , @endDate datetime = dateadd(month,1,@currentMonth);
   
   
    DECLARE @ANIO as int
    DECLARE @FechaIni Datetime;
    DECLARE @FechaFin Datetime;
    /* Fecha en yyyy-mm-dd*/
    SET @FechaIni = CONVERT(DATETIME, '2022-01-01 00:00:00', 102);
    SET @FechaFin = CONVERT(DATETIME, '2022-12-31 00:00:00', 102);
    SET @ANIO = 2022;

   with temp as(
    SELECT DISTINCT cast(C.entCode as nvarchar(50)) AS Transan
    /* Selecciona la columna entcode desde la tabla C */
    FROM DCCPProcurement.dbo.prcPOHeader A with(nolock)
    /* with(nolock) previene que otros procesos hayan bloqueado la clave */
    /* prcPOHeader: Tabla que almacena la información de las cabeceras de las OC */
    INNER JOIN
    DCCPPlatform.dbo.gblOrganization B with(nolock) ON A.porSellerOrganization = B.orgCode
    /*gb*/
    INNER JOIN
    DCCPPlatform.dbo.gblEnterprise C with(nolock) ON B.orgEnterprise = C.entCode
    WHERE (A.porBuyerStatus IN (4, 5, 6, 7, 12)) AND /* Estados que validan una OC*/
    (A.porSendDate < @FechaFin) AND
    (A.porSendDate >= @FechaIni)

    UNION

    SELECT DISTINCT cast(C.orgEnterprise as nvarchar(50)) as Transan
    FROM DCCPProcurement.dbo.prcBIDQuote A with(nolock) /* Busca empresas que hayan ofertado*/
    INNER JOIN
    DCCPProcurement.dbo.prcRFBHeader B with(nolock) ON A.bidRFBCode = B.rbhCode
    /* Tabla de licitaciones*/
    INNER JOIN
    DCCPPlatform.dbo.gblOrganization C with(nolock) ON A.bidOrganization = C.orgCode
    WHERE (A.bidDocumentStatus IN (3, 4, 5)) AND
    (A.bidEconomicIssueDate < @FechaFin) AND
    (A.bidEconomicIssueDate >= @FechaIni)

    UNION

    SELECT DISTINCT cast(B.orgEnterprise as nvarchar(50)) AS Transan
    FROM DCCPProcurement.dbo.prcPOCotizacion A
    INNER JOIN DCCPPlatform.dbo.gblOrganization B ON
    A.proveedorRut=B.orgTaxID
    INNER JOIN
    DCCPProcurement.dbo.prcPOHeader C ON
    A.porId = C.porID
    WHERE (C.porSendDate < @FechaFin) AND
    (C.porSendDate >= @FechaIni)


    UNION
    /* MÁS LOS RUT NO REGISTRADOS EN MERCADO PÚBLICO Y QUE SON DEL PERÍODO DE REFERENCIA */

    SELECT DISTINCT  cast(A.proveedorRut as nvarchar(50)) as Transan
    FROM DCCPProcurement.dbo.prcPOCotizacion A
    INNER JOIN DCCPProcurement.dbo.prcPOHeader C ON
    A.porId = C.porID
    WHERE A.proveedorRut NOT IN (
      SELECT
      DISTINCT A.proveedorRut
      FROM [DCCPProcurement].[dbo].[prcPOCotizacion] A
      INNER  JOIN DCCPPlatform..gblOrganization B ON
      A.proveedorRut=B.orgTaxID) AND
    (C.porSendDate < @FechaFin) AND
    (C.porSendDate >= @FechaIni)
  )
  SELECT
  s.TipoSello
  , COUNT(DISTINCT t.Transan) [Proveedores]

  from temp t
  LEFT JOIN (SELECT DISTINCT
      s.entcode as Transan
      ,s.TipoSello
      FROM [DCCPMantenedor].[MSello].[SelloProveedor] s
      WHERE
      (s.[TipoSello] = 3 and s.persona = 1) or  -- persona natural
      (s.[TipoSello] = 3 and s.persona = 2
      and year(s.FechaCaducidad) >= @ANIO
      and year(s.fechacreacion) <= @ANIO)    -- persona juridica
      ) s on s.Transan collate Modern_Spanish_CI_AI = t.Transan
  GROUP BY s.TipoSello

                      "
))}

q_4 = q_4 %>%
  mutate(TipoSello = ifelse(!is.na(TipoSello),"Mujeres", "Hombres")) %>%
  ungroup() %>%
  mutate(total = sum(Proveedores),
         prop = (Proveedores/total)*100) %>%
  select(-total)

saveRDS(q_4, file = "sello_proveedores.rds")


q_4 = readr::read_rds(file = "sello_proveedores.rds")




# q_5 = sqlQuery(con2, "
#                 DECLARE @ANIO AS INT
#                 SET @ANIO = 2022
#                 
#                 ; with temp as(SELECT
#                   UPPER(E.entname) 'Institucion'
#                  ,COUNT(DISTINCT U.usrTaxID) [usuarios]
#                  , E.entcode
#                  , S.TipoSello
# 
#                 FROM        [DCCPPlatform].[dbo].[gblSecUserRole] as UR
#                 INNER JOIN  [DCCPPlatform].[dbo].gblOrganization as O      ON UR.uroOrganization      = O.orgCode
#                 INNER JOIN  [DCCPPlatform].[dbo].GblUser as U              ON UR.uroUser              = U.usrCode
#                 left join   [DCCPPlatform].[dbo].gblEnterprise  as E        ON O.orgEnterprise         = E.entcode
#                 LEFT JOIN (SELECT distinct s.EntCode, s.TipoSello
#                 from [DCCPMantenedor].[MSello].[SelloProveedor] s
#                 where (s.[TipoSello]= 3 and s.persona =1) or  -- persona natural con sello mujer
#                 (s.[TipoSello]= 3 and s.persona=2 and year(s.FechaCaducidad) >= 2022) and
#                 (year(s.fechacreacion)<= 2022)
#                 ) s on E.entcode=s.EntCode collate Modern_Spanish_CI_AI
#                 WHERE  U.usrIsActive       = 1
#                 AND O.orgIsActive   = 1
#                 AND E.entIsActive   = 1
#                 and o.orgistest = 0
#                 AND (U.usrEmail NOT LIKE '%ontraloria.cl' OR (U.usrEmail LIKE '%ontraloria.cl' AND E.entCode = '7231'))  -- 7231 Codigo de la Contraloria
#                 AND (U.usrEmail NOT LIKE '%ontroloria.cl' OR (U.usrEmail LIKE '%ontroloria.cl' AND E.entCode = '7231'))
#                 AND E.entName NOT IN ('MERCADOPUBLICOTEST','MPCOMPRADORTEST_SKY','MPCOMPRADORTEST_SKY2','DCCP-OPERACIONES-PRUEBA COMPRADOR') -- Usuarios de Prueba
#                 AND U.usrPosition != ''             -- No consideramos contactos sin usrPosition
#                 AND U.usrEmail  != ''        -- No consideramos contactos sin mail
# 
#                 and year(usrlastlogin)=@ANIO
#                 and O.orgClass = 1
#                 and o.orgtaxid not in ('0-0','0.000.000-0','1-9','A.t21-125','yyyyyyyyyy')
# 		            GROUP BY E.entcode,  E.entName, S.TipoSello)
# 		            
# 		            SELECT 
# 
# 		            t.TipoSello
# 		            , COUNT(DISTINCT t.entcode) [Proveedores]
# 		            ,(CASE t.TipoSello WHEN 3 THEN 'Mujeres' ELSE 'Hombres' END) 'Sello Mujer'
# 		            FROM temp t
# 		            GROUP By t.TipoSello
#                ")
# 
#  saveRDS(q_5, file = "entidades_logueadas_2022.rds")

#Tabla con el número de usuarios logueados en el 2022 junto al sello Mujer
q_5 = readr::read_rds(file = "entidades_logueadas_2022.rds")

# q_5.1 = q_5 %>% 
#   mutate()


query_part = function(x,y) sqlQuery(con2, paste0(
  "
                DECLARE @YEAR AS INT;
                DECLARE @MONTH AS INT;
                
                SET @YEAR = ",y,";
                SET @MONTH = ",x,";
                
                DECLARE @CURRENTMONTH datetime = datetimefromparts(@YEAR, @MONTH, 1,0,0,0,0);
                DECLARE @startDate datetime = dateadd(month, -2, @currentMonth)
                , @endDate datetime = dateadd(month, 1, @currentMonth)

                
                
                
                ; with temp as(
                SELECT
                UPPER(E.entname) [Proveedor]
                ,COUNT(DISTINCT U.usrTaxID) [Usuarios]
                , E.entcode [id]
                , (CASE S.TipoSello WHEN 3 THEN 'Mujeres' ELSE 'Hombres' END) [Sello Mujer]
                , U.usrLastLogin
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
                --AND YEAR(U.usrLastLogin) >= @YEAR
                AND U.usrLastLogin <= @endDate AND U.usrLastLogin >= @startDate
                AND O.orgClass = 1
                AND o.orgtaxid not in ('0-0','0.000.000-0','1-9','A.t21-125','yyyyyyyyyy')
                GROUP BY E.entcode,  E.entName, S.TipoSello, U.usrLastLogin, YEAR(U.usrLastLogin), MONTH(U.usrLastLogin)   
   
                )
		            
		            SELECT 
                t.[Anio Central]
                ,t.[Mes Central]
		            ,t.[Sello Mujer]
		            , COUNT(DISTINCT t.id) [Proveedores]
		            
		            FROM temp t
		            GROUP By [Sello Mujer], t.[Mes Central], t.[Anio Central] 
               "
))

q_6.1 = lapply(1:12, function(x) query_part(x,2022)) %>%
  data.table::rbindlist()

q_6.2 = lapply(1:12, function(x) query_part(x,2023)) %>%
  data.table::rbindlist()

q_6 = rbind(q_6.1, q_6.2)

saveRDS(q_6, "participacion_plataforma.rds")

q_6 = readr::read_rds(file = "participacion_plataforma.rds") %>% 
  data.table::dcast(formula = `Mes Central`+`Anio Central`~`Sello Mujer`,
                    value.var = c("Proveedores")) %>% 
  mutate(prop = Mujeres/(Hombres), 
         date = as.Date(paste0(`Anio Central`,"-", `Mes Central`, "-",1))) %>% 
  arrange(`Mes Central`) %>% 
  arrange(`Anio Central`)
  
(p <- ggplot(q_6, aes(x = date, y = prop)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Razón entre la actividad de mujeres y hombres en Mercado Público",
       x = "Fecha",
       y = "Valor") +
  theme_minimal())  

ggplotly(p)

q_6.1 = lapply(9, function(x) sqlQuery(con2, paste0( 
                "
                DECLARE @ANIO AS INT
                DECLARE @MONTH AS INT
                
                SET @ANIO = 2022
                SET @MONTH = ",x,"
                
                SELECT
                UPPER(E.entname) [Institucion]
                --COUNT(DISTINCT U.usrTaxID) [Usuarios]
                 ,E.entcode [id]
                , S.TipoSello [Sello]
                , year(GS.sesEndDateTime) [Anio]
                , month(GS.sesEndDateTime) [Mes]
                , GS.sesEndDateTime
                , U.usrFirstName
                , U.usrLastName
                
                FROM  [DCCPPlatform].[dbo].gblSession as GS      
                LEFT JOIN [DCCPPlatform].[dbo].[gblSecUserRole] as UR ON GS.sesUser = UR.uroUser
                INNER JOIN  [DCCPPlatform].[dbo].gblOrganization as O ON UR.uroOrganization      = O.orgCode
                INNER JOIN  [DCCPPlatform].[dbo].GblUser as U ON UR.uroUser              = U.usrCode
                LEFT JOIN   [DCCPPlatform].[dbo].gblEnterprise  as E  ON O.orgEnterprise         = E.entcode
                LEFT JOIN (SELECT distinct s.EntCode, s.TipoSello
                FROM [DCCPMantenedor].[MSello].[SelloProveedor] s
                WHERE (s.[TipoSello]= 3 and s.persona =1) or  -- persona natural con sello mujer
                (s.[TipoSello]= 3 and s.persona=2 and year(s.FechaCaducidad) >= @ANIO) and
                (year(s.fechacreacion)<= @ANIO)
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
                
                AND year(GS.sesEndDateTime)>=@ANIO
                AND month(GS.sesEndDateTime) = @MONTH
                AND O.orgClass = 1
                AND o.orgtaxid not in ('0-0','0.000.000-0','1-9','A.t21-125','yyyyyyyyyy')
                --GROUP BY U.usrTaxID, year(GS.sesEndDateTime), month(GS.sesEndDateTime)
                --GROUP BY  S.TipoSello, year(GS.sesEndDateTime), month(GS.sesEndDateTime) -- E.entCode,  E.entName,
                --ORDER BY E.entCode
                 "
                )
                )
) 


#Tabla con Status de las órdenes de compra 
q_3 = sqlQuery(con2, "SELECT * FROM [DCCPProcurement].[dbo].[prcPOBuyerStatus]")

#Tabla con Status de las licitaciones 
q_4 = sqlQuery(con2, "SELECT * FROM [DCCPProcurement].[dbo].[prcRFBStatus]")

#q_5 = sqlQuery(con2, "SELECT * FROM [DCCPProcurement].[dbo].[prcRFBHeader]")

q_6 = sqlQuery(con2, "SELECT * FROM [DCCPProcurement].[dbo].[prcRFBAwardType]")

q_7 = sqlQuery(con2, "SELECT * FROM [DCCPProcurement].[dbo].[prcRFBContractType]")

q_8 = sqlQuery(con3,"SELECT * FROM [DM_Transaccional].[dbo].[DimProducto]")

q_9 = sqlQuery(con3,"SELECT * FROM [DM_Transaccional].[dbo].[DimRubro]")

q_10 = sqlQuery(con3, "SELECT * FROM [DM_Transaccional].[dbo].[DimTipoOC]")

q_11 = sqlQuery(con3, "SELECT * FROM [DM_Transaccional].[dbo].[DimTipoProd]")


# q_8 = sqlQuery(con3, "
#                	SELECT    year(Oneg.FechaPublicacion) as año_publicacion
#               		    , month(Oneg.FechaPublicacion) as mes_publicacion	
#               		    , TOC.SufijoTipoON as id_tamaño
#               			, TOC.TipoON as tamaño_licitacion
#               		    , Oneg.rbhCode 
#               		    , COUNT(O.bidID) as ofertas -- utilizar en su lugar el entcode u otra llave
#               	FROM	DM_Procesos.dbo.THOportunidadesNegocio Oneg 
#               			inner join DM_Procesos.dbo.THOfertas O	on Oneg.rbhCode = O.rbhCode 
#               			inner join DM_Transaccional.dbo.DimTipoON TOC on TOC.IDTipoON = Oneg.IDTipoON						
#               	WHERE	O.IdEstadoOferta in (3,4,5)	
#               			and year(oneg.FechaPublicacion) >= 2022
#               			and Oneg.IDEstadoON	 >= 6 
#               	GROUP BY 
#               			year(Oneg.FechaPublicacion),
#               			month(Oneg.FechaPublicacion),
#               			TOC.SufijoTipoON,
#               			TOC.TipoON,
#               			Oneg.rbhCode")

#saveRDS(q_8, file = "licitaciones_adjudicadas.rds")

q_8 = read_rds("licitaciones_adjudicadas.rds")


#Entrega una tabla con licitaciones, empresas participantes, identificando sello mujer y ganadoras. 

# q_9 = sqlQuery(con3, 
#                "SELECT    
#                     year(Oneg.FechaPublicacion) as año_publicacion
#               			, Oneg.IDFechaCierre
#               		    , month(Oneg.FechaPublicacion) as mes_publicacion	
#               			, TOC.TipoON as tamaño_licitacion
#               		    , Oneg.rbhCode 
#               			, P.entCode
#               			, O.Ganadora
#               			, Oneg.NumeroAdq
#               			, SP.TipoSello [Sello Mujer]
#               	FROM	DM_Procesos.dbo.THOportunidadesNegocio Oneg 
#               			inner join DM_Procesos.dbo.THOfertas O	on Oneg.rbhCode = O.rbhCode 
#               			left  join DM_Transaccional.dbo.DimProveedor P on P.IDSucursal=O.IdSucursal
#               			LEFT JOIN (select distinct SP.entcode, SP.[TipoSello]
#                            from [10.34.71.146\\AQUILES_CONSULTA].[DCCPMantenedor].[MSello].[SelloProveedor] SP
#                            where (SP.[TipoSello]= 3 and SP.persona =1) or  -- persona natural con sello mujer
#                           (SP.[TipoSello]= 3 and SP.persona=2 and year(SP.FechaCaducidad) >= 2022) and
#                             (year(SP.fechacreacion)<= 2022)
#                             ) sp on sp.entcode=cast(p.entcode as nvarchar(50))
#               			inner join DM_Transaccional.dbo.DimTipoON TOC on TOC.IDTipoON = Oneg.IDTipoON
#               	WHERE	O.IdEstadoOferta in (3,4,5)	
#               			and year(oneg.FechaPublicacion) >= 2022
#               			and Oneg.IDEstadoON	 >= 6")
# 
# saveRDS(q_9, file = "licitaciones_adjudicadas_segun_sello.rds")

q_9 = read_rds("licitaciones_adjudicadas_segun_sello.rds")

sum_q9 = q_9 %>% 
  mutate(Ganadora = ifelse(Ganadora == 0 | is.na(Ganadora), 0, 1), 
         `Sello Mujer` = ifelse(!is.na(`Sello Mujer`), 0, 1), 
         mujeres_ganadoras = Ganadora*`Sello Mujer`) %>% 
  group_by(mujeres_ganadoras) %>% 
  count()

q_10 =  sqlQuery(con2, "
        DECLARE @ANIO AS INT
        SET @ANIO = 2022
        
        SELECT DISTINCT
        UPPER([orgTaxID]) as rut_proveedor
        ,[orgEnterprise]
        ,UPPER([orgLegalName]) as razon_social
        ,min(cast([orgCreationDate] as date)) as fecha_de_creacion_empresa
        ,(select distinct '1'
          from [DCCPMantenedor].[MSello].[SelloProveedor] s
          where ((s.[TipoSello] = 3 and s.persona = 1 and year(s.fechacreacion) <= @ANIO) or  -- persona natural con sello mujer
                 (s.[TipoSello] = 3 and s.persona = 2 and year(s.FechaCaducidad) >= @ANIO and (year(s.fechacreacion)<= @ANIO))) and
          s.entcode = [orgEnterprise] COLLATE Modern_Spanish_CI_AI) AS Sello_Mujer
        FROM [DCCPPlatform].[dbo].[gblOrganization]
        WHERE YEAR(orgCreationDate) = @ANIO 
        AND orgClass = 1 -- proveedores o proveedoras
        AND orgIsActive = 1
        AND orgIsTest = 0
        GROUP BY UPPER([orgTaxID]),[orgEnterprise],UPPER([orgLegalName])")

#Tablas y campos útiles para el análisis 
# rbdAwardDate: Fecha de adjudicación de la licitación, viene de la tabla prcRFBDate
# rbhDocumentStatus: Código de status de documento de licitación viene de la tabla prcRFBHeader
# rbhProcessType: Código de tipo de proceso (publica, privada, trato directo), viene de la tabla prcRFBHeader
# rbhProcessSubType: Código de subtipo de proceso, viene de la tabla prcRFBHeader
# rbhOwnerTaxID ---
# rbhOwnerDistrict ---
#rbhEstimatedAmount ---
#rbhEstimatedAwardAmount ---
#rbhTotalAward ---
# 


# q_4 = sqlQuery(con2, "
#                DECLARE @FECHAINICIO DATETIME
#                 DECLARE @FECHAFINAL DATETIME
#                 SET @FECHAINICIO = '20220101'
#                 SET @FECHAFINAL = '20221231'
#                 use DCCPProcurement
#                 SELECT DISTINCT
#                 --Datos de la Adquisición
#                 prcRFBHeader.rbhExternalCode AS [Número de Adquisición],
#                 'http://www.mercadopublico.cl/fichaLicitacion.html?idLicitacion='+prcRFBHeader.rbhExternalCode AS Link,
#                 prcRFBHeader.rbhName AS [Nombre de Adquisición],
#                 prcRFBStatus.rstPublicName AS [Estado Licitación],
#                 prcRFBHeader.rbhDescription AS [Descripción de Adquisición],
#                 prcRFBProcessSubType.rpsDescripcion AS [Tipo de Adquisición],
#                 (CASE prcRFBHeader.rbhCurrency WHEN 'CLP' THEN 'Peso Chileno'
#                 WHEN 'CLF' THEN 'Unidad de Fomento' WHEN 'EUR' THEN 'Euro' WHEN 'USD' THEN 'Dólar' ELSE
#                 'Moneda revisar' END) AS [Moneda Adquisición],
#                 prcRFBDate.rbdOpeningDate AS [Fecha de Publicación],
#                 prcRFBDate.rbdTechnicalBidReception AS [Fecha de Cierre],
#                 prcRFBHeader.rbhEstimatedAwardAmount AS [Monto Estimado Adjudicado],
#                 --Organismo Demandante
#                 DCCPPlatform.dbo.gblEnterprise.entName AS Institución,
#                 ORGAComp.orgLegalName AS [Razón Social],
#                 ORGAComp.orgName AS [Unidad de compra],
#                 ORGAComp.orgTaxID AS [R.U.T]
#                 FROM prcRFBHeader 
#                 INNER JOIN prcRFBDate ON prcRFBHeader.rbhCode = prcRFBDate.rbdRFBCode 
#                 INNER JOIN prcRFBStatus ON prcRFBHeader.rbhDocumentStatus = prcRFBStatus.rstCode 
#                 INNER JOIN prcRFBProcessSubType ON prcRFBHeader.rbhProcessSubType = prcRFBProcessSubType.rpsCode 
#                 INNER JOIN DCCPPlatform.dbo.gblEnterprise ON prcRFBHeader.rbhEnterprise = DCCPPlatform.dbo.gblEnterprise.entCode 
#                 INNER JOIN DCCPPlatform.dbo.gblOrganization AS ORGAComp ON prcRFBHeader.rbhOrganization = ORGAComp.orgCode
#                 WHERE (prcRFBHeader.rbhDocumentStatus = 8) AND
#                 (prcRFBDate.rbdOpeningDfate BETWEEN @FECHAINICIO AND @FECHAFINAL)
#                ")


#Usar dim.dbo.DimComprador

q_11 = sqlQuery(con3,
               "SELECT
                    year(Oneg.FechaPublicacion) as año_publicacion
              			, Oneg.IDFechaCierre
              		    , month(Oneg.FechaPublicacion) as mes_publicacion
              			, TOC.TipoON as tamaño_licitacion
              		    , Oneg.rbhCode
              			, P.entCode [Transan]
              			, O.Ganadora
              			, O.IdEstadoOferta
              			, Oneg.NumeroAdq
              			, SP.TipoSello [Sello Mujer]
              			, I.NombreInstitucion [Institución Licitante]
              	FROM	DM_Procesos.dbo.THOportunidadesNegocio Oneg -- IdUnidaddeCompra, diminstitucion entcode con dimcomprador de entcode
              			left join DM_Procesos.dbo.DimComprador C on Oneg.IdUnidaddeCompra = C.IdUnidaddeCompra
              			left join DM_Transaccional.dbo.DimInstitucion I on C.entCode = I.entCode
              			inner join DM_Procesos.dbo.THOfertas O	on Oneg.rbhCode = O.rbhCode
              			left  join DM_Transaccional.dbo.DimProveedor P on P.IDSucursal=O.IdSucursal
              			LEFT JOIN (select distinct SP.entcode, SP.[TipoSello]
                           from [10.34.71.146\\AQUILES_CONSULTA].[DCCPMantenedor].[MSello].[SelloProveedor] SP
                           where (SP.[TipoSello]= 3 and SP.persona =1) or  -- persona natural con sello mujer
                          (SP.[TipoSello]= 3 and SP.persona=2 and year(SP.FechaCaducidad) >= 2022) and
                            (year(SP.fechacreacion)<= 2022)
                            ) sp on sp.entcode=cast(p.entcode as nvarchar(50))
              			inner join DM_Transaccional.dbo.DimTipoON TOC on TOC.IDTipoON = Oneg.IDTipoON
              	WHERE	O.IdEstadoOferta in (3,4,5)
              			and year(oneg.FechaPublicacion) >= 2022
              			and Oneg.IDEstadoON	 >= 6")

 saveRDS(q_11, file = "licitaciones_adjudicadas_segun_sello_institucion.rds")

q_11 = read_rds("licitaciones_adjudicadas_segun_sello_institucion.rds")


# q_12 = sqlQuery(con2, "
#         DECLARE @ANIO as int
#     DECLARE @FechaIni Datetime;
#     DECLARE @FechaFin Datetime;
#     /* Fecha en yyyy-mm-dd*/
#     SET @FechaIni = CONVERT(DATETIME, '2022-01-01 00:00:00', 102);
#     SET @FechaFin = CONVERT(DATETIME, '2022-12-31 00:00:00', 102);
#     SET @ANIO = 2022
# 
#     SELECT DISTINCT C.entCode AS Transan
#     /* Selecciona la columna entcode desde la tabla C */
#     FROM DCCPProcurement.dbo.prcPOHeader A with(nolock)
#     /* with(nolock) previene que otros procesos hayan bloqueado la clave */
#     /* prcPOHeader: Tabla que almacena la información de las cabeceras de las OC */
#     INNER JOIN
#     DCCPPlatform.dbo.gblOrganization B with(nolock) ON A.porSellerOrganization = B.orgCode
#     /*gb*/
#     INNER JOIN
#     DCCPPlatform.dbo.gblEnterprise C with(nolock) ON B.orgEnterprise = C.entCode
#     WHERE (A.porBuyerStatus IN (4, 5, 6, 7, 12)) AND /* Estados que validan una OC*/
#     (A.porSendDate < @FechaFin) AND
#     (A.porSendDate >= @FechaIni)
# 
#     UNION
# 
#     SELECT DISTINCT C.orgEnterprise as Transan
#     FROM DCCPProcurement.dbo.prcBIDQuote A with(nolock) /* Busca empresas que hayan ofertado*/
#     INNER JOIN
#     DCCPProcurement.dbo.prcRFBHeader B with(nolock) ON A.bidRFBCode = B.rbhCode
#     /* Tabla de licitaciones*/
#     INNER JOIN
#     DCCPPlatform.dbo.gblOrganization C with(nolock) ON A.bidOrganization = C.orgCode
#     WHERE (A.bidDocumentStatus IN (3, 4, 5)) AND
#     (A.bidEconomicIssueDate < @FechaFin) AND
#     (A.bidEconomicIssueDate >= @FechaIni)
# 
#     UNION
# 
#     SELECT DISTINCT (B.orgEnterprise) AS Transan
#     FROM DCCPProcurement.dbo.prcPOCotizacion A
#     INNER JOIN DCCPPlatform.dbo.gblOrganization B ON
#     A.proveedorRut=B.orgTaxID
#     INNER JOIN
#     DCCPProcurement.dbo.prcPOHeader C ON
#     A.porId = C.porID
#     WHERE (C.porSendDate < @FechaFin) AND
#     (C.porSendDate >= @FechaIni)
# 
#     UNION
# 
#     SELECT DISTINCT COTI.[CodigoEmpresa] collate Modern_Spanish_100_CI_AI as Transan
#     FROM DCCPCotizacion.dbo.SolicitudCotizacion as SOLI
#     INNER JOIN [DCCPCotizacion].[dbo].[Cotizacion] as COTI ON SOLI.Id = COTI.SolicitudCotizacionId
#     WHERE YEAR(SOLI.FechaCierre) = @ANIO
#   	AND EstadoId = 2 -- enviada                    "
# )
# 
# saveRDS(q_12, file = "participantes_mercado_publico_1.rds")

q_12 = read_rds("participantes_mercado_publico_1.rds")

q_13 = sqlQuery(con2, "
                DECLARE @ANIO as int
                SET @ANIO = 2022
                
                SELECT DISTINCT s.entcode as [Transan],
                s.TipoSello [Sello Mujer], 
                s.FechaCaducidad [Fecha de caducidad],
                s.fechacreacion [Fecha de creacion]
                FROM [DCCPMantenedor].[MSello].[SelloProveedor] s
                WHERE
                  (s.[TipoSello] = 3 and s.persona = 1) or  -- persona natural
                    (s.[TipoSello] = 3 and s.persona = 2
                    and year(s.FechaCaducidad) >= @ANIO
                    and year(s.fechacreacion) <= @ANIO)    -- persona juridica
                ") %>% 
  mutate(Transan = as.integer(Transan)) %>% 
  filter(!is.na(Transan)) 

disag_ind = q_12  %>%
  left_join(q_11, by = c('Transan'))  %>%
  mutate(participan = ifelse(is.na(año_publicacion) & is.na(IDFechaCierre) & 
                               is.na(mes_publicacion) & is.na(tamaño_licitacion) &
                               is.na(rbhCode) & is.na(Ganadora) &
                               is.na(IdEstadoOferta) & is.na(NumeroAdq) &
                               is.na(`Sello Mujer`) & is.na(`Institución Licitante`), 1, 0),
         Ganadora = ifelse(is.na(Ganadora) & participan!=1,1, Ganadora)) %>% 
  group_by(`Sello Mujer`, Ganadora, `Institución Licitante`, .drop = FALSE) %>%
  summarise(n= n()) %>% 
  mutate(`Sello Mujer` = ifelse(as.numeric(is.na(`Sello Mujer`)) == 1,0,1)) %>% 
  setDT() %>% 
  data.table::dcast(formula = `Institución Licitante`~`Sello Mujer`+Ganadora,
                    value.var = "n") %>% 
  rowwise() %>% 
  mutate(total = sum(`0_0`+`0_1`+`1_0`+`1_1`, na.rm = TRUE), 
         p_adj_m = `1_1`/total, 
         p_adj_h = `0_1`/total, 
         r_adj = p_adj_m/p_adj_h,
         p_ofer_m = (`1_1`+`1_0`)/total,
         p_ofer_h = (`0_1`+`0_0`)/total,
         r_ofer = p_ofer_m/p_ofer_h,
         indicador = (r_adj*r_ofer)^(1/2)
         )

saveRDS(disag_ind, file = 'indice_por_institucion.rds')

disag_ind = readr::read_rds(file = "C:/o/OneDrive - DCCP/Escritorio/Proyectos/IPPG en Mercado Público/indice_por_institucion.rds")


p_1 = ggplot(disag_ind, aes(x = indicador)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  geom_vline(aes(xintercept = median(indicador)), color = "red", linetype = "dashed", size = 10) +
  labs(title = "Distribución IPPG",
       x = "IPPG",
       y = "Frecuencia") +
  theme_minimal()

ag_ind = q_12  %>%
  left_join(q_13, by = c('Transan'))  %>%
  mutate(`Sello Mujer` = ifelse(as.numeric(is.na(`Sello Mujer`)) == 1,0,1)) %>% 
  group_by(`Sello Mujer`, .drop = FALSE) %>%
  summarise(n= n()) %>% 
  setDT() %>%
  data.table::dcast(formula = .~`Sello Mujer`,
                    value.var = "n") %>% 
  rowwise() %>% 
  mutate(p_par_h = `0`/(`1`+`0`),
         p_par_m = `1`/(`1`+`0`), 
         r_par = p_par_m/p_par_h)


ag_ind_2 = q_12  %>%
  left_join(q_11, by = c('Transan'))  %>%
  mutate(participan = ifelse(is.na(año_publicacion) & is.na(IDFechaCierre) & 
                               is.na(mes_publicacion) & is.na(tamaño_licitacion) &
                               is.na(rbhCode) & is.na(Ganadora) &
                               is.na(IdEstadoOferta) & is.na(NumeroAdq) &
                               is.na(`Sello Mujer`) & is.na(`Institución Licitante`), 1, 0),
         Ganadora = ifelse(is.na(Ganadora) & participan!=1,1, Ganadora)) %>% 
  group_by(`Sello Mujer`, Ganadora, .drop = FALSE) %>%
  summarise(n= n()) %>% 
  mutate(`Sello Mujer` = ifelse(as.numeric(is.na(`Sello Mujer`)) == 1,0,1)) %>% 
  setDT() %>% 
  data.table::dcast(formula = .~`Sello Mujer`+Ganadora,
                    value.var = "n") %>% 
  rowwise() %>% 
  mutate(total = sum(`0_0`+`0_1`+`1_0`+`1_1`, na.rm = TRUE), 
         p_adj_m = `1_1`/total, 
         p_adj_h = `0_1`/total, 
         r_adj = p_adj_m/p_adj_h,
         p_ofer_m = (`1_1`+`1_0`)/total,
         p_ofer_h = (`0_1`+`0_0`)/total,
         r_ofer = p_ofer_m/p_ofer_h
  ) %>% 
  mutate(r_par = ag_ind$r_par, 
         indicador = (r_par*r_ofer*r_adj)^(1/3))

saveRDS(ag_ind_2, file = "indice_agregado.rds")

participan = function(x) sqlQuery(con2, paste0(
  "
              DECLARE @ANIO as int
              DECLARE @FechaIni Datetime;
              DECLARE @FechaFin Datetime;
              
              /* Fecha en yyyy-mm-dd*/
              SET @FechaIni = CONVERT(DATETIME, '",x,"-01-01 00:00:00', 102);
              SET @FechaFin = CONVERT(DATETIME, '",x,"-12-31 00:00:00', 102);
              SET @ANIO = ",x,";
              
              WITH TEMP as(/*Se adjudican algún tipo de orden de compra*/
              
              SELECT DISTINCT 
                    UPPER(O.orgTaxID)  [Rut Proveedor]
                    , O.orgEnterprise [EntCode]
                    , C.entName [Razon Social]
                    , 'Se adjudica procedimiento de compra' as [Tipo de participacion]
              
              FROM DCCPProcurement.dbo.prcPOHeader A with(nolock)
              INNER JOIN DCCPPlatform.dbo.gblOrganization O with(nolock) ON A.porSellerOrganization = O.orgCode
              INNER JOIN DCCPPlatform.dbo.gblEnterprise C with(nolock) ON O.orgEnterprise = C.entCode
              WHERE (A.porBuyerStatus IN (4, 5, 6, 7, 12)) AND /* Estados que validan una OC*/
              (A.porSendDate < @FechaFin) AND
              (A.porSendDate >= @FechaIni)
          
              UNION
          
              SELECT DISTINCT
                    UPPER(C.orgTaxID) [Rut Proveedor]
                    ,C.orgEnterprise [EntCode]
                    ,C.orgLegalName [Razon Social]
                    ,'Oferta en licitaciones (o convenio Marco)' as [Tipo de participacion] 
              FROM DCCPProcurement.dbo.prcBIDQuote A with(nolock) 
              INNER JOIN DCCPProcurement.dbo.prcRFBHeader B with(nolock) ON A.bidRFBCode = B.rbhCode
              INNER JOIN DCCPPlatform.dbo.gblOrganization C with(nolock) ON A.bidOrganization = C.orgCode
              WHERE (A.bidDocumentStatus IN (3, 4, 5)) AND
                    (A.bidEconomicIssueDate < @FechaFin) AND
                    (A.bidEconomicIssueDate >= @FechaIni)
          
             
          
              /*
               UNION
              
              SELECT DISTINCT 
                    UPPER(A.proveedorRut) [Rut Proveedor]
                    ,B.orgEnterprise [EntCode]
                    ,B.orgLegalName [Razon Social]
                    ,'Entrega cotización para una consulta al mercado' as [Tipo de participacion]
              FROM DCCPProcurement.dbo.prcPOCotizacion A
              INNER JOIN DCCPPlatform.dbo.gblOrganization B ON A.proveedorRut=B.orgTaxID
              INNER JOIN DCCPProcurement.dbo.prcPOHeader C ON A.porId = C.porID
              WHERE (C.porSendDate < @FechaFin) AND
              (C.porSendDate >= @FechaIni)*/
      
              UNION
          
              SELECT DISTINCT
              UPPER(B.orgTaxID) [Rut Proveedor]
              ,COTI.CodigoEmpresa collate Modern_Spanish_100_CI_AI [EntCode]
              ,B.orgLegalName [Razon Social]
              
              ,'Entrega cotización para Compra ágil' as [Tipo de participacion]
              FROM DCCPCotizacion.dbo.SolicitudCotizacion as SOLI
              INNER JOIN [DCCPCotizacion].[dbo].[Cotizacion] as COTI ON SOLI.Id = COTI.SolicitudCotizacionId
              INNER JOIN DCCPPlatform.dbo.gblOrganization B ON COTI.CodigoEmpresa collate Modern_Spanish_100_CI_AI =B.orgEnterprise
              WHERE YEAR(SOLI.FechaCierre) = @ANIO
            	AND EstadoId = 2 -- enviada
            	)
            	
            	SELECT DISTINCT
                  	STRING_AGG(T.[Rut Proveedor], ',') [Rut Proveedor]
                  	,T.[EntCode] [EntCode]
                  	, STRING_AGG(T.[Razon Social], ',') [Razon Social]
                  	, (CASE s.TipoSello WHEN 3 THEN 'Mujeres' ELSE 'Hombres' END) [Sello Mujer]
                  	, STRING_AGG(T.[Tipo de participacion], ',') [Tipo de participacion] 
                  	, COUNT(T.EntCode) [Procesos]
            	FROM TEMP T
            	LEFT JOIN (SELECT distinct s.EntCode, s.TipoSello
                FROM [DCCPMantenedor].[MSello].[SelloProveedor] s
                WHERE (s.[TipoSello]= 3 and s.persona =1) or  -- persona natural con sello mujer
                (s.[TipoSello]= 3 and s.persona=2 and year(s.FechaCaducidad) >= @ANIO) and
                (year(s.fechacreacion)<= @ANIO)
                ) s on T.EntCode=s.EntCode 
            	GROUP BY
            	      T.[EntCode], s.[TipoSello]
                     "
  
)
)

# part = participan(2022) 
# 
# saveRDS(part, file = paste0(gsub("-", "", today()),gsub(" ","_"," participan de algún proceso de compra 2022.rds")))

part = readr::read_rds(file = list.files(pattern = "participan")) 

###################################
# Otros ==========================
###################################


licitantes = function(x) sqlQuery(con3,paste0(
  "        
                   DECLARE @YEAR AS INT;
                   SET @YEAR = ",x,";
                   
                   WITH TEMP as(
                   SELECT
                        year(Oneg.FechaPublicacion) as año_publicacion
                  			, Oneg.IDFechaCierre
                		    , month(Oneg.FechaPublicacion) as mes_publicacion
                		    , Oneg.rbhCode
                		    , P.RazonSocialSucursal [Razon Social]
                  			, P.entCode [EntCode]
                  			, P.RUTSucursal [Rut Proveedor]
                  			, (CASE O.Ganadora WHEN  1 THEN 1 ELSE 0 END) [Ganadora]
                  			, Oneg.NumeroAdq
                  			, SP.Id_Tipo_Sello [Sello Mujer]
                  	FROM	DM_Procesos.dbo.THOportunidadesNegocio Oneg
              			INNER JOIN DM_Procesos.dbo.THOfertas O  on Oneg.rbhCode = O.rbhCode
              			LEFT JOIN DM_Transaccional.dbo.DimProveedor P on P.IDSucursal=O.IdSucursal
                    LEFT JOIN (select distinct SP.EntCode, SP.Id_Tipo_Sello
                         from [DM_RegistroProveedores].[dbo].[Sello_Proveedor] SP
                         where (SP.[Id_Tipo_Sello]= 3) or  -- persona natural con sello mujer
                        (SP.[Id_Tipo_Sello]= 3 and year(SP.Fecha_Caducidad) >= @YEAR) and
                          (year(SP.Fecha_Creacion)<= @YEAR)
                          ) sp on sp.entcode=cast(p.entcode as nvarchar(50))
                  	WHERE	O.IdEstadoOferta in (3,4,5)
                  			and year(oneg.FechaPublicacion) = 2022
                  			and Oneg.IDEstadoON	 >= 6
                  			)
                  			
                  SELECT 
                         
                         T.[Rut Proveedor]
                         ,T.EntCode
                         ,T.[Razon Social]
                         ,(CASE T.[Sello Mujer] WHEN 3 THEN 'Mujeres' ELSE 'Hombres' END) [Sello Mujer]
                         ,SUM(cast(T.Ganadora as INT)) [Adjudicadas]
                   FROM TEMP T
                   GROUP BY T.año_publicacion
                   , T.[Rut Proveedor]
                   ,T.EntCode
                   , T.[Razon Social]
                   , T.[Sello Mujer]
                   , T.Ganadora
                    
               ")
)

#lic = licitantes(2022)  
# 
# saveRDS(lic, file = paste0(gsub("-", "", today()),gsub(" ","_"," empresas licitantes 2022.rds")))

n_ = length(list.files(pattern = "licitantes"))

lic = readr::read_rds(file = list.files(pattern = "licitantes")[n_])
