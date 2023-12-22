              DECLARE @YEAR AS INT;
              DECLARE @MONTH AS INT;
                
              SET @MONTH = 12;
              SET @YEAR = 2022;
                
              DECLARE @CURRENTMONTH datetime = datetimefromparts(@YEAR, @MONTH, 1,0,0,0,0);
              DECLARE @startDate datetime = dateadd(month, -11, @currentMonth)
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