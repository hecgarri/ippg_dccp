Cuaderno de investigación: Índice de Participación con Perspectiva de
Género
================

Un **cuaderno de investigación**, también conocido como cuaderno de
laboratorio o cuaderno de campo, es un documento utilizado por
investigadores para registrar de manera sistemática y detallada todas
las etapas y aspectos de un proyecto de investigación. Este cuaderno
sirve como un registro permanente de las observaciones, experimentos,
resultados, análisis y cualquier otro dato relevante asociado con la
investigación.

## ChileCompra

<span style="color: red;"> Estos bullets se usaron para la presentación
realizada con funcionarias del ministerio de hacienda, por tanto, se
consideró necesario hacer un recordatorio del quehacer del servicio, sin
embargo, para uso interno es prescindible </span>

- La Dirección de Compras y Contratación Pública, Dirección ChileCompra
  es una entidad clave para más de 1.000 organismos públicos en Chile,
  facilitando la adquisición de bienes y servicios. A través de su
  plataforma transaccional, [Mercado
  Público](https://www.mercadopublico.cl).

- En 2022 las transacciones de organismos del Estado a través de la
  plataforma de ascendieron a más de **US\$ 15.000 millones**, cerca del
  5% del PIB, a través de más de 1.800.000 órdenes de compra, siendo uno
  de los mercados más importantes del país.

- Hoy fue promulgada la modernización de la Ley de Compras Públicas, con
  el objetivo de mejorar la calidad del gasto, elevar la probidad y
  transparencia, e integrar principios de economía circular.

## Motivación

La Dirección ChileCompra actualizó su directiva incorporando perspectiva
de género en las compras públicas, buscando abordar desigualdades y
roles de género para promover una sociedad más equitativa.

- La autonomía de la mujer, clave para su independencia en decisiones
  físicas, educativas y económicas, destaca como pilar crucial para
  lograr igualdad de oportunidades.

- La autonomía económica de las mujeres haya apoyo estratégico en las
  compras públicas, ofreciendo oportunidades y contribuyendo a su
  independencia financiera.

- De allí que el **Objetivo General** de esta propuesta consiste en
  **desarrollar e implementar un indicador para medir la participación
  de mujeres como proveedoras en el sistema de compras públicas** de
  Chile, con el propósito de evaluar y promover la equidad de género en
  las distintas etapas del proceso y contribuir a la autonomía económica
  de las mujeres.

## Contexto: Empresas lideradas por mujeres según Monto transado y órdenes de compra

![](README_files/figure-gfm/primer-1.png)<!-- --> Este gráfico muestra
que aunque no hay variaciones importantes entre ambos períodos entre
cantidad de órdenes de compra y montos transados, sí se puede apreciar
una leve mejora en términos de participación en los montos pasando de un
17% en 2022 para las mujeres a un 19,7%, sobre todo si se tiene en
cuenta que a pesar de que el año 2023 no ha concluido, se aprecia un
total transado muy similar al año anterior.

## Monto y cantidad de órdenes de compra según procedimiento de compra

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

<span style="color: red;"> Aunque es interesante realizar una apertura
por procedimiento de compra, este gráfico no refleja adecuadamente la
realidad pues no puede ser que el % de participación de las empresas
lideradas por mujeres sea inferior en todos los procedimientos de compra
en relación con el total. Por lo tanto debo verificar que la query está
bien construida. Revisar archivo ‘indicadores_complementarios_20231213’
</span>

## Instituciones según importancia de empresas lideradas por mujeres

``` r
q_2 = readr::read_rds(file = "q2_monto_cantidad_oc_segun_sello_institucion.rds") %>% 
  setDT() %>% 
  data.table::dcast(formula = NombreInstitucion ~ Sello,
                    value.var = c("Monto anual USD", "Cantidad OC")) %>% 
  arrange(desc(`Monto anual USD_Mujeres`)) %>% 
  mutate(total_oc = `Cantidad OC_Hombres`+`Cantidad OC_Mujeres`,
         perc_muj=`Cantidad OC_Mujeres`/total_oc,
         total_usd = `Monto anual USD_Mujeres`+`Monto anual USD_Hombres`,
         perc_usd = `Monto anual USD_Mujeres`/total_usd)
  


(
  oc_perc_plot = ggplot(q_2, aes(x = perc_muj, y = `perc_usd`, size = total_usd)) +
    geom_point() +
    geom_vline(xintercept = mean(q_2$perc_muj,na.rm = TRUE), linetype = "dashed", color = "blue", linewidth = 1) +
    geom_hline(yintercept = mean(q_2$perc_usd,na.rm = TRUE), linetype = "dashed", color = "red", linewidth = 1) +
    labs(title = "Participación de las mujeres en OC y Montos",
         x = "Porcentaje OC's",
         y = "porcentaje Monto OC's",
         size = "Monto OC's") +
    theme_minimal()
)
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Los principales compradores del primer cuadrante para las empresas con Sello Mujer

``` r
cuad_1 = q_2 %>% 
  mutate(media_x=mean(perc_muj,na.rm = TRUE),
         media_y = mean(perc_usd,na.rm = TRUE)) %>% 
  filter(perc_muj> media_x & perc_usd>media_y) %>%
  arrange(desc(`Monto anual USD_Mujeres`)) %>% 
  mutate(monto_mujeres = `Monto anual USD_Mujeres`)

cuad_1_long <- tidyr::pivot_longer(cuad_1 %>% 
  arrange(desc(`Monto anual USD_Mujeres`)), cols = c(`Monto anual USD_Hombres`, `Monto anual USD_Mujeres`), names_to = "Genero", values_to = "Monto") %>% 
  arrange(desc(monto_mujeres))



# Define los colores para "Hombres" y "Mujeres"
colores <- c("Monto anual USD_Hombres" = "#87CEEB",  # Celeste pastel para Hombres
             "Monto anual USD_Mujeres" = "#FFB6C1")  # Rosado pastel para Mujeres


# Crear el gráfico apilado
cuad_plot_1 <- ggplot(cuad_1_long[1:20,], aes(x = reorder(NombreInstitucion, monto_mujeres), y = Monto, fill = Genero)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
    scale_fill_manual(values = colores)+
  labs(title = "Monto transado por Institución", y = "Cantidad")+
  geom_text(data = cuad_1_long[1:20, ],
            aes(label = scales::comma(round(Monto)), y = Monto),
            position = position_dodge(width = 1),  # Ancho de la posición dodge
            vjust = 0.5)+
  theme_minimal()+
  coord_flip()+
  labs(x = "Instituciones", y = "Monto en millones de dólares")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
    theme(legend.position = "none")

cuad_1 = q_2 %>% 
  mutate(media_x=mean(perc_muj,na.rm = TRUE),
         media_y = mean(perc_usd,na.rm = TRUE)) %>% 
  filter(perc_muj> media_x & perc_usd>media_y) %>% 
  mutate(cantidad_mujeres = `Cantidad OC_Mujeres`)

cuad_1_long <- tidyr::pivot_longer(cuad_1 %>% 
  arrange(desc(`Cantidad OC_Mujeres`)), cols = c(`Cantidad OC_Hombres`, `Cantidad OC_Mujeres`), names_to = "Genero", values_to = "OCs") 

colores <- c("Cantidad OC_Hombres" = "#87CEEB",  # Celeste pastel para Hombres
             "Cantidad OC_Mujeres" = "#FFB6C1")  # Rosado pastel para Mujeres


# Crear el gráfico apilado
cuad_plot_2 <- ggplot(cuad_1_long[1:20,], aes(x = reorder(NombreInstitucion, cantidad_mujeres), y = OCs, fill = Genero)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
    scale_fill_manual(values = colores)+
  labs(title = "Cantidad de órdenes de compra por institución", y = "Cantidad")+
  geom_text(data = cuad_1_long[1:20, ],
            aes(label = scales::comma(round(OCs)), y = OCs),
            position = position_dodge(width = 1),  # Ancho de la posición dodge
            vjust = 0.5)+
  theme_minimal()+
  coord_flip()+
  labs(x = "Instituciones", y = "Cantidad de órdenes de compra")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
    theme(legend.position = "none")



grid.arrange(cuad_plot_1, cuad_plot_2, ncol = 2)
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Cantidad y Tasa de Participación de proveedoras en el sistema

``` r
q_4 = readr::read_rds(file = "sello_proveedores.rds") %>% 
  mutate(Sello = formattable::comma(Sello, decimal.mark = ",", big.mark = "."), 
         prop = formattable::comma(prop, decimal.mark = ",", big.mark = ".")) 

colores <- c("Hombres" = "#87CEEB",  # Celeste pastel para Hombres
             "Mujeres" = "#FFB6C1")  # Rosado pastel para Mujeres

ggplot(q_4, aes(x = reorder(`Sello Mujer`, -Sello), y = `Sello`, fill = `Sello Mujer`)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = colores)+
   geom_text(aes(label = paste0(round(prop, 1), "%")), position = position_stack(vjust = 0.5)) +
   geom_text(aes(label = paste0(round(Sello, 0))), position = position_stack(vjust = 1.05))+
  labs(title = "Empresas con Sello mujer 2022",
       x = "Sello Mujer",
       y = "Cantidad proveedores",
       caption = "Elaboración propia a partir de datos de ChileCompra") +
  theme_minimal() +
  theme(legend.position = "top")
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
# kable(
#   q_4,
#   col.names = c("Sello Mujer", "Cantidad proveedores", "Proporción del total"),
#   digits = 2,
#   caption = "Empresas con Sello mujer 2022"
#   ) %>% kable_styling(font_size = 7, latex_options="scale_down") %>%
#   footnote(
#     general = "Elaboración propia a partir de datos de ChileCompra",
#     general_title = "Nota:",
#     footnote_as_chunk = TRUE
#     )
```

## Propuesta

- El indice que proponemos, siguiendo de cerca el trabajo de
  Permanyer (2010) es el siguiente:

$$
IPPG_{t}^{i} = \sqrt[n_{j}]{\prod_{j=1}^{n_{j}}R_{j,t}^{i}}
$$

Donde: $$
 R_{j,t}^{i} = \frac{P_{M,j,t}^{i}}{P_{H,j,t}^{i}} 
  $$

En que $i=1,...,M$ corresponde a las instituciones del Estado;
$j = 1,..,n_{j}$, corresponde a las dimensiones a considerar; $t=1,..,T$
identifica la dimensión tiempo, por último, el subíndice $M$ a las
empresas lideradas por mujeres y $H$, aquellas lideradas por hombres

## Propuesta

En este caso, $R_{j,t}^{i}$ es un odds ratio para cada una de las etapas
del proceso de participación, representando por tanto la razón entre las
probabilidades tanto de hombres como mujeres en las diferentes etapas
del proceso, es decir, las chance de:

- Participar en Mercado Público: Medida como la razón entre el número de
  empresas lideradas por mujeres y aquellas lideradas por hombres
  inscritas en la plataforma.
- Ofertar en algún procedimiento de compra: Razón entre empresas
  lideradas por mujeres y por hombres que ofertan en licitaciones,
  compra ágil, convenio marco, etc.
- Recibir una orden de compra: Razón entre el número de empresas
  lideradas por mujeres y por hombres, que reciben una orden de compra

## Resultados preliminares

El índice tiene como rango valores entre 0 e $\infty$, valores mayores a
1 indican una mayor preferencia por la contratación de empresas
lideradas por mujeres, mientras que un valor cercano a 0 una mayor
preferencia por empresas lideradas por hombres

``` r
ind_hist = readr::read_rds(file = "data_index_inst.rds")


(
  ind_hist = ggplot(ind_hist %>% 
                      arrange(indicador), aes(indicador, , y = ..density..)) +
    geom_histogram(binwidth = 0.01, fill = "blue", color = "black") +
    geom_density(color = "red", size = 1) +  # Agrega la curva de densidad
    geom_hline(yintercept = mean(datos_ind_fil$indicador), linetype = "dashed", color = "red", size = 1) +
    labs(title = "Histograma del IPPG",
         x = "IPPG",
         y = "Frecuencia")+
    theme_minimal()
  
)
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: The dot-dot notation (`..density..`) was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `after_stat(density)` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

<img src="README_files/figure-gfm/unnamed-chunk-6-1.png" width="80%" />

## Resultados preliminares

``` r
ind_hist = readr::read_rds(file = "20231126_datos_índice_desagregado_por_instituciones.rds")


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
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Agenda

- Determinar la capacidad discriminativa del indicador realizando cruces
  adicionales de información

- Analizar la evolución del fenómeno mediante la construcción de un
  índice de carácter temporal

- Automatizar la rutina de cálculo del índice para asegurar su
  oportunidad

- Identificar posibles variables que permitan explicar el comportamiento
  del índice y de esta manera comprender las razones del fenómeno
