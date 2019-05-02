En el anterior [post](https://alonmar.github.io/knn-comida/), hablábamos de un algoritmo que se encuentra en la categoría de algoritmos **supervisados**, ya que cada observación tiene una etiqueta que indica la clase correspondiente, ahora toca el turno de un ejemplo de algoritmo **no supervisado**. Crearemos un sistema de recomendaciones para visitar nuevos negocios.

Después de completar este tutorial, sabrás:

-   Que es el coeficiente jaccard
-   Crear una matriz one hot
-   Como crear un sistema de recomendaciones

Nos apoyaremos del [dataset](https://sites.google.com/site/yangdingqi/home/foursquare-dataset) el cual incluye los ids de personas y negocios, y las visitas que hicieron las personas a dichos negocios tomado de [foursquare](https://es.foursquare.com/)

``` r
#cargamos los paquetes necesarios
library(tidyverse) #Manipulación
library(knitr)     #Presentación de tablas
```

Cargamos la base de datos

``` r
data <- read_csv("https://raw.githubusercontent.com/alonmar/sistema-de-recomendacion/master/data/checkins.csv")
#Nombramos las variables
names(data) <- c("user_id","negocio_id")
head(data,10) %>% kable()
```

|  user\_id|  negocio\_id|
|---------:|------------:|
|     35443|          899|
|     24973|        42406|
|     14860|          177|
|    222505|          177|
|     63524|          609|
|     51957|       435580|
|      7860|        68829|
|      8952|        12790|
|     42283|        15071|
|     14506|        25864|

Veamos cuantas veces visito cada persona a cada negocio

``` r
data %>% group_by(user_id,negocio_id) %>%
  summarise(n = n()) %>% arrange(desc(n)) %>%
  head(15) %>% kable()
```

|  user\_id|  negocio\_id|    n|
|---------:|------------:|----:|
|     51957|       435580|  149|
|     51445|        64971|   92|
|     17238|         9209|   86|
|      8952|        39242|   74|
|    133970|        41096|   73|
|     39836|        15743|   72|
|     38456|         7999|   69|
|     59959|         5222|   63|
|     17238|        82634|   55|
|     49029|       139117|   55|
|     54405|        41096|   55|
|     56139|        86250|   54|
|     18345|        42705|   53|
|      2033|        14327|   51|
|     91070|        35085|   51|

Como vimos si una persona visita mas de una vez un mismo lugar esa observación aparecerá dos veces. Veamos cuantas personas y negocios únicos tenemos

``` r
cat("Usuarios unicos: ")
```

    ## Usuarios unicos:

``` r
cat(unique(data$user_id) %>% length())
```

    ## 2060

``` r
cat("\n")
```

``` r
cat("Negocios unicos: ")
```

    ## Negocios unicos:

``` r
cat(unique(data$negocio_id) %>% length())
```

    ## 2876

Para este tutorial elimináramos las observaciones que solo tienen una visita, ya que nos puede indicar que no se tomo la molestia de regresar a ese negocio, en cambio nos quedaremos con aquellos que regresaron una o mas veces.

``` r
filter_data <- data %>% group_by(user_id,negocio_id) %>%
  summarise(n = n()) %>% arrange(desc(n)) %>% filter(n > 1)

filter_data %>% head(5) %>%  kable()
```

|  user\_id|  negocio\_id|    n|
|---------:|------------:|----:|
|     51957|       435580|  149|
|     51445|        64971|   92|
|     17238|         9209|   86|
|      8952|        39242|   74|
|    133970|        41096|   73|

``` r
filter_data %>% tail(5) %>%  kable()
```

|  user\_id|  negocio\_id|    n|
|---------:|------------:|----:|
|    266532|         4310|    2|
|    266532|        10604|    2|
|    266532|        22768|    2|
|    266712|         4889|    2|
|    266712|         9953|    2|

Ahora lo que haremos es lo que se conoce como [**one hot coding**](https://hackernoon.com/what-is-one-hot-encoding-why-and-when-do-you-have-to-use-it-e3c6186d008f). Básicamente se trata de crear una matriz donde si un valor corresponde tendremos un **"1"** y de lo contrario tendremos un **0**

Veamos esta tabla:

| Author                 | Nacionalidad |
|:-----------------------|:-------------|
| Juan Rulfo             | MEX          |
| Gabriel García Márquez | COL          |
| Octavio Paz            | MEX          |

Ahora transformada en one-hot se vería así

| Author                 |  COL|  MEX|
|:-----------------------|----:|----:|
| Gabriel García Márquez |    1|    0|
| Juan Rulfo             |    0|    1|
| Octavio Paz            |    0|    1|

Sin embargo para nuestro ejemplo colocaremos NAs en lugar de ceros, ya que nos apoyaremos de la función **drop\_na()**.

De tal manera que esta sería nuestra nueva matriz, donde las filas corresponden a cada ID de negocio, y las columnas a cada ID de usuario.

``` r
data_matrix <- filter_data %>% select(-n) %>%
  mutate(count= 1) %>%
  spread(user_id,count)

data_matrix[1:10,1:10] %>% kable()
```

|  negocio\_id|    6|   15|   72|  102|  104|  105|  132|  133|  134|
|------------:|----:|----:|----:|----:|----:|----:|----:|----:|----:|
|           15|   NA|   NA|   NA|   NA|   NA|   NA|   NA|   NA|   NA|
|           20|   NA|   NA|   NA|   NA|   NA|   NA|   NA|   NA|   NA|
|           25|   NA|   NA|   NA|   NA|   NA|   NA|   NA|   NA|   NA|
|           36|   NA|   NA|   NA|   NA|   NA|   NA|   NA|   NA|   NA|
|           39|   NA|   NA|   NA|   NA|   NA|   NA|   NA|   NA|   NA|
|           40|   NA|   NA|   NA|   NA|   NA|   NA|   NA|   NA|   NA|
|           42|   NA|   NA|   NA|   NA|   NA|   NA|   NA|   NA|   NA|
|           46|   NA|   NA|   NA|   NA|   NA|   NA|   NA|   NA|   NA|
|           47|   NA|   NA|   NA|   NA|   NA|   NA|   NA|   NA|   NA|
|           52|   NA|   NA|   NA|   NA|   NA|   NA|   NA|   NA|   NA|

Solo vemos NAs, ya que para esos negocios y esos usuario no existe interacción, sin embargo veamos un caso concreto

Seleccionemos un id de usuario y eliminaremos los NA

``` r
v1 <- data_matrix %>% select(negocio_id,'14860')  %>% drop_na()
v1 %>% kable()
```

|  negocio\_id|  14860|
|------------:|------:|
|          177|      1|
|          578|      1|
|          837|      1|
|         7542|      1|
|        11872|      1|
|        13630|      1|
|        14790|      1|
|        16126|      1|
|        19954|      1|
|        24001|      1|
|        76187|      1|

seleccionaremos otro id de usuario para ver que tan similares son con el anterior

``` r
v2 <- data_matrix %>% select(negocio_id,'222505')  %>% drop_na()
v2 %>% kable()
```

|  negocio\_id|  222505|
|------------:|-------:|
|          177|       1|
|         5867|       1|

Haremos uso del coeficiente de [**Jaccard**](https://en.wikipedia.org/wiki/Jaccard_index), siendo el cociente de la intersección de dos conjuntos entre la unión de dichos conjuntos.

$ $

En otras palabras dividiremos el numero de elementos que se encuentran en ambos conjuntos entre el numero de elementos únicos de ambos conjuntos.

Ejemplo: **conjunto\_A = {a,b,c,c,d}** **conjunto\_B = {b,d,e}**

*A*⋂*B* = (b,d) es decir solo tenemos **2** elementos que se encuentran en A y B *A*⋂*B* = (a,b,c,d,e) lo cual significa existen **5** elementos únicos en ambos conjuntos

$jaccar = \\frac{2}{5} = 0.4$

De tal manera que el índice de jaccard nos indica que el conjunto A y B son similares en **0.4** en una escala de 0 a 1

Regresando a los 2 usuarios que tomamos antes veamos que tan similares son

``` r
intersect = length(intersect(v1$negocio_id,v2$negocio_id))
union = length(unique(v1$negocio_id,v2$negocio_id))

jaccard = intersect/union
jaccard
```

    ## [1] 0.0909

Lo cual significa que ambos usuarios son 0.0909 similares.

A continuación crearemos una función que nos ayudara a calcular el índice de Jaccard

``` r
jaccard <- function(index,v2,v1) {
  v2 <- data.frame(index,v2)
  v2 <- v2 %>% drop_na()
  length(intersect(v1$negocio_id,v2$index))/length(unique(v1$negocio_id,v2$index))
}
```

Ahora aplicaremos esta función a toda la matriz antes creada para conocer que similitud mantiene con cada uno de los otros usuarios

``` r
#creamos el vector que usaremos para buscar el coeficiente de jaccard
v1 <- data_matrix %>% select(negocio_id,'14860') %>% drop_na()
#guardamos los indices de los negicios
index <- data_matrix$negocio_id
# aplicamos el coeficiente de jaccard para todas las columnas
result <- sapply(data_matrix %>% select(-1,-'14860'),
                 function(x) jaccard(index,x,v1))


# Filtramos los resultados para solo quedarnos con aquellos que tienen un
# coeficiente mayor a cero
result <- result[result > 0]
# guardamos los ids de los usuarios
names <- result %>% names()

#observamos solo el top 10
data.frame(names = names,similitud = c(result)) %>%
   arrange(desc(similitud)) %>% head(10)
```

    ##     names similitud
    ## 1   45720    0.1818
    ## 2  181505    0.1818
    ## 3     836    0.0909
    ## 4    1174    0.0909
    ## 5    1581    0.0909
    ## 6    2637    0.0909
    ## 7    4403    0.0909
    ## 8    6083    0.0909
    ## 9    6243    0.0909
    ## 10   6998    0.0909

Perfecto!

Como vemos el usuarios con el ID 14860 mantiene una mayor similitud con el usuarios 45720, a continuación recomendaremos lugares que ya visito el segundo usuarios y que el primero aun no conoce

``` r
v1 <- data_matrix %>% select(negocio_id,'14860') %>% drop_na()
v2 <- data_matrix %>% select(negocio_id,'45720') %>% drop_na()

v2$negocio_id [!v2$negocio_id %in% v1$negocio_id]
```

    ##  [1]     155     243     639     653     985    1483    4920    6434
    ##  [9]    6764    7507    7999    8953    9307    9448   10192   10784
    ## [17]   12159   12496   12500   12833   12875   12910   13233   13661
    ## [25]   13890   14714   15139   15189   15463   16506   18177   18191
    ## [33]   25695   25791   25883   26051   27553   28788   29594   31548
    ## [41]   32783   71641  108408  136335 1760748

Ahora metamos todo esto en una sola función donde seleccionemos el ID al cual nos interesa recomendarle establecimientos de acuerdo a el usuarios que mas se le parezca

``` r
recomendacion <- function(id,data) {
id <- as.character(id)
#creamos el vector que usaremos para buscar el coeficiente de jaccard
v1 <- data %>% select(negocio_id,id) %>% drop_na()
#guardamos los índices de los negocios
index <- data$negocio_id
# aplicamos el coeficiente de jaccard para todas las columnas excepto la primera
# que es donde se encuentran los índices y la columna del usuario
result <- sapply(data %>% select(-1,-id),
                 function(x) jaccard(index,x,v1))


# Filtramos los resultados para solo quedarnos con aquellos que tienen un
# coeficiente mayor a cero
result <- result[result > 0]
# guardamos los ids de los usuarios
names <- result %>% names()

id_recomender <- data.frame(names = names,similitud = c(result)) %>%
   arrange(desc(similitud))

v2 <- data %>% select(negocio_id,as.character(id_recomender[1,1])) %>% drop_na()

cat("Quizás te interesen estos lugares: \n\n")
data.frame(negocio_id = (v2$negocio_id [!v2$negocio_id %in% v1$negocio_id])) %>%
  kable()

}
```

``` r
recomendacion("190",data_matrix)
```

    ## Quizás te interesen estos lugares:

|  negocio\_id|
|------------:|
|         2585|
|        13119|
|        13233|

Conclusiones
------------

Este es nuestro primer ejemplo de algoritmo *No clasificado* y nuestro primer sistema de recomendación, es un ejemplo excelente para comprender ambos conceptos y como vimos en la medida que obtengamos mas data acerca de los intereses de los usuarios las recomendaciones serán mejor dadas. Te invito a que elijas un usuario y veas que recomendaciones existen para él, y también te invito a que en lugar de lugares visitados mas de una vez sean mas de 5 veces ¿que implicaciones tendrá? y ¿mas de 10 veces?.

Fuentes
-------

<https://tidyr.tidyverse.org/reference/spread.html> <https://hackernoon.com/what-is-one-hot-encoding-why-and-when-do-you-have-to-use-it-e3c6186d008f>
