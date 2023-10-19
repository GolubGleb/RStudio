# Основы обработки данных с помощью R
Тыван Максим Владимирович

# Цель работы

1.  Развить практические навыки использования языка программирования R
    для обработки данных

2.  Закрепить знания базовых типов данных языка R

3.  Развить пркатические навыки использования функций обработки данных
    пакета dplyr -- функции select(), filter(), mutate(), arrange(),
    group_by()

# Задание

Проанализировать встроенный в пакет dplyr набор данных starwars с
помощью языка R и ответить на вопросы:

``` r
library(dplyr)
```


    Присоединяю пакет: 'dplyr'

    Следующие объекты скрыты от 'package:stats':

        filter, lag

    Следующие объекты скрыты от 'package:base':

        intersect, setdiff, setequal, union

1.Сколько строк в датафрейме?

``` r
starwars %>% nrow()
```

    [1] 87

1.  Сколько столбцов в датафрейме?

``` r
starwars %>% ncol()
```

    [1] 14

1.  Как просмотреть примерный вид датафрейма?

``` r
starwars %>% glimpse()
```

    Rows: 87
    Columns: 14
    $ name       <chr> "Luke Skywalker", "C-3PO", "R2-D2", "Darth Vader", "Leia Or…
    $ height     <int> 172, 167, 96, 202, 150, 178, 165, 97, 183, 182, 188, 180, 2…
    $ mass       <dbl> 77.0, 75.0, 32.0, 136.0, 49.0, 120.0, 75.0, 32.0, 84.0, 77.…
    $ hair_color <chr> "blond", NA, NA, "none", "brown", "brown, grey", "brown", N…
    $ skin_color <chr> "fair", "gold", "white, blue", "white", "light", "light", "…
    $ eye_color  <chr> "blue", "yellow", "red", "yellow", "brown", "blue", "blue",…
    $ birth_year <dbl> 19.0, 112.0, 33.0, 41.9, 19.0, 52.0, 47.0, NA, 24.0, 57.0, …
    $ sex        <chr> "male", "none", "none", "male", "female", "male", "female",…
    $ gender     <chr> "masculine", "masculine", "masculine", "masculine", "femini…
    $ homeworld  <chr> "Tatooine", "Tatooine", "Naboo", "Tatooine", "Alderaan", "T…
    $ species    <chr> "Human", "Droid", "Droid", "Human", "Human", "Human", "Huma…
    $ films      <list> <"The Empire Strikes Back", "Revenge of the Sith", "Return…
    $ vehicles   <list> <"Snowspeeder", "Imperial Speeder Bike">, <>, <>, <>, "Imp…
    $ starships  <list> <"X-wing", "Imperial shuttle">, <>, <>, "TIE Advanced x1",…

1.  Сколько уникальных рас персонажей (species) представлено в данных?

``` r
length(unique(starwars$species))
```

    [1] 38

1.  Найти самого высокого персонажа.

``` r
arrange(starwars, desc(height), by_group=TRUE) %>% slice(1:1) %>% select(name:height)
```

    # A tibble: 1 × 2
      name        height
      <chr>        <int>
    1 Yarael Poof    264

1.  Найти всех персонажей ниже 170

``` r
filter(starwars, height < 170) %>% pull(name)
```

     [1] "C-3PO"                 "R2-D2"                 "Leia Organa"          
     [4] "Beru Whitesun lars"    "R5-D4"                 "Yoda"                 
     [7] "Mon Mothma"            "Wicket Systri Warrick" "Nien Nunb"            
    [10] "Watto"                 "Sebulba"               "Shmi Skywalker"       
    [13] "Dud Bolt"              "Gasgano"               "Ben Quadinaros"       
    [16] "Cordé"                 "Barriss Offee"         "Dormé"                
    [19] "Zam Wesell"            "Jocasta Nu"            "Ratts Tyerell"        
    [22] "R4-P17"                "Padmé Amidala"        

1.  Подсчитать ИМТ (индекс массы тела) для всех персонажей. ИМТ
    подсчитать по формуле 𝐼 = 𝑚/ℎ^2, где 𝑚 – масса (weight), а ℎ – рост
    (height).

``` r
starwars %>% mutate(IMT = mass / height^2) %>% select(name,IMT)
```

    # A tibble: 87 × 2
       name                   IMT
       <chr>                <dbl>
     1 Luke Skywalker     0.00260
     2 C-3PO              0.00269
     3 R2-D2              0.00347
     4 Darth Vader        0.00333
     5 Leia Organa        0.00218
     6 Owen Lars          0.00379
     7 Beru Whitesun lars 0.00275
     8 R5-D4              0.00340
     9 Biggs Darklighter  0.00251
    10 Obi-Wan Kenobi     0.00232
    # ℹ 77 more rows

1.  Найти 10 самых “вытянутых” персонажей. “Вытянутость” оценить по
    отношению массы (mass) к росту(height) персонажей.

``` r
starwars %>% mutate(Вытянутость = mass/height) %>% arrange(desc(Вытянутость), by_group=TRUE) %>% select(name,Вытянутость)
```

    # A tibble: 87 × 2
       name                  Вытянутость
       <chr>                       <dbl>
     1 Jabba Desilijic Tiure       7.76 
     2 Grievous                    0.736
     3 IG-88                       0.7  
     4 Owen Lars                   0.674
     5 Darth Vader                 0.673
     6 Jek Tono Porkins            0.611
     7 Bossk                       0.595
     8 Tarfful                     0.581
     9 Dexter Jettster             0.515
    10 Chewbacca                   0.491
    # ℹ 77 more rows

1.  Найти средний возраст персонажей каждой расы вселенной Звездных
    войн.

``` r
starwars %>% group_by(species) %>% filter(!is.na(birth_year)) %>% summarise(MidAge = mean(birth_year, na.rm = TRUE)) %>% select(species,MidAge) %>% arrange(desc(MidAge))
```

    # A tibble: 16 × 2
       species        MidAge
       <chr>           <dbl>
     1 Yoda's species  896  
     2 Hutt            600  
     3 Wookiee         200  
     4 Cerean           92  
     5 <NA>             62  
     6 Zabrak           54  
     7 Human            53.4
     8 Droid            53.3
     9 Trandoshan       53  
    10 Gungan           52  
    11 Mirialan         49  
    12 Twi'lek          48  
    13 Rodian           44  
    14 Mon Calamari     41  
    15 Kel Dor          22  
    16 Ewok              8  

1.  Найти самый распространенный цвет глаз персонажей вселенной Звездных
    войн.

``` r
starwars %>% group_by(eye_color) %>% summarise(Количество = n()) %>% arrange(desc(Количество)) %>% slice(1:1)
```

    # A tibble: 1 × 2
      eye_color Количество
      <chr>          <int>
    1 brown             21

1.  Подсчитать среднюю длину имени в каждой расе вселенной Звездных
    войн.

``` r
starwars %>% group_by(species) %>% mutate(NL = nchar(name)) %>% summarise(nameleng = mean(NL)) %>% select(species, nameleng) %>% arrange(desc(nameleng))
```

    # A tibble: 38 × 2
       species   nameleng
       <chr>        <dbl>
     1 Ewok          21  
     2 Hutt          21  
     3 Geonosian     17  
     4 Besalisk      15  
     5 Mirialan      14  
     6 Toong         14  
     7 Aleena        13  
     8 Cerean        12  
     9 Gungan        11.7
    10 Human         11.3
    # ℹ 28 more rows
