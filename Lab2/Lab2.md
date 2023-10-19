---
title: "Основы обработки данных с помощью R"
author: "Тыван Максим Владимирович"
output: html_document
---

# Цель работы

1.  Развить практические навыки использования языка программирования R для обработки данных
2.  Закрепить знания базовых типов данных языка R
3.  Развить пркатические навыки использования функций обработки данных пакета dplyr -- функции select(), filter(), mutate(), arrange(), group_by()

# Задание

Проанализировать встроенный в пакет dplyr набор данных starwars с помощью языка R и ответить на вопросы:

```{r}
library(dplyr)
```

1.Сколько строк в датафрейме?

```{r}
starwars %>% nrow()
```

2.  Сколько столбцов в датафрейме?

```{r}
starwars %>%  ncol()
```

3.  Как просмотреть примерный вид датафрейма?

```{r}
starwars %>% glimpse()
```

4.  Сколько уникальных рас персонажей (species) представлено в данных?

```{r}
length(unique(starwars$species))
```

5.  Найти самого высокого персонажа.

```{r}
arrange(starwars, desc(height), by_group=TRUE) %>% slice(1:1) %>% select(name:height)
```

6.  Найти всех персонажей ниже 170

```{r}
filter(starwars, height < 170) %>% pull(name)
```

7.  Подсчитать ИМТ (индекс массы тела) для всех персонажей. ИМТ подсчитать по формуле 𝐼 = 𝑚/ℎ\^2, где 𝑚 -- масса (weight), а ℎ -- рост (height).

```{r}
starwars %>% mutate(IMT = mass / height^2) %>% select(name,IMT)
```

8.  Найти 10 самых "вытянутых" персонажей. "Вытянутость" оценить по отношению массы (mass) к росту(height) персонажей.

```{r}
starwars %>% mutate(Вытянутость = mass/height) %>% arrange(desc(Вытянутость), by_group=TRUE) %>% select(name,Вытянутость)
```

9.  Найти средний возраст персонажей каждой расы вселенной Звездных войн.

```{r}
starwars %>% group_by(species) %>% filter(!is.na(birth_year)) %>% summarise(MidAge = mean(birth_year, na.rm = TRUE)) %>% select(species,MidAge) %>% arrange(desc(MidAge))
```

10. Найти самый распространенный цвет глаз персонажей вселенной Звездных войн.

```{r}
starwars %>% group_by(eye_color) %>% summarise(Количество = n()) %>% arrange(desc(Количество)) %>% slice(1:1)
```

11. Подсчитать среднюю длину имени в каждой расе вселенной Звездных войн.

```{r}
starwars %>% group_by(species) %>% mutate(NL = nchar(name)) %>% summarise(nameleng = mean(NL)) %>% select(species, nameleng) %>% arrange(desc(nameleng))
```
