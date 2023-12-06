# Анализ логов с помощью языка R
Тыван Максим Владимирович

# Цель работы

1.  Получить знания о методах исследования радиоэлектронной обстановки.
2.  Составить представление о механизмах работы Wi-Fi сетей на канальном
    и сетевом уровне модели OSI.
3.  Закрепить практические навыки использования языка программирования R
    для обработки данных
4.  Закрепить знания основных функций обработки данных экосистемы
    tidyverse языка R

# Ход работы

Импорт данных и библиотек

``` r
library(dplyr)
```


    Присоединяю пакет: 'dplyr'

    Следующие объекты скрыты от 'package:stats':

        filter, lag

    Следующие объекты скрыты от 'package:base':

        intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ forcats   1.0.0     ✔ readr     2.1.4
    ✔ ggplot2   3.4.4     ✔ stringr   1.5.0
    ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ✔ purrr     1.0.2     ✔ tidyr     1.3.0

    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
mir1 = read.csv("mir_csv-01.csv", nrows = 167)
mir2 = read.csv("mir_csv-01.csv", skip = 169)
```

Привидение данных в аккуратный вид

``` r
mir1$Last.time.seen <- as.POSIXct(mir1$Last.time.seen, format = "%Y-%m-%d %H:%M:%S")
mir1$First.time.seen <- as.POSIXct(mir1$First.time.seen, format = "%Y-%m-%d %H:%M:%S")
mir2$Last.time.seen <- as.POSIXct(mir2$Last.time.seen, format = "%Y-%m-%d %H:%M:%S")
mir2$First.time.seen <- as.POSIXct(mir2$First.time.seen, format = "%Y-%m-%d %H:%M:%S")
mir2 <- mir2 %>% mutate_at(vars(Station.MAC, BSSID, Probed.ESSIDs), trimws) %>% mutate_at(vars(Station.MAC, BSSID, Probed.ESSIDs), na_if, "")

mir2 %>% head
```

            Station.MAC     First.time.seen      Last.time.seen Power X..packets
    1 CA:66:3B:8F:56:DD 2023-07-28 09:13:03 2023-07-28 10:59:44   -33        858
    2 96:35:2D:3D:85:E6 2023-07-28 09:13:03 2023-07-28 09:13:03   -65          4
    3 5C:3A:45:9E:1A:7B 2023-07-28 09:13:03 2023-07-28 11:51:54   -39        432
    4 C0:E4:34:D8:E7:E5 2023-07-28 09:13:03 2023-07-28 11:53:16   -61        958
    5 5E:8E:A6:5E:34:81 2023-07-28 09:13:04 2023-07-28 09:13:04   -53          1
    6 10:51:07:CB:33:E7 2023-07-28 09:13:05 2023-07-28 11:56:06   -43        344
                  BSSID Probed.ESSIDs
    1 BE:F1:71:D5:17:8B  C322U13 3965
    2  (not associated)  IT2 Wireless
    3 BE:F1:71:D6:10:D7  C322U21 0566
    4 BE:F1:71:D5:17:8B  C322U13 3965
    5  (not associated)          <NA>
    6  (not associated)          <NA>

Просмотр общей структуры данных

``` r
mir1 %>% glimpse()
```

    Rows: 167
    Columns: 15
    $ BSSID           <chr> "BE:F1:71:D5:17:8B", "6E:C7:EC:16:DA:1A", "9A:75:A8:B9…
    $ First.time.seen <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28 …
    $ Last.time.seen  <dttm> 2023-07-28 11:50:50, 2023-07-28 11:55:12, 2023-07-28 …
    $ channel         <int> 1, 1, 1, 7, 6, 6, 11, 11, 11, 1, 6, 14, 11, 11, 6, 6, …
    $ Speed           <int> 195, 130, 360, 360, 130, 130, 195, 130, 130, 195, 180,…
    $ Privacy         <chr> " WPA2", " WPA2", " WPA2", " WPA2", " WPA2", " OPN", "…
    $ Cipher          <chr> " CCMP", " CCMP", " CCMP", " CCMP", " CCMP", " ", " CC…
    $ Authentication  <chr> " PSK", " PSK", " PSK", " PSK", " PSK", "   ", " PSK",…
    $ Power           <int> -30, -30, -68, -37, -57, -63, -27, -38, -38, -66, -42,…
    $ X..beacons      <int> 846, 750, 694, 510, 647, 251, 1647, 1251, 704, 617, 13…
    $ X..IV           <int> 504, 116, 26, 21, 6, 3430, 80, 11, 0, 0, 86, 0, 0, 0, …
    $ LAN.IP          <chr> "   0.  0.  0.  0", "   0.  0.  0.  0", "   0.  0.  0.…
    $ ID.length       <int> 12, 4, 2, 14, 25, 13, 12, 13, 24, 12, 10, 0, 24, 24, 1…
    $ ESSID           <chr> " C322U13 3965", " Cnet", " KC", " POCO X5 Pro 5G", " …
    $ Key             <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…

``` r
mir2 %>% glimpse()
```

    Rows: 12,269
    Columns: 7
    $ Station.MAC     <chr> "CA:66:3B:8F:56:DD", "96:35:2D:3D:85:E6", "5C:3A:45:9E…
    $ First.time.seen <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28 …
    $ Last.time.seen  <dttm> 2023-07-28 10:59:44, 2023-07-28 09:13:03, 2023-07-28 …
    $ Power           <chr> " -33", " -65", " -39", " -61", " -53", " -43", " -31"…
    $ X..packets      <chr> "      858", "        4", "      432", "      958", " …
    $ BSSID           <chr> "BE:F1:71:D5:17:8B", "(not associated)", "BE:F1:71:D6:…
    $ Probed.ESSIDs   <chr> "C322U13 3965", "IT2 Wireless", "C322U21 0566", "C322U…

## Анализ

### Точки доступа

Определить небезопасные точки доступа (без шифрования – OPN)

``` r
openwifi <- mir1 %>% filter(grepl("OPN", Privacy)) %>% select(BSSID, ESSID) %>% arrange(BSSID) %>% distinct
openwifi
```

                   BSSID          ESSID
    1  00:03:7A:1A:03:56        MT_FREE
    2  00:03:7F:12:34:56        MT_FREE
    3  00:25:00:FF:94:73               
    4  00:26:99:F2:7A:E0               
    5  00:26:99:F2:7A:EF               
    6  00:3E:1A:5D:14:45        MT_FREE
    7  00:53:7A:99:98:56        MT_FREE
    8  00:AB:0A:00:10:10               
    9  02:67:F1:B0:6C:98        MT_FREE
    10 02:BC:15:7E:D5:DC        MT_FREE
    11 02:CF:8B:87:B4:F9        MT_FREE
    12 E0:D9:E3:48:FF:D2               
    13 E0:D9:E3:49:00:B1               
    14 E8:28:C1:DC:0B:B2               
    15 E8:28:C1:DC:33:12               
    16 E8:28:C1:DC:B2:40  MIREA_HOTSPOT
    17 E8:28:C1:DC:B2:41   MIREA_GUESTS
    18 E8:28:C1:DC:B2:42               
    19 E8:28:C1:DC:B2:50   MIREA_GUESTS
    20 E8:28:C1:DC:B2:51               
    21 E8:28:C1:DC:B2:52  MIREA_HOTSPOT
    22 E8:28:C1:DC:BD:50   MIREA_GUESTS
    23 E8:28:C1:DC:BD:52  MIREA_HOTSPOT
    24 E8:28:C1:DC:C6:B0   MIREA_GUESTS
    25 E8:28:C1:DC:C6:B1               
    26 E8:28:C1:DC:C6:B2               
    27 E8:28:C1:DC:C8:30   MIREA_GUESTS
    28 E8:28:C1:DC:C8:31               
    29 E8:28:C1:DC:C8:32  MIREA_HOTSPOT
    30 E8:28:C1:DC:FF:F2               
    31 E8:28:C1:DD:04:40  MIREA_HOTSPOT
    32 E8:28:C1:DD:04:41   MIREA_GUESTS
    33 E8:28:C1:DD:04:42               
    34 E8:28:C1:DD:04:50   MIREA_GUESTS
    35 E8:28:C1:DD:04:51               
    36 E8:28:C1:DD:04:52  MIREA_HOTSPOT
    37 E8:28:C1:DE:47:D0   MIREA_GUESTS
    38 E8:28:C1:DE:47:D1               
    39 E8:28:C1:DE:47:D2  MIREA_HOTSPOT
    40 E8:28:C1:DE:74:30               
    41 E8:28:C1:DE:74:31               
    42 E8:28:C1:DE:74:32  MIREA_HOTSPOT

Определить производителя для каждого обнаруженного устройства

00:03:7A Taiyo Yuden Co., Ltd. 00:03:7F Atheros Communications, Inc.
00:25:00 Apple, Inc. 00:26:99 Cisco Systems, Inc E0:D9:E3 Eltex
Enterprise Ltd. E8:28:C1 Eltex Enterprise Ltd.

Выявить устройства, использующие последнюю версию протокола шифрования
WPA3, и названия точек доступа, реализованных на этих устройствах

``` r
mir1 %>% filter(grepl("WPA3", Privacy)) %>% select(BSSID, ESSID, Privacy)
```

                  BSSID               ESSID    Privacy
    1 26:20:53:0C:98:E8                      WPA3 WPA2
    2 A2:FE:FF:B8:9B:C9          Christie’s  WPA3 WPA2
    3 96:FF:FC:91:EF:64                      WPA3 WPA2
    4 CE:48:E7:86:4E:33  iPhone (Анастасия)  WPA3 WPA2
    5 8E:1F:94:96:DA:FD  iPhone (Анастасия)  WPA3 WPA2
    6 BE:FD:EF:18:92:44            Димасик   WPA3 WPA2
    7 3A:DA:00:F9:0C:02   iPhone XS Max 🦊🐱🦊  WPA3 WPA2
    8 76:C5:A0:70:08:96                      WPA3 WPA2

Отсортировать точки доступа по интервалу времени, в течение которого они
находились на связи, по убыванию

``` r
mir1int <- mir1 %>% mutate(Time.Interval = Last.time.seen - First.time.seen)

mir1int %>% arrange(desc(Time.Interval)) %>% mutate(Time.Interval = seconds_to_period(Time.Interval)) %>% select(BSSID, First.time.seen, Last.time.seen, Time.Interval) %>% head
```

                  BSSID     First.time.seen      Last.time.seen Time.Interval
    1 00:25:00:FF:94:73 2023-07-28 09:13:06 2023-07-28 11:56:21    2H 43M 15S
    2 E8:28:C1:DD:04:52 2023-07-28 09:13:09 2023-07-28 11:56:05    2H 42M 56S
    3 E8:28:C1:DC:B2:52 2023-07-28 09:13:03 2023-07-28 11:55:38    2H 42M 35S
    4 08:3A:2F:56:35:FE 2023-07-28 09:13:27 2023-07-28 11:55:53    2H 42M 26S
    5 6E:C7:EC:16:DA:1A 2023-07-28 09:13:03 2023-07-28 11:55:12     2H 42M 9S
    6 E8:28:C1:DC:B2:50 2023-07-28 09:13:06 2023-07-28 11:55:12     2H 42M 6S

Обнаружить топ-10 самых быстрых точек доступа

``` r
spottop <- mir1 %>% arrange(desc(Speed)) %>% select(BSSID, ESSID, Speed, Privacy) %>% head(10)
spottop
```

                   BSSID               ESSID Speed    Privacy
    1  26:20:53:0C:98:E8                       866  WPA3 WPA2
    2  96:FF:FC:91:EF:64                       866  WPA3 WPA2
    3  CE:48:E7:86:4E:33  iPhone (Анастасия)   866  WPA3 WPA2
    4  8E:1F:94:96:DA:FD  iPhone (Анастасия)   866  WPA3 WPA2
    5  9A:75:A8:B9:04:1E                  KC   360       WPA2
    6  4A:EC:1E:DB:BF:95      POCO X5 Pro 5G   360       WPA2
    7  56:C5:2B:9F:84:90          OnePlus 6T   360       WPA2
    8  E8:28:C1:DC:B2:41        MIREA_GUESTS   360        OPN
    9  E8:28:C1:DC:B2:40       MIREA_HOTSPOT   360        OPN
    10 E8:28:C1:DC:B2:42                       360        OPN

Отсортировать точки доступа по частоте отправки запросов

``` r
mir1req <- mir1int %>% mutate(beacon_rate = as.double(X..beacons) / as.integer(Time.Interval))

mir1req %>% select(BSSID, ESSID, Privacy, X..beacons, Time.Interval, beacon_rate) %>% filter(!is.infinite(beacon_rate)) %>% arrange(desc(beacon_rate)) %>% head
```

                  BSSID                ESSID    Privacy X..beacons Time.Interval
    1 F2:30:AB:E9:03:ED      iPhone (Uliana)       WPA2          6        7 secs
    2 B2:CF:C0:00:4A:60  Михаил's Galaxy M32       WPA2          4        5 secs
    3 3A:DA:00:F9:0C:02    iPhone XS Max 🦊🐱🦊  WPA3 WPA2          5        9 secs
    4 02:BC:15:7E:D5:DC              MT_FREE        OPN          1        2 secs
    5 00:3E:1A:5D:14:45              MT_FREE        OPN          1        2 secs
    6 76:C5:A0:70:08:96                       WPA3 WPA2          1        2 secs
      beacon_rate
    1   0.8571429
    2   0.8000000
    3   0.5555556
    4   0.5000000
    5   0.5000000
    6   0.5000000

### Данные клиентов

Определить производителя для каждого обнаруженного устройства

``` r
mir2 %>% filter(grepl("(..:..:..:)(..:..:..)", BSSID)) %>% distinct(BSSID)
```

                   BSSID
    1  BE:F1:71:D5:17:8B
    2  BE:F1:71:D6:10:D7
    3  1E:93:E3:1B:3C:F4
    4  E8:28:C1:DC:FF:F2
    5  00:25:00:FF:94:73
    6  00:26:99:F2:7A:E2
    7  0C:80:63:A9:6E:EE
    8  E8:28:C1:DD:04:52
    9  0A:C5:E1:DB:17:7B
    10 9A:75:A8:B9:04:1E
    11 8A:A3:03:73:52:08
    12 4A:EC:1E:DB:BF:95
    13 BE:F1:71:D5:0E:53
    14 08:3A:2F:56:35:FE
    15 6E:C7:EC:16:DA:1A
    16 2A:E8:A2:02:01:73
    17 E8:28:C1:DC:B2:52
    18 E8:28:C1:DC:C6:B2
    19 E8:28:C1:DC:C8:32
    20 56:C5:2B:9F:84:90
    21 9A:9F:06:44:24:5B
    22 12:48:F9:CF:58:8E
    23 E8:28:C1:DD:04:50
    24 AA:F4:3F:EE:49:0B
    25 3A:70:96:C6:30:2C
    26 E8:28:C1:DC:3C:92
    27 8E:55:4A:85:5B:01
    28 5E:C7:C0:E4:D7:D4
    29 E2:37:BF:8F:6A:7B
    30 96:FF:FC:91:EF:64
    31 CE:B3:FF:84:45:FC
    32 00:26:99:BA:75:80
    33 76:70:AF:A4:D2:AF
    34 E8:28:C1:DC:B2:50
    35 00:AB:0A:00:10:10
    36 E8:28:C1:DC:C8:30
    37 8E:1F:94:96:DA:FD
    38 E8:28:C1:DB:F5:F2
    39 E8:28:C1:DD:04:40
    40 EA:7B:9B:D8:56:34
    41 BE:FD:EF:18:92:44
    42 7E:3A:10:A7:59:4E
    43 00:26:99:F2:7A:E1
    44 00:23:EB:E3:49:31
    45 E8:28:C1:DC:B2:40
    46 E0:D9:E3:49:04:40
    47 3A:DA:00:F9:0C:02
    48 E8:28:C1:DC:B2:41
    49 E8:28:C1:DE:74:32
    50 E8:28:C1:DC:33:12
    51 92:F5:7B:43:0B:69
    52 DC:09:4C:32:34:9B
    53 E8:28:C1:DC:F0:90
    54 E0:D9:E3:49:04:52
    55 22:C9:7F:A9:BA:9C
    56 E8:28:C1:DD:04:41
    57 92:12:38:E5:7E:1E
    58 B2:1B:0C:67:0A:BD
    59 E8:28:C1:DC:33:10
    60 E0:D9:E3:49:04:41
    61 1E:C2:8E:D8:30:91
    62 A2:64:E8:97:58:EE
    63 A6:02:B9:73:2F:76
    64 A6:02:B9:73:81:47
    65 AE:3E:7F:C8:BC:8E
    66 B6:C4:55:B5:53:24
    67 86:DF:BF:E4:2F:23
    68 02:67:F1:B0:6C:98
    69 36:46:53:81:12:A0
    70 E8:28:C1:DC:0B:B0
    71 82:CD:7D:04:17:3B
    72 E8:28:C1:DC:54:B2
    73 00:03:7F:10:17:56
    74 00:0D:97:6B:93:DF

00:03:7F Atheros Communications, Inc. 00:0D:97 Hitachi Energy USA Inc.
00:23:EB Cisco Systems, Inc 00:25:00 Apple, Inc. 00:26:99 Cisco Systems,
Inc 08:3A:2F Guangzhou Juan Intelligent Tech Joint Stock Co.,Ltd
0C:80:63 Tp-Link Technologies Co.,Ltd. DC:09:4C Huawei Technologies
Co.,Ltd E0:D9:E3 Eltex Enterprise Ltd. E8:28:C1 Eltex Enterprise Ltd.

Обнаружить устройства, которые НЕ рандомизируют свой MAC адрес

``` r
mir2 %>% filter(grepl("(..:..:..:)(..:..:..)", BSSID) & !is.na(Probed.ESSIDs)) %>% select(BSSID, Probed.ESSIDs) %>% group_by(BSSID, Probed.ESSIDs) %>% filter(n() > 1) %>% arrange(BSSID) %>% unique()
```

    # A tibble: 16 × 2
    # Groups:   BSSID, Probed.ESSIDs [16]
       BSSID             Probed.ESSIDs   
       <chr>             <chr>           
     1 00:26:99:BA:75:80 GIVC            
     2 00:26:99:F2:7A:E2 GIVC            
     3 0C:80:63:A9:6E:EE KOTIKI_XXX      
     4 1E:93:E3:1B:3C:F4 Galaxy A71      
     5 6E:C7:EC:16:DA:1A Cnet            
     6 8E:55:4A:85:5B:01 Vladimir        
     7 AA:F4:3F:EE:49:0B Redmi Note 8 Pro
     8 BE:F1:71:D5:17:8B C322U13 3965    
     9 E8:28:C1:DC:B2:50 MIREA_HOTSPOT   
    10 E8:28:C1:DC:B2:50 MIREA_GUESTS    
    11 E8:28:C1:DC:B2:52 MIREA_HOTSPOT   
    12 E8:28:C1:DC:C6:B2 MIREA_HOTSPOT   
    13 E8:28:C1:DC:F0:90 MIREA_GUESTS    
    14 E8:28:C1:DD:04:40 MIREA_HOTSPOT   
    15 E8:28:C1:DD:04:52 MIREA_HOTSPOT   
    16 E8:28:C1:DE:74:32 MIREA_HOTSPOT   

Кластеризовать запросы от устройств к точкам доступа по их именам.
Определить время появления устройства в зоне радиовидимости и время
выхода его из нее

``` r
mir2 %>% filter(Probed.ESSIDs != '<NA>') %>% group_by(Station.MAC, Probed.ESSIDs) %>%  summarise("FirstTimeCluster" = min(First.time.seen), "LastTimeClusteer" = max(Last.time.seen), Power)
```

    `summarise()` has grouped output by 'Station.MAC'. You can override using the
    `.groups` argument.

    # A tibble: 1,477 × 5
    # Groups:   Station.MAC [1,477]
       Station.MAC       Probed.ESSIDs FirstTimeCluster    LastTimeClusteer    Power
       <chr>             <chr>         <dttm>              <dttm>              <chr>
     1 00:90:4C:E6:54:54 Redmi         2023-07-28 09:16:59 2023-07-28 10:21:15 " -6…
     2 00:95:69:E7:7C:ED nvripcsuite   2023-07-28 09:13:11 2023-07-28 11:56:13 " -5…
     3 00:95:69:E7:7D:21 nvripcsuite   2023-07-28 09:13:15 2023-07-28 11:56:17 " -3…
     4 00:95:69:E7:7F:35 nvripcsuite   2023-07-28 09:13:11 2023-07-28 11:56:07 " -6…
     5 00:F4:8D:F7:C5:19 Redmi 12      2023-07-28 10:45:04 2023-07-28 11:43:26 " -7…
     6 02:00:00:00:00:00 xt3 w64dtgv5… 2023-07-28 09:54:40 2023-07-28 11:55:36 " -6…
     7 02:06:2B:A5:0C:31 Avenue611     2023-07-28 09:55:12 2023-07-28 09:55:12 " -6…
     8 02:1D:0F:A4:94:74 iPhone (Дима… 2023-07-28 09:57:08 2023-07-28 09:57:08 " -6…
     9 02:32:DC:56:5C:82 Timo Resort   2023-07-28 10:58:23 2023-07-28 10:58:24 " -8…
    10 02:35:E9:C2:44:5F iPhone (Макс… 2023-07-28 10:00:55 2023-07-28 10:00:55 " -8…
    # ℹ 1,467 more rows

Оценить стабильность уровня сигнала внури кластера во времени. Выявить
наиболее стабильный кластер. Для оценки стабильности оценить
математическое ожидание и среднеквадратичное отклонение для каждого
найденного кластера.

``` r
mir2 %>% filter(!is.na(Probed.ESSIDs),!is.na(Power) ) %>% group_by(Station.MAC) %>%  summarise("FirstTimeCluster" = min(First.time.seen), "LastTimeCluster" = max(Last.time.seen), Power) %>% arrange(desc(Power)) %>% head(1)
```

    # A tibble: 1 × 4
      Station.MAC       FirstTimeCluster    LastTimeCluster     Power 
      <chr>             <dttm>              <dttm>              <chr> 
    1 8A:45:77:F9:7F:F4 2023-07-28 10:00:55 2023-07-28 10:00:55 " -89"
