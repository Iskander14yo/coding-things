library(readxl)
library(lubridate)
library(tidyverse)
library(knitr)
library(magrittr)
library(stringr)
library(kableExtra)
library(psych)
library(data.table)
# Прежде всего посчитаем годовую доходность для всех портфелей и индексов
# Создадим три датафрейма, 1 столбец - Пиф, 2 - год, 3 столбец - доходность за этот год

# Вычисляет годовую доходность (прирост в стоимости)
annual_return <- function(path, name){
  name = transform(readxl::read_xlsx(path), 
                   Дата = as.Date(Дата))
  print(head(name))
  i = 1 # будет отрываться
  j = 1 # будет стоять на месте
  vec = c()
  n = nrow(name)
  while ( i != n){
    while (year(name[j, 1]) == year(name[i, 1]) & i != n){
      i = i + 1
    }
    a.return = name[j, 2] / name[i - 1, 2] * 100
    vec = c(vec, a.return)
    j = i
  }
  return(vec - 100)
}

setwd(file.path('C:', 'Ранх учеба', 'НИР', "Данные", "Акции"))
t.eq = data.frame(Fund = 'alpha.eq', nominal.returns = annual_return('Альфа - Ликвидные акции (по 07.11).xlsx', 
                                                      alpha.eq))

t.eq = rbind(t.eq, data.frame(Fund = 'vtb.eq', nominal.returns = annual_return('ВТБ - Фонд Акций (по 07.11).xlsx', 
                                                                   vtb.eq)))
t.eq = rbind(t.eq, data.frame(Fund = 'promsv.eq', nominal.returns = annual_return('Промсвязь - Акции (по 07.11).xlsx', 
                                                                    promsv.eq)))
t.eq = rbind(t.eq, data.frame(Fund = 'raif.eq', nominal.returns = annual_return('Райффайзен - Акции (по 07.11).xlsx', 
                                                                    raif.eq)))
t.eq = rbind(t.eq, data.frame(Fund = 'sber.eq', nominal.returns = annual_return('Сбербанк - Добрыня Никитич ( по 07.11).xlsx', 
                                                                  sber.eq)))


setwd(file.path('C:', 'Ранх учеба', 'НИР', "Данные", "Облигации"))
t.bonds = data.frame(Fund = 'alpha.b', nominal.returns = annual_return('Альфа - Капитал (по 07.11).xlsx', 
                                                                alpha.b))
t.bonds = rbind(t.bonds, data.frame(Fund = 'vtb.b', 
                                   nominal.returns = annual_return('ВТБ - Фонд Казначеский (по 07.11).xlsx', 
                                                                          vtb.b)))
t.bonds = rbind(t.bonds, data.frame(Fund = 'raif.b', 
                                    nominal.returns = annual_return('Райффайзен - Облигации (по 07.11).xlsx', 
                                                            raif.b)))
t.bonds = rbind(t.bonds, data.frame(Fund = 'sber_m.b', 
                                    nominal.returns = annual_return('Сбербанк - Илья Муромец (по 07.11).xlsx', 
                                                            sber_m.b)))
t.bonds = rbind(t.bonds, data.frame(Fund = 'sber_p.b', 
                          nominal.returns = annual_return('Сбербанк - Фонд перспективных облигаций (по 07.11).xlsx', 
                                                            sber_p.b)))


setwd(file.path('C:', 'Ранх учеба', 'НИР', "Данные", "Смешанные"))
t.m = data.frame(Fund = 'alpha.m', nominal.returns = annual_return('Альфа - Баланс (по 07.11).xlsx', 
                                                           alpha.m))
t.m = rbind(t.m, data.frame(Fund = 'vtb.m', 
                                    nominal.returns = annual_return('ВТБ - Фонд Сбалансированный.xlsx', 
                                                            vtb.m)))
t.m = rbind(t.m, data.frame(Fund = 'sber.m', 
                            nominal.returns = annual_return('Сбербанк - Фонд Сбалансированный.xlsx', 
                                                    sber.m)))

counting <- function(dataset){
  i = 1
  j = 1
  k = 1
  y = c()
  n = nrow(dataset)
  while (j <= n){
    
    while (j <= n & dataset[i, 1] == dataset[j, 1]){
      y = c(y, 2020 - k + 1)
      j = j + 1
      k = k + 1
    }
    k = 1
    i = j
  }
  return(data.frame(years = y, dataset))
} # проставляет года
t.eq = counting(t.eq)
t.bonds = counting(t.bonds)
t.m = counting(t.m)
rm('counting')
rm('annual_return')
# # # # 
inflation = rev(c(111.03, 184.43, 136.53, 120.18, 118.58, 115.06, 111.99, 111.73, 
                  110.92, 109.00, 111.87, 113.28, 108.80, 108.78, 106.10, 106.57, 
                  106.47, 111.35, 112.91, 105.39, 102.51, 104.26, 103.04, 102.402) - 100)
Names = rep(2020, 24)
for (i in 1:length(Names)){
  Names[i] = Names[i] - i + 1
}
names(inflation) = as.character(Names); rm(list = c('i', 'Names'))


real_annual_return <- function(dataset1){
  n.mat = dataset1 %>%
    group_by(Fund) %>%
      summarize(n = n())
  print(n.mat)
  i = 1 # будет оставаться на месте
  j = 1 # будет двигаться
  k = 1
  vec = c()
  n = nrow(dataset1)
  while (j <= n){
    row = which(n.mat$Fund == dataset1[i, 2])
    number = as.numeric(n.mat[row,2])
    print(number)
    inflation1 = inflation[1:number]
    while (dataset1[i, 'Fund'] == dataset1[j, 'Fund'] & j <= n){
      real_return = ((1 + dataset1[j, 'nominal.returns']/100) / (1 + inflation1[k]/100) - 1) * 100
      vec = c(vec, real_return)
      j = j + 1
      k = k +1
    }
    i = j
    k = 1
  }
  return(vec)
}


t.eq.real_returns = data.frame(t.eq, real.returns = real_annual_return(t.eq))
t.b.real_returns = data.frame(t.bonds, real.returns = real_annual_return(t.bonds))
t.m.real_returns = data.frame(t.m, real.returns = real_annual_return(t.m))
t.eq = t.eq.real_returns
t.b = t.b.real_returns
t.m = t.m.real_returns
rm('t.eq.real_returns', 't.b.real_returns', 't.m.real_returns', 't.bonds')



# # # # # 
# РАБОТА С ИНДЕКСАМИ
# открытие файла 
setwd(file.path('C:', 'Ранх учеба', "НИР", "Данные", "Индексы"))
RGBI = read_excel('RGBI TR.xlsx')
names(RGBI) = c('Дата', "Цена")

IMOEX = read.csv2('IMOEX (1).csv', header = T, skip = 2)
IMOEX[, 'begin'] <- NULL
IMOEX[, 'end'] %<>%
  unlist() %>%
  as.Date(format = '%Y-%m-%d')
names(IMOEX)[7] = 'Date'
IMOEX[, 'open'] %<>%
  unlist() %>%
  as.numeric()
IMOEX %<>% 
  as_tibble()
# Завершили чтение файлов RGBI и IMOEX
# Получим номинальную доходность для индексов
annual_return_for_IMOEX <- function(dataset){
  dataset = as.data.frame(dataset)
  i = 1 # будет оставаться на месте
  j = 1 # будет итератором
  vec = c()
  n = nrow(dataset)
  while (j <= n){
    while (year(dataset[i, 'Date']) == year(dataset[j, 'Date']) & j <= n){
      j = j + 1 
    }
    annual_return = dataset[i, 'open'] / dataset[j - 1, 'open'] * 100
    vec = c(vec, annual_return)
    i = j
  }
  return(vec - 100)
}
IMOEX.real_returns = data.frame(year = 2020:2005, 
                                annual.nominal.return = annual_return_for_IMOEX(IMOEX)[-(17:21)])
rm('annual_return_for_IMOEX')

annual_return_for_RGBI = function(dataset){
  dataset = as.data.frame(dataset)
  i = 1 # будет оставаться на месте
  j = 1 # будет итератором
  vec = c()
  n = nrow(dataset)
  while (j <= n){
    while (year(dataset[i, 'Дата']) == year(dataset[j, 'Дата']) & j <= n){
      j = j + 1 
    }
    annual_return = dataset[j - 1, 'Цена'] / dataset[i, 'Цена'] * 100
    vec = c(vec, annual_return)
    i = j
  }
  return(vec - 100)
}
RGBI.real_returns = data.frame(year = 2020:2005, 
                               annual.nominal.return = annual_return_for_RGBI(RGBI)[-17]) # чтобы не брать 21 год
rm('annual_return_for_RGBI')

vec = c()
for (i in 1:nrow(RGBI.real_returns)){
  vec = c(vec, (IMOEX.real_returns[i, 2] + RGBI.real_returns[i, 2]) / 2)
}
mixed_index = data.frame(year = 2020:2005, 
                         annual.nominal.return = vec)
rm('vec', 'i')



real_annual_return_for_indexes <- function(dataset){
  inflation = inflation[1: nrow(dataset)]
  vec = c()
  for (i in 1:nrow(dataset)){
    value = (( 1 + dataset[i, 'annual.nominal.return']/100 ) / ( 1 + inflation[i]/100 ) - 1) * 100
    vec = c(vec, value)
  }
  return(vec)
}
IMOEX = data.frame(IMOEX.real_returns, 
                   annual.real.return = real_annual_return_for_indexes(IMOEX.real_returns))
RGBI = data.frame(RGBI.real_returns, 
                  annual.real.return = real_annual_return_for_indexes(RGBI.real_returns))
mixed_index = data.frame(mixed_index, 
                         annual.real.return = real_annual_return_for_indexes(mixed_index))
rm('IMOEX.real_returns', 'RGBI.real_returns', 'real_annual_return_for_indexes')

# # # # # # # # # # # # # 
names(t.eq) = c('year', 'Fund', 'nominal.returns', 'annual growth rate of real returns')
names(t.b) = c('year', 'Fund', 'nominal.returns', 'annual growth rate of real returns')
names(t.m) = c('year', 'Fund', 'nominal.returns', 'annual growth rate of real returns')
t.eq = data.frame(t.eq, 'real returns' = real_annual_return(t.eq) + 100)
t.b = data.frame(t.b, 'real returns' = real_annual_return(t.b) + 100)
t.m = data.frame(t.m, 'real returns' = real_annual_return(t.m) + 100)
rm('real_annual_return')






# # # # # # # # # 

# отобразим (в виде хистограммы, например) ежегодную доходность сначала для фондов, затем для индексов.
# датасеты, содержащие данные о средней доходности за год по каждому классу активов
t1.eq = t.eq %>%
  group_by(year) %>%
    summarize(mean = mean(annual.growth.rate.of.real.returns))
t1.b = t.b %>%
  group_by(year) %>%
    summarize(mean = mean(annual.growth.rate.of.real.returns))
t1.m = t.m %>%
  group_by(year) %>%
    summarize(mean = mean(`annual.growth.rate.of.real.returns`))

df = data.frame(name = c(rep('eq', 20), rep('bonds', 20), rep('mixed', 20)), 
                year = t1.eq$year[-21], mean = c(t1.eq$mean[-21], t1.b$mean[-21], t1.m$mean))
df %>%
  group_by(name) %>%
    ggplot(data = ., aes(x = year, y = mean, fill = name)) + 
    geom_bar(position = position_dodge2(width = 0.5, padding = 0.01), stat = 'identity') + 
    geom_vline(xintercept = 2000:2019, color = 'grey', alpha = 0.7) + 
    xlab('years') + ylab('arithmetic mean per year')

df1 = data.frame(name = c(rep('eq', 16), rep('bonds', 16), rep('mixed', 16)),
                 year = RGBI$year, mean = c(IMOEX$annual.real.return, RGBI$annual.real.return, mixed_index$annual.real.return))
df1 %>%
  group_by(name) %>%
   ggplot(data = ., aes(x = year, y = mean, fill = name)) + 
    geom_bar(position = position_dodge2(width = 0.9, padding = 0.001, preserve = 'total'), stat = 'identity') + 
    geom_vline(xintercept = 2005:2019, color = 'grey') + 
    xlab('years') + ylab('arithmetic mean per year')

# # Худшие года для каждого класса активов
worst.eq = t1.eq %>%
  arrange(mean) %>%
   head(5) %>%
    ungroup()
worst.eq$year = as.character(worst.eq$year)
ggplot() +
  geom_bar(data = worst.eq, aes(x = reorder(year, -mean), y = mean, fill = 'red'), stat = 'identity') + 
  theme(
    axis.text = element_text(size = 14, color = "black")
  ) + ggtitle(label = 'Equity Funds') + labs(x = 'year') + theme(legend.position="none")

worst.b = t1.b %>%
  arrange(mean) %>%
    head(5)
worst.b$year = as.character(worst.b$year)
ggplot() +
  geom_bar(data = worst.b, aes(x = reorder(year, -mean), y = mean, fill = 'red'), stat = 'identity') + 
  theme(
    axis.text = element_text(size = 14, color = "black")
  ) + ggtitle(label = 'Bonds Funds') + labs(x = 'year') + theme(legend.position="none")

worst.m = t1.m %>%
  arrange(mean) %>%
    head(5)
worst.m$year = as.character(worst.m$year)
ggplot() +
  geom_bar(data = worst.m, aes(x = reorder(year, -mean), y = mean, fill = 'red'), stat = 'identity')  + 
  theme(
    axis.text = element_text(size = 14, color = "black")
  ) + ggtitle(label = 'Mixed Funds') + labs(x = 'year') + theme(legend.position="none")
# # # # #   

chain_index = function(vector){
  chain.vector = (vector + 100)/100
  vec = c(1 + vector[1]/100)
  for (i in 2L:length(vector)){
    value = prod(chain.vector[1:i])
    vec = c(vec, value)
  }
  return(unlist(vec))
}

t1.eq = data.frame(t1.eq, chain.index = chain_index(t1.eq$mean))
t1.b = data.frame(t1.b, chain.index = chain_index(t1.b$mean))
t1.m = data.frame(t1.m, chain.index = chain_index(t1.m$mean))

IMOEX = data.frame(IMOEX, chain.index = rev(chain_index(rev(IMOEX$annual.real.return))))
RGBI = data.frame(RGBI, chain.index = rev(chain_index(rev(RGBI$annual.real.return))))
mixed_index = data.frame(mixed_index, chain.index = rev(chain_index(rev(mixed_index$annual.real.return))))
# # # #  Рассчитать цепную доходность для индексов
# посчитать таблицу для каждого класса активов
# построить графики с фондами и бенчмарками

f <- function(vector){
  value = vector[length(vector)]
  return( (value ** (1/length(vector)) - 1) * 100 )
}
# считает геометрическое среднее

f1 = function(sd, len = 20){
  return( sd / sqrt(len))
}
# считает стандартную ошибку

f2 = function(dataset, arg){
  if (arg == 'min') value = min(dataset[, 2, drop = T])
  if (arg == 'max') value = max(dataset[, 2, drop = T])
  row = which(dataset[, 2] == value)
  return(dataset[row, 1])
}
# считает лучший и худший год

performance = data.frame(asset = c('equities', 'bonds', 'mixed'), 
                         arithmetic.mean = c(mean(t1.eq$mean), mean(t1.b$mean), mean(t1.m$mean)),
                         geometric.mean = c(f(t1.eq$chain.index), f(t1.b$chain.index), f(t1.m$chain.index)),
                         standard.deviation = c(sd(t1.eq$mean), sd(t1.b$mean), sd(t1.m$mean)),
                         standard.error = c(f1(sd(t1.eq$mean)), f1(sd(t1.b$mean)), f1(sd(t1.m$mean))),
                         minimum.return = c(min(t1.eq$mean), min(t1.b$mean), min(t1.m$mean)),
                         minimum.year = c(f2(t1.eq, 'min'), f2(t1.b, 'min'), f2(t1.m, 'min')),
                         maximum.return = c(max(t1.eq$mean), max(t1.b$mean), max(t1.m$mean)),
                         maximum.year = c(f2(t1.eq, 'max'), f2(t1.b, 'max'), f2(t1.m, 'max')))

f3 = function(dataset, arg){
  if (arg == 'min')  {d = dataset %>%
    group_by(Fund) %>%
      summarize(min = min(annual.growth.rate.of.real.returns))}
  else d = dataset %>%
    group_by(Fund) %>%
    summarize(min = max(annual.growth.rate.of.real.returns))
  vector = dataset[, 1, drop = T]
  vec = c()
  for (i in 1:nrow(d)){
    row = which(dataset[, 4] == as.numeric(d[i, 2])) 
    vec = c(vec, vector[row])
  }
  return(vec)
}
  
p1 = t.eq %>%
  group_by(Fund) %>%
    summarize(arithmetic.mean = mean(annual.growth.rate.of.real.returns),
              geometric.mean = NA,
              standard.deviation = sd(annual.growth.rate.of.real.returns),
              standard.error = f1(sd(annual.growth.rate.of.real.returns)),
              minimum.return = min(annual.growth.rate.of.real.returns),
              maximum.return = max(annual.growth.rate.of.real.returns)) %>%
  mutate(minimum.year = f3(t.eq, arg = 'min'), .before = maximum.return) %>%
  mutate(maximum.year = f3(t.eq, arg = 'max')) %>%
  rename('asset' = Fund)

p = full_join(x = performance, y = p1, by = NULL)
# # 

p2 = t.b %>%
  group_by(Fund) %>%
  summarize(arithmetic.mean = mean(annual.growth.rate.of.real.returns),
            geometric.mean = NA,
            standard.deviation = sd(annual.growth.rate.of.real.returns),
            standard.error = f1(sd(annual.growth.rate.of.real.returns)),
            minimum.return = min(annual.growth.rate.of.real.returns),
            maximum.return = max(annual.growth.rate.of.real.returns)) %>%
  mutate(minimum.year = f3(t.b, arg = 'min'), .before = maximum.return) %>%
  mutate(maximum.year = f3(t.b, arg = 'max')) %>%
  rename('asset' = Fund)

p = full_join(x = p, y = p2, by = NULL)
# # 

p3 = t.m %>%
    group_by(Fund) %>%
    summarize(arithmetic.mean = mean(annual.growth.rate.of.real.returns),
              geometric.mean = NA,
              standard.deviation = sd(annual.growth.rate.of.real.returns),
              standard.error = f1(sd(annual.growth.rate.of.real.returns)),
              minimum.return = min(annual.growth.rate.of.real.returns),
              maximum.return = max(annual.growth.rate.of.real.returns)) %>%
    mutate(minimum.year = f3(t.m, arg = 'min'), .before = maximum.return) %>%
    mutate(maximum.year = f3(t.m, arg = 'max')) %>%
    rename('asset' = Fund)

p = full_join(x = p, y = p3, by = NULL)
p %>% kbl(caption = 'Performance') %>%
  kable_classic(full_width = T, html_font = "Cambria") %>%
  row_spec(1:3, bold = T)

  


# # # # # 
# Доходность пифов против индексов
ggplot() +
  geom_line(data = t1.eq, aes(x = year, y = chain.index*100, color = 'blue')) + 
  geom_line(data = IMOEX, aes(x = year, y = chain.index*100, color = 'orange')) +
  xlab("year") + ylab('Real return') + 
  theme(
    axis.text = element_text(size = 12, color = "black")
  ) + ggtitle(label = 'Equities') + 
  scale_color_manual(name = "", values = c("blue","orange"), labels = c('Funds', 'Index'))

ggplot() +
  geom_line(data = t1.b, aes(x = year, y = chain.index*100, color = 'blue')) + 
  geom_line(data = RGBI, aes(x = year, y = chain.index*100, color = 'orange')) +
  xlab("year") + ylab('Real return') +
  xlab("year") + ylab('Real return') + 
  theme(
    axis.text = element_text(size = 12, color = "black")
  ) + ggtitle(label = 'Bonds') + 
  scale_color_manual(name = "", values = c("blue","orange"), labels = c('Funds', 'Index'))

ggplot() +
  geom_line(data = t1.m, aes(x = year, y = chain.index*100, color = 'blue')) + 
  geom_line(data = mixed_index, aes(x = year, y = chain.index*100, color = 'orange')) +
  xlab("year") + ylab('Real return') + 
  theme(
    axis.text = element_text(size = 12, color = "black")
  ) + ggtitle(label = 'Mixed funds') + 
  scale_color_manual(name = "", values = c("blue","orange"), labels = c('Funds', 'Index'))

