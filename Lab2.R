# загрузка пакетов
library('data.table')          # работаем с объектами "таблица данных"
library('moments')             # коэффициенты асимметрии и эксцесса 
library('lattice')
library('ggplot2')
library('dplyr')

# загружаем файл с данными по импорту масла в РФ (из прошлой практики)
fileURL <- 'https://raw.githubusercontent.com/aksyuk/R-data/master/COMTRADE/040510-Imp-RF-comtrade.csv'
# создаём директорию для данных, если она ещё не существует:
if (!file.exists('./data')) {
  dir.create('./data')
}
# создаём файл с логом загрузок, если он ещё не существует:
if (!file.exists('./data/download.log')) {
  file.create('./data/download.log')
}
# загружаем файл, если он ещё не существует,
#  и делаем запись о загрузке в лог:
if (!file.exists('./data/040510-Imp-RF-comtrade.csv')) {
  download.file(fileURL, './data/040510-Imp-RF-comtrade.csv')
  # сделать запись в лог
  write(paste('Файл "040510-Imp-RF-comtrade.csv" загружен', Sys.time()), 
        file = './data/download.log', append = T)
}
# читаем данные из загруженного .csv во фрейм, если он ещё не существует
if (!exists('DT')){
  DT.import <- data.table(read.csv('./data/040510-Imp-RF-comtrade.csv', 
                                   stringsAsFactors = F))
}
# предварительный просмотр
dim(DT.import)            # размерность таблицы
str(DT.import)            # структура (характеристики столбцов)
DT.import          # удобный просмотр объекта data.table

# сколько NA в каждом из оставшихся столбцов?
na.num <- apply(DT.import, 2, function(x) length(which(is.na(x)))) 
# выводим только положительные и по убыванию
sort(na.num[na.num > 0], decreasing = T)

# графики плотности распределения массы поставки по годам
densityplot(~ Netweight.kg | as.factor(Year), data = DT.import,
            ylim = c(-0.5e-05, 8.5e-05),
            main = 'Распределение Netweight.kg по годам',
            xlab = 'Масса поставки, кг',
            ylab = 'Плотность распределения') # Рис. 19


# явное преобразование типа, чтобы избежать проблем 
DT.import[, Netweight.kg := as.double(Netweight.kg)]
# считаем среднее округляем до целого, как исходные данные
DT.import[, round(mean(.SD$Netweight.kg, na.rm = T), 0),
          by = Year]



# сначала копируем все значения
DT.import[, Netweight.kg.mean := round(mean(.SD$Netweight.kg,
                                                na.rm = T), 0),
          by = Year]

# затем заменяем пропуски на медианы
DT.import[!is.na(Netweight.kg), Netweight.kg.mean := Netweight.kg]

# смотрим результат
DT.import[, Netweight.kg, Netweight.kg.mean]
DT.import[is.na(Netweight.kg), Year, Netweight.kg.mean]

str(DT.import)

# смотрим, что изменилось
densityplot( ~ Netweight.kg.mean | as.factor(Year),
             data = DT.import,
             ylim = c(-0.5e-05, 8.5e-05),
             main = 'Распределение массы поставки по годам, Netweight.kg.median',
             xlab = 'Масса поставки, кг',
             ylab = 'Плотность распределения')

### Добавляем новый столбец

mutate(DT.import,
       month = Period - Year*100)

### Графики
# переменная-фактор: тип коробки передач
DT.import[, period := factor(am, levels = c(0, 1), 
                                    labels = c('автоматическая',
                                               'ручная'))]
