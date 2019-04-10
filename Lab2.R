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
            ylab = 'Плотность распределения')


# явное преобразование типа, чтобы избежать проблем 
DT.import[, Netweight.kg := as.double(Netweight.kg)]
# считаем среднее округляем до целого, как исходные данные
DT.import[, round(mean(.SD$Netweight.kg, na.rm = T), 0),
          by = Year]



# сначала копируем все значения
DT.import[, Netweight.kg.mean := round(mean(.SD$Netweight.kg,
                                                na.rm = T), 0),
          by = Year]

# затем заменяем пропуски на среднее
DT.import[!is.na(Netweight.kg), Netweight.kg.mean := Netweight.kg]

# смотрим результат
DT.import[, Netweight.kg, Netweight.kg.mean]
DT.import[is.na(Netweight.kg), Year, Netweight.kg.mean]

str(DT.import)

# смотрим, что изменилось
densityplot( ~ Netweight.kg.mean | as.factor(Year),
             data = DT.import,
             ylim = c(-0.5e-05, 8.5e-05),
             main = 'Распределение массы поставки по годам, Netweight.kg.mean',
             xlab = 'Масса поставки, кг',
             ylab = 'Плотность распределения')

### Добавляем новый столбец с номерами месяцев

DT.import$month = transmute(DT.import,
       month = Period - Year*100)
select(DT.import, Period, Year, month)

DT.import$month[DT.import$month < 9] = 0
DT.import$month[DT.import$month > 8] = 1
DT.import$month[DT.import$month == 0] = '1-8'
DT.import$month[DT.import$month == 1] = '9-12'
DT.import$month <- as.factor(DT.import$month)
str(DT.import)

### График lattice

png('Pic1.png', width = 500, height = 500)
densityplot( ~ Netweight.kg.mean | as.factor(Year),
             data = DT.import,
             ylim = c(-0.5e-05, 8.5e-05),
             groups = month,
             main = 'пакет lattice',
             xlab = 'Масса поставки, кг',
             ylab = 'Плотность распределения')
dev.off()

### График ggplot

png('Pic2.png', width = 500, height = 500)

gp <- ggplot(DT.import, aes(x = DT.import$Netweight.kg.mean, colour = DT.import$month)) + geom_density(alpha = .3)
gp <- gp + xlab('Масса поставки, кг')
gp <- gp + ylab('Плотность распределения')
gp

dev.off()

# График base

png('Pic3.png', width = 500, height = 500)

plot(density(DT.import$Netweight.kg.mean),
     xlab = 'Масса поставки, кг',
     ylab = 'Плотность распределения',
     main = 'пакет base'
)


dev.off()
