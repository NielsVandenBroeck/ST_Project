# Read heart.csv data
data = read.csv(file="heart.csv", header=TRUE, dec=",", sep=";")
# Student number: 20203844
i = 8
j = 4
k = 4
# Rows to delete: 5,9,17,33,129
data = data[-c(k+1, j+1, i+1, j*k+1, i*j+1, i*k+1, i*j*k+1, i+j+k+1),]

# QUESTION 1:
los = data$los

dev.new()
par(mfrow=c(1, 3))

# Histogram
hist(los, main="Histogram van LOS", xlab="Lengte van Verblijf", ylab="Frequentie")

# Boxplot
boxplot(los, main="Boxplot van LOS", ylab="Lengte van Verblijf")

# QQ-plot
qqnorm(los, main="QQ-plot van LOS", xlab="Theoretische Kwantielen", ylab="Geobserveerde Kwantielen")
qqline(los, col="red")

# Some useful data
count_los = length(los)
min_los = min(los)
max_los = max(los)
mean_los = mean(los)
median_los = median(los)
var_los = var(los)
sd_los = sd(los)
iqr_los = IQR(los)

# Test if normal distributed
shapiro.test(los)

transormartion_inverse = los[los!=0]^-1
shapiro.test(transormartion_inverse)

transformation_sqrt = sqrt(los[los!=0])
shapiro.test(transformation_sqrt)

transormartion_cube = abs(los)^(1/3)
shapiro.test(transormartion_cube)

transormartion_log = log(los[los!=0])
shapiro.test(transormartion_log)


# QUESTION 2:
type_hartinfarct = data$mitype
ontslag_status = data$dstat

# Chi-kwadraat test
tabel_infarct_ontslag <- table(type_hartinfarct, ontslag_status)
test = chisq.test(tabel_infarct_ontslag)
test$observed
test$expected
test

# QUESTION 3:
age = data$age
bmi = data$bmi

dev.new()

par(mfrow=c(1, 3))
#Scatterplot
plot(age, bmi, main="Scatterplot leeftijd-BMI",
     xlab="leeftijd ", ylab="BMI")

# normality of  age
qqnorm(age, main="QQ-plot van leeftijd", xlab="Theoretische Kwantielen", ylab="Geobserveerde Kwantielen")
qqline(age, col="red")
shapiro.test(age)

# normality of BMI
qqnorm(bmi, main="QQ-plot van BMI", xlab="Theoretische Kwantielen", ylab="Geobserveerde Kwantielen")
qqline(bmi, col="red")
shapiro.test(bmi)

# spearman correlation test
cor.test(method="spearman", age, bmi)

# linear regression
model = lm(bmi ~ age)
summary(model)

