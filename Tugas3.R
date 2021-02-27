#library 
install.packages('tidyr')
install.packages('Hmisc')
install.packages('sqldf')
install.packages('ggplot2')
library(Hmisc)
library(sqldf)
library(dplyr)
library(ggplot2)
library(tidyr)

#import data
library(readr)
dataset3 <- read_csv("F:/Tugas3/dataset3.csv")
View(dataset3)

#-----------eksplorasi data-----------#
#struktur data 
str(dataset3)

#dimensi 
dim(dataset3)

#head (meihat 6 data teratas) dan tail (melihat 6 data terbawah)
head(dataset3)
tail(dataset3)

#summary -melihat ringkasan data 
summary(dataset)

#melakukan pendeskripsian data 
#:lebih lengkap outputnya drpd summary
describe(dataset3)

#distribution class
y <- dataset$hours.per.week
cbind(freq=table(y), percentage=prop.table(table(y))*100)

#distribution class
y <- dataset3$class.label
cbind(freq=table(y), percentage=prop.table(table(y))*100)

#Cari distinct value data movies
length(unique(dataset3))
length(unique(dataset$age))
length(unique(dataset3$workclass))
length(unique(dataset3$fnlwgt))
length(unique(dataset3$education))
length(unique(dataset3$education.num))
length(unique(dataset3$marital.status))
length(unique(dataset3$occupation))
length(unique(dataset3$relationship))
length(unique(dataset3$race))
length(unique(dataset3$sex))
length(unique(dataset3$capital.gain))
length(unique(dataset3$capital.loss))
length(unique(dataset3$hours.per.week))
length(unique(dataset3$cative.country))
length(unique(dataset3$class.label))

#-----------Visualisasi Data-----------#
ggplot(dataset, aes(x=class.label, y=sex)) +
  geom_col(stat="identity", fill="#009999") + theme_bw()
  xlab("Pendapatan") +
  ylab("Jenis Kelamin") +
  ggtitle("Pendapatan berdasarkan Jenis Kelamin")

ggplot(dataset3, aes(x=class.label, y=as.factor(age), fill=sex))+ 
  geom_col() + labs(y="Age")

#-----------------PRA-PROSES-------------#
#data Redundan
  datasetr<- dataset3 %>% distinct()

#Missing value
#-apakah ada?
any(is.na(datasetr))
#-dimana aja?
sapply(datasetr, function(x) any(is.na(x)))
#-berapa yang N/A?
sapply(datasetr, function(x) sum(is.na(x)))
datasetpd <- datasetr[-c(which(is.na(datasetr))), ]  


#menghilangkan tanda ?
dataset <- datasetpd %>% filter(age != "?", workclass != "?", fnlwgt != "?",
                                education != "?", education.num != "?", marital.status != "?", 
                                occupation != "?", relationship != "?", race != "?",  sex != "?",
                                capital.gain != "?", capital.loss != "?", hours.per.week != "?",
                                cative.country != "?", class.label != "?")
dataset_clean <- datasetpd %>% filter(age != "?", workclass != "?", fnlwgt != "?",
                                education != "?", education.num != "?", marital.status != "?", 
                                occupation != "?", relationship != "?", race != "?",  sex != "?",
                                capital.gain != "?", capital.loss != "?", hours.per.week != "?",
                                cative.country != "?", class.label != "?")

#membuat class.label menjadi 2 kelas (awalnya ada 4)
dataset$class.label <- as.factor(dataset$class.label)
dataset$class.label[dataset$class.label == "<=50K."] <- "<=50K"
dataset$class.label[dataset$class.label == ">50K."] <- ">50K"

dataset_clean$class.label <- as.factor(dataset_clean$class.label)
dataset_clean$class.label[dataset_clean$class.label == "<=50K."] <- "<=50K"
dataset_clean$class.label[dataset_clean$class.label == ">50K."] <- ">50K"

#membuat kategori umur 
dataset <- (mutate(dataset, age = ifelse(age %in% 15:30, "YoungAdult",
                                         ifelse(age %in% 31:50, "MiddleAdult", 
                                                "SeniorAdult"))))
dataset$age <- as.factor(dataset$age)

#membuat kategori jam kerja 
dataset <- (mutate(dataset, hours.per.week = ifelse(hours.per.week %in% 1:30, "Part-Time",
                                         ifelse(hours.per.week %in% 31:40, "Full-Time",
                                                ifelse(hours.per.week %in% 41:56, "Over-Time",
                                                 "Work-Holic")))))

dataset$hours.per.week <- as.factor(dataset$hours.per.week)

#membuat kategori capital gain
dataset[["capital.gain"]] <- ordered(cut(dataset[["capital.gain"]], 
                                             c(-Inf,0,median(dataset[["capital.gain"]] 
                                                             [dataset[["capital.gain"]]>0]),Inf)), labels=c("None", "Low", "High"))

#dataset <- (mutate(dataset, capital.gain = ifelse(capital.gain %in% 0, "None",
                                                    #ifelse(capital.gain %in% mean(dataset$capital.gain), "Low",
                                                     #             "High"))))


#membuat kategori capital loss 
dataset[["capital.loss"]] <- ordered(cut(dataset[["capital.loss"]], 
                                         c(-Inf,0,median(dataset[["capital.loss"]] 
                                                         [dataset[["capital.loss"]]>0]),Inf)), labels=c("None", "Low", "High"))

View(dataset)

#drop fitur
dataset$fnlwgt <- NULL
dataset$`education.num` <- NULL


#visualisasi
#proporsi pendapatan diatas 50k - status
library(knitr)
dataset_vis <- dataset
dataset_vis$income<-ifelse(dataset_vis$`class.label`=='>50K',1,0)
kable(head(dataset_vis))
MaritalLevel<- sqldf("SELECT `marital.status` as status
                    , Count (*) as Count
                    , sum(income) as Above50k
                    FROM dataset_vis
                    GROUP BY status
                    ORDER BY status") 
kable(MaritalLevel)

library(reshape2)
Maritalclass<-melt(MaritalLevel,id.vars = 'status')
ggplot(Maritalclass,aes(x=status,y=value,fill=variable))+
  geom_bar(stat = 'identity')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle('Proporsi pendapatan diatas 50K - Marital Status')+
  xlab("Status")+
  ylab("Jumlah orang")
table<-data.frame(Class=MaritalLevel$status, 
                  Proportion=
                    MaritalLevel$Above50k/MaritalLevel$Count)
kable(table)

#proporsi pendapatan diatas 50k - gender
GenderLevel<- sqldf("SELECT 
                    sex as gender
                       , Count (*) as Count
                       , sum(income) as Above50k
                       FROM 
                       dataset_vis
                       GROUP BY 
                       gender
                       ORDER BY gender") 
Genderclass<-melt(GenderLevel,id.vars = 'gender')
ggplot(Genderclass,aes(x=gender,y=value,fill=variable))+
  geom_bar(stat = 'identity')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle('Proporsi pendapatan diatas 50K - Gender')+
  xlab("Gender Class")+
  ylab("Number of People")

table<-data.frame(Class=GenderLevel$gender, 
                  Proportion=GenderLevel$Above50k/GenderLevel$Count)
kable(table)

#workclass
WorkclassLevel<- sqldf("SELECT 
                    workclass as workclass
                    , Count (*) as Count
                    , sum(income) as Above50k
                    FROM 
                    dataset_vis
                    GROUP BY 
                    workclass
                    ORDER BY workclass") 
Workclass<-melt(WorkclassLevel,id.vars = 'workclass')
ggplot(Workclass,aes(x=workclass,y=value,fill=variable))+
  geom_bar(stat = 'identity')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle('Proporsi pendapatan diatas 50K - WorkClass')+
  xlab("Work Class")+
  ylab("Number of People")
table<-data.frame(Class=WorkclassLevel$workclass, 
                  Proportion=
                    WorkclassLevel$Above50k/WorkclassLevel$Count)
kable(table)

#education
Education_Level<- sqldf("SELECT 
                       education as 'Education'
                        , Count (*) as 'Count'
                        , sum(income) as 'Above50k'
                        FROM 
                        dataset_vis
                        GROUP BY 
                        Education
                        ORDER BY Education") 
EducationLevel<-melt(Education_Level,id.vars = 'Education')
ggplot(EducationLevel,aes(x=Education,y=value,fill=variable))+
  geom_bar(stat = 'identity')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle('Proportion of People with income above 50k')+
  xlab("Education Class")+
  ylab("Number of People")

table<-data.frame(Class=Education_Level$Education, 
                  Proportion=
                    Education_Level$Above50k/Education_Level$Count)
kable(table)

#boxplot
ggplot(dataset_clean, aes(y=age, fill=age)) +
  geom_boxplot(varwidth = TRUE, alpha=0.2) +
  theme(legend.position="none") +
  ggtitle("Boxplot Umur (age)")

#pie chart
#p - education
Education_pie<- sqldf("SELECT 
                       education as 'Education'
                        , Count (*) as 'Count'
                        FROM 
                        dataset_vis
                        GROUP BY 
                        Education
                        ORDER BY Education") 
piepercent_ed<- round(100*Education_pie$Count/sum(Education_pie$Count), 1)
pie(Education_pie$Count, main = "Proporsi Education",
    labels = piepercent_ed,
    col = rainbow(length(Education_pie$Count)))
legend("topright", Education_pie$Education, cex = 0.6,
       fill=rainbow(length(Education_pie$Count)))

#p - status
marital_pie<- sqldf("SELECT 
                       `marital.status` as status
                    , Count (*) as Count
                    FROM dataset_vis
                    GROUP BY status
                    ORDER BY status") 
piepercent_mr<- round(100*marital_pie$Count/sum(marital_pie$Count), 1)
pie(marital_pie$Count, main = "Proporsi Marital Status",
    labels = piepercent_mr,
    col = rainbow(length(marital_pie$Count)))
legend("topright", marital_pie$status, cex = 0.6,
       fill=rainbow(length(marital_pie$Count)))

#p - gender
gender_pie<- sqldf("SELECT sex as gender, Count (*) as Count
                    FROM dataset_vis
                    GROUP BY gender
                    ORDER BY gender") 
piepercent_gd <- round(100*gender_pie$Count/sum(gender_pie$Count), 1)
pie(gender_pie$Count, main = "Proporsi Gender",
    labels = piepercent_gd,
    col = rainbow(length(gender_pie$Count)))
legend("topright", gender_pie$gender, cex = 0.6,
       fill=rainbow(length(gender_pie$Count)))

#p - work class
wclass_pie<- sqldf("SELECT 
                    workclass as workclass
                    , Count (*) as Count
                    FROM 
                    dataset_vis
                    GROUP BY 
                    workclass
                    ORDER BY workclass") 
piepercent_wc <- round(100*wclass_pie$Count/sum(wclass_pie$Count), 1)
pie(wclass_pie$Count, main = "Proporsi Workclass",
    labels = piepercent_wc,
    col = rainbow(length(wclass_pie$Count)))
legend("topright", wclass_pie$workclass, cex = 0.6,
       fill=rainbow(length(wclass_pie$Count)))

#p - occupation
occ_pie<- sqldf("SELECT 
                   occupation as occupation
                   , Count (*) as Count
                   FROM 
                   dataset_vis
                   GROUP BY 
                   occupation
                   ORDER BY occupation") 
piepercent_oc <- round(100*occ_pie$Count/sum(occ_pie$Count), 1)
pie(occ_pie$Count, main = "Proporsi Occupation",
    labels = piepercent_oc,
    col = rainbow(length(occ_pie$Count)))
legend("topright", occ_pie$occupation, cex = 0.6,
       fill=rainbow(length(occ_pie$Count)))

#p -age
age_pie<- sqldf("SELECT 
                age as age
                , Count (*) as Count
                FROM 
                dataset_vis
                GROUP BY 
                age
                ORDER BY age") 
piepercent_ag <- round(100*age_pie$Count/sum(age_pie$Count), 1)
piepercent_ag
pie(age_pie$Count, main = "Proporsi Age",
    labels = age_pie$age,
    col = rainbow(length(age_pie$Count)))

barplot(dataset$class.label)

counts <- table(dataset$class.label)
barplot(counts, main="Distribusi Income",
        xlab="Jumlah Income")

#HEATMAP
cor.mat <- round(cor(dataset_clean[,c(1,3,5,11,12,13)]),2)
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + 
  geom_text(aes(x=Var1, y=Var2, label=value)) + ggtitle("Heatmap Atribut Numerik")

# histogram of age by income group
ggplot(dataset_clean) + aes(x=age, group=class.label, fill=class.label) + 
  geom_histogram(binwidth=1, color='black') +ggtitle("Distribusi Umur menurut Pendapatan")
hist(dataset$class.label)
# histogram of age by income group
ggplot(dataset) + aes(x=class.label, fill=class.label) + 
  geom_histogram(binwidth=1, color='black') +ggtitle("Distribusi Umur menurut Pendapatan")

# histogram of age by gender group
ggplot(dataset_clean) + aes(x=age, group=sex, fill=sex) + 
  geom_histogram(binwidth=1, color='black') +ggtitle("Distribusi Umur menurut Jenis Kelamin")

levels(dataset_clean$workclass)[1] <- 'Unknown'
# combine into Government job
dataset_clean$workclass <- gsub('^Federal-gov', 'Government', dataset_clean$workclass)
dataset_clean$workclass <- gsub('^Local-gov', 'Government', dataset_clean$workclass)
dataset_clean$workclass <- gsub('^State-gov', 'Government', dataset_clean$workclass) 

# combine into Sele-Employed job
dataset_clean$workclass <- gsub('^Self-emp-inc', 'Self-Employed', dataset_clean$workclass)
dataset_clean$workclass <- gsub('^Self-emp-not-inc', 'Self-Employed', dataset_clean$workclass)

# combine into Other/Unknown
dataset_clean$workclass <- gsub('^Never-worked', 'Other', dataset_clean$workclass)
dataset_clean$workclass <- gsub('^Without-pay', 'Other', dataset_clean$workclass)
dataset_clean$workclass <- gsub('^Other', 'Other/Unknown', dataset_clean$workclass)
dataset_clean$workclass <- gsub('^Unknown', 'Other/Unknown', dataset_clean$workclass)

dataset_clean$workclass <- as.factor(dataset_clean$workclass)

count <- table(dataset_clean[dataset_clean$workclass == 'Government',]$class.label)["<=50K"]
count <- c(count, table(dataset_clean[dataset_clean$workclass == 'Government',]$class.label)[">50K"])
count <- c(count, table(dataset_clean[dataset_clean$workclass == 'Other/Unknown',]$class.label)["<=50K"])
count <- c(count, table(dataset_clean[dataset_clean$workclass == 'Other/Unknown',]$class.label)[">50K"])
count <- c(count, table(dataset_clean[dataset_clean$workclass == 'Private',]$class.label)["<=50K"])
count <- c(count, table(dataset_clean[dataset_clean$workclass == 'Private',]$class.label)[">50K"])
count <- c(count, table(dataset_clean[dataset_clean$workclass == 'Self-Employed',]$class.label)["<=50K"])
count <- c(count, table(dataset_clean[dataset_clean$workclass == 'Self-Employed',]$class.label)[">50K"])
count <- as.numeric(count)

# create a dataframe
industry <- rep(levels(dataset_clean$workclass), each = 2)
income <- rep(c('<=50K', '>50K'), 4)
df <- data.frame(industry, income, count)
df

# calculate the percentages
df <- ddply(df, .(industry), transform, percent = count/sum(count) * 100)

# format the labels and calculate their positions
df <- ddply(df, .(industry), transform, pos = (cumsum(count) - 0.5 * count))
df$label <- paste0(sprintf("%.0f", df$percent), "%")

# bar plot of counts by industry with in group proportions 
ggplot(df, aes(x = industry, y = count, fill = income)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Income by Industry')

# histogram of capital_gain
ggplot(dataset_clean) + aes(x=as.numeric(capital.gain), group=class.label, fill=class.label) + 
  geom_histogram(bins=10, color='black') + ggtitle('Histogram of Capital Gain')

# histogram of capital_loss
ggplot(dataset_clean) + aes(x=as.numeric(capital.loss), group=class.label, fill=class.label) + 
  geom_histogram(bins=10, color='black') + ggtitle('Histogram of Capital Loss')


#---------------ASSOCIATION------------#

#FPGrowth
library(rJava)
library(arules)
library(Matrix)
library(rCBA)
library(grid)
library(arulesViz)
#rules1=======
rules1_fp <- rCBA::fpgrowth(dataset, support = 0.2,
                           confidence = 0.6, maxLength = 8, 
                           consequent = "class.label", parallel=FALSE)
rules1_fp
inspect(rules1_fp)
#Sort Rules
sort.rule1 <- sort(rules1_fp, by="lift")
sort.rule1 <- sort(rules1_fp, by="confidence")
inspect(sort.rule1) 
inspect(sort.rule1[1:5]) 
#Visualization
plot(sort.rule1[1:10], method="graph", control=list(nodeCol="red", edgeCol="blue"))
plot(sort.rule1)
plot(sort.rule1, method="grouped", control=list(col=2))

#rules2=======
rules2_fp <- rCBA::fpgrowth(dataset, support = 0.3,
                           confidence = 0.6, maxLength = 8, 
                           consequent = "class.label", parallel=FALSE)

rules2_fp
inspect(rules2_fp)
#Sort Rules
sort.rule2 <- sort(rules2_fp, by="lift")
sort.rule2 <- sort(rules2_fp, by="confidence")
inspect(sort.rule2) 
inspect(sort.rule2[1:5])
#Visualization
library(grid)
library(arulesViz)
plot(sort.rule2[1:10], method="graph", control=list(nodeCol="red", edgeCol="blue"))
plot(sort.rule2)
plot(sort.rule2, method="grouped", control=list(col=2))

#rules3 ========
rules3_fp <- rCBA::fpgrowth(dataset, support = 0.2,
                            confidence = 0.7, maxLength = 8, 
                            consequent = "class.label", parallel=FALSE)
rules3_fp
#Sort Rules
sort.rule3 <- sort(rules3_fp, by="lift")
inspect(sort.rule3)
inspect(sort.rule3[1:5])
#Visualization
library(grid)
library(arulesViz)
plot(sort.rule3[1:10], method="graph", control=list(nodeCol="red", edgeCol="blue"))
plot(sort.rule3)
plot(sort.rule3, method="grouped", control=list(col=2))


#rules4=======
rules4_fp <- rCBA::buildFPGrowth(dataset, "class.label",parallel=FALSE, verbose=TRUE)
inspect(rules4_fp$itemset)
inspect(rules4_fp$model)
inspect(rules4_fp[1:5])
#Sort Rules
sort.rule4 <- sort(rules4_fp, by="lift")
sort.rule4 <- sort(rules4_fp, by="confidence")

inspect(sort.rule4) 
inspect(sort.rule4[1:5])
#Visualization
plot(sort.rule4[1:10], method="graph", control=list(nodeCol="red", edgeCol="blue"))
plot(sort.rule4)
plot(sort.rule4, method="grouped", control=list(col=2))

dataset$age <- as.factor(dataset$agedon)
dataset$age <- as.factor(dataset$age)
dataset.tr<-as(dataset, "transactions")

itemFrequencyPlot(dataset.tr, topN=10, type="absolute", main="Item Frequency")

install.packages("SparkR")
library(SparkR)
sparkR.session(master = "", appName = "SparkR",
               sparkHome = Sys.getenv("SPARK_HOME"), sparkConfig = list(),
               sparkJars = "", sparkPackages = "", enableHiveSupport = TRUE)
sparkR.session()
dataspark <- SparkR::createDataFrame(dataset, schema = NULL,
                samplingRatio = 1, numPartitions = NULL)
spark.fpGrowth(dataspark)
spark.freqItemsets(dataset)
SparkR::freqItems(dataset, class.label, support = 0.01)



