#Load the data 
library(readr)
library(rpart)
library(data.table)
library(sqldf) 
library(radarchart) 
library(tidyr)
library(dplyr)
library(plyr)
library(dtplyr) 
library(DT) 
library(ggplot2)
library(modeest) 
library(data.table)
library(recommenderlab) 
library(stringr)

library(knitr)
library(grid)
library(gridExtra)
library(corrplot)
library(ggraph) 
library(methods)
library(Matrix)
library(ggthemes)
library(caret)
library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians 
 

install.packages("fpc")
library(fpc) #para hacer el plotcluster x 
install.packages("ggraph")
install.packages("modeest")
install.packages("dtplyr")
install.packages("radarchart")
install.packages("sqldf")
install.packages("DT")
install.packages("recommenderlab")

# reading the dataset

FullData1 <- read_csv("C:/Mindenes/Foci/Fifa18 forecasting/complete.csv")
complemento1 = read_csv("C:/Mindenes/Foci/CompleteDataset.csv")
View(FullData1)
View(complemento1)

#Summary of the data
setDT(FullData)
setDT(complemento)
names(FullData) # az adathalmaz v�ltoz�inak a neve


#Ages of football players
windows()
ggplot(FullData, aes(age, fill=age)) + geom_density(position = "Stack")

#Player�s Overall
windows()
FullData %>% ggplot(aes(x=overall, fill=factor(overall))) + geom_bar(color="grey20") + guides(fill=FALSE )+ labs(title = "Player's Overall")
#  The minimum rating is 46, the maxima is 94, the mean is 66.2530288 and the trend is 66


#Rating vs Age 
windows()

agerating <- FullData[age<41,.("overall"=mean(overall)),by=age][order(-age)]
ggplot(data = agerating,aes(x=age,y=overall))+
  geom_line(color="red",size=2)+labs(title="Rating vs Age")+
  annotate("text", x = 30, y = max(agerating$overall),color="blue", label = "Max", parse = TRUE, size = 3)

#Best players (TOP10)
TOP10 = FullData %>% 
  #arrange(-overall) %>%
  #top_n(15, =overall) %>%
  select(name,age,overall, club,eur_value) %>%
  data.table(class = "nowrap hover row_border", escape = FALSE, options= list(dom= 't', scrollX = TRUE, autoWidth=TRUE))

TOP10
View(TOP10)


#Physical of the players 
#Players Height 
windows()
FullData %>% 
  ggplot(aes(x=height_cm, fill=factor(height_cm))) + geom_bar(color="grey20")+ guides(fill=FALSE)+ labs(title="Players Height")
# The minimum height is 155, the maxima is 205, the mean is 181.2719795 and the trend is 180


# Players Weight
FullData %>%
  ggplot(aes(x=weight_kg, fill=factor(weight_kg))) + geom_bar(color="grey20")+ guides(fill=FALSE)+ labs(title="Players Weight")


# Players per country (TOP10)
Nationalitys <- FullData[FullData$nationality!="",.N,by=.(nationality,`flag`)][order(-N)]
Nationalitys = Nationalitys[,c(1,3)] 
head(Nationalitys[order(Nationalitys$N, decreasing = TRUE),],10)

# Best Team
#Groping players by club and appling average on players rating
TeamDF <- arrange(FullData[, list(Avg=mean(overall)), by="club"], desc(Avg))
TeamDF<-arrange(FullData[, list(Avg=mean(overall)), by= "club" ], desc(Avg) )
kable(head(TeamDF, 10))


# Clustering (Ward's m�dszer) 2 �s 4 klaszter k�z�tt lehets�ges
cluster= FullData[,c(7,10,11,17:29,34:94)] 
cluster[is.na(cluster)] = 0 
wss <- (nrow(cluster[,c()])-1)*sum(apply(cluster[,1:ncol(cluster)],2,var))    
for (i in 2:10) 
  wss[i] <- sum(kmeans(cluster[,1:ncol(cluster)], centers=i)$withinss)

plot(1:
       10, wss, type="b", xlab="Number of Cluster", ylab="Squares Summatory")
# In the graph of Number of Clusters you can see a big change between 2 and 4. 


# Realization of clusters
set.seed(123)
km <- kmeans(cluster[,1:ncol(cluster)],4)
cluster$grupo <- km$cluster
FullData$grupo <-km$cluster
g1 <- FullData[FullData$grupo ==1,]
g2 <- FullData[FullData$grupo ==2,]
g3 <- FullData[FullData$grupo ==3,]
g4 <- FullData[FullData$grupo ==4,]

plotcluster(cluster[,1:(ncol(cluster)-1)],km$cluster) # fpc konyvt�rat kell megnyitni



#Players Overall
library(gridExtra)
p1 = g1 %>% 
  ggplot(aes(x = overall, fill = factor(overall))) +
  geom_bar(color = "grey20") + guides(fill = FALSE)+
  labs(title="Cluster 1 Ratings")
p2 = g2 %>% 
  ggplot(aes(x = overall, fill = factor(overall))) +
  geom_bar(color = "grey20") + guides(fill = FALSE)+
  labs(title="Cluster 2 Ratings")
p3= g3 %>% 
  ggplot(aes(x = overall, fill = factor(overall))) +
  geom_bar(color = "grey20") + guides(fill = FALSE)+
  labs(title="Cluster 3 Ratings")
p4 = g4 %>% 
  ggplot(aes(x = overall, fill = factor(overall))) +
  geom_bar(color = "grey20") + guides(fill = FALSE)+
  labs(title="Cluster 4 Ratings")

grid.arrange(p1,p2,p3,p4, nrow= 2)

# Clusters of best player 
#(Mindegyik klaszterben top 10-es j�t�kosai vil�gklasszisok ) (kilist�zzuk )
b1 = g1 %>% 
  mutate(image = paste0('<img src="', `photo`, '"></img>')) %>% 
  #arrange(-overall) %>% 
  #top_n(15,wt = overall) %>% 
  select(name, age,overall,club,eur_value) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))
windows()

g2 %>% 
  mutate(image = paste0('<img src="', `photo`, '"></img>')) %>% 
  #arrange(-overall) %>% 
  #top_n(15,wt = overall) %>% 
  select(name, age,overall,club,eur_value) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

g3 %>% 
  mutate(image = paste0('<img src="', `photo`, '"></img>')) %>% 
  #arrange(-overall) %>% 
  #top_n(15,wt = overall) %>% 
  select(name, age,overall,club,eur_value) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

g4 %>% 
  mutate(image = paste0('<img src="', `photo`, '"></img>')) %>% 
  #arrange(-overall) %>% 
  #top_n(15,wt = overall) %>% 
  select(name, age,overall,club,eur_value) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))


#Players per Country (a 4 klaszternek a j�t�kosait)
#1. klaszterben nagyon kev�s j�tokos nemzetis�gei vannak
#2. klaszterban m�r enn�l t�bb, sok j�t�kos a vil�g minden t�j�r�l
#3. klaszterben nem rendelkezik sok j�t�kossal,de �k t�bbnyire eur�paiak �s d�l-amerikaiak
#4. klaszterben a j�t�kosok a vil�g minden t�j�r�l �rkeztek,kevesebb �zsiai j�t�kos van

Nationalitys <- g1[g1$nationality!="",.N,by=.(nationality,`flag`)][order(-N)] 
Nationalitys = Nationalitys[,c(1,3)]
head(Nationalitys[order(Nationalitys$N, decreasing = TRUE),],10)


Nationalitys <- g2[g2$nationality!="",.N,by=.(nationality,`flag`)][order(-N)]
Nationalitys = Nationalitys[,c(1,3)]
head(Nationalitys[order(Nationalitys$N, decreasing = TRUE),],10)


Nationalitys <- g3[g3$nationality!="",.N,by=.(nationality,`flag`)][order(-N)]
Nationalitys = Nationalitys[,c(1,3)]
head(Nationalitys[order(Nationalitys$N, decreasing = TRUE),],10)


Nationalitys <- g4[g4$nationality!="",.N,by=.(nationality,`flag`)][order(-N)]
Nationalitys = Nationalitys[,c(1,3)]
head(Nationalitys[order(Nationalitys$N, decreasing = TRUE),],10)


#Players Value
t1 = ggplot(g1, aes(eur_value, fill = eur_value)) +
  geom_density(position = "stack")+labs(title="Cluster 1 Ratings")
summary(g1$eur_value)

t2 = ggplot(g2, aes(eur_value, fill = eur_value)) +
  geom_density(position = "stack")+labs(title="Cluster 2 Ratings")
summary(g2$eur_value)

t3 = ggplot(g3, aes(eur_value, fill = eur_value)) +
  geom_density(position = "stack")+labs(title="Cluster 3 Ratings")
summary(g3$eur_value)

t4 = ggplot(g4, aes(eur_value, fill = eur_value)) +
  geom_density(position = "stack")+labs(title="Cluster 4 Ratings")
summary(g4$eur_value)


grid.arrange(t1,t2,t3,t4, nrow=2)


#Cluster conclusion (k�vetkeztet�s a klaszterekr�l)
# a nemzetis�g nem szab�lyozza, hogy melyik klaszterhez tartozik a j�t�kos, A j�t�kosok min�s�t�se mellet az �rt�k�k is fontos, hogy egy adott cosportban legyenek. 
#tov�bb� �szrevehet�, hogy egy adott j�t�kos m�n�s�t�se egy adott cosporton bel�l �s k�v�l is elhelyezhet�
# az els� klaszterben 46 j�t�kos van, m�g a 2.-ban 337 (ez csup�n kb 10,56%-ka a j�t�kosoknak )


#Machine Learning
# a j�t�kosok k�l�nb�z� posztokon j�tszanak, �s az �sszes specifik�ci�kr�l van adat. 
#el�rejelezz�k, hogy a focist�k a v�delemben vagy a t��mad�sban j�tszanak fontosabb szerepet
#1 a t�mad�s, 0 a v�delem

#kiv�lasztjuk �s megtiszt�tjuk az adatokat, �tfog�, potenci�lis, prefer�lt �s �sszes pozici�t vizsg�lunk
#ezut�n meg kell tisztitani a prefer�lt pozici�t, kiv�lasztva minden j�t�kos els� poz�ci�j�t

## Getting specs from all players
specs = FullData[,c(22:30,36:67)]
specs = data.frame(specs)
View(specs)
## Specs normalization
specsNorm = preProcess(specs,method = "scale")
specs = predict(specsNorm, specs)
## Adding ID and Overall to Specs
id_overall = FullData[,c(1,20)]
specs = cbind(specs,id_overall)

##Getting prefered_positions and ID
positions = complemento[,c(53,64)]   
positions = data.frame(positions)
View(position)
## Cleaning Preferred Positions
prefered_positions = positions[,'Preferred.Positions']
split = strsplit(prefered_positions, split=" ")
positionVector = 0
length = length(split)
for (position in 1:length) {
  positionVector[position] <- unlist(split[[position]][1]) 
}
positions[,'Preferred.Positions'] = positionVector


#Joining data (adatok �sszekapcsol�sa)
selected_data = join(specs, positions, by = NULL, type = "full", match = "first") 
selected_data = selected_data[complete.cases(selected_data),]
View(selected_data)

#Attack or Defense
position = selected_data[,'Preferred.Positions']
attack = c('ST','LW','RW','RM','CM','LM','CAM','CF')
defense = c('CDM','CB','LB','RB','RWB','LWB','GK')

View(FullData)
#Replacing (visszahelyez�s)
position <- lapply(position, function(x) replace(x,x %in% attack, 1))
position <- lapply(position, function(x) replace(x,x %in% defense, 0))


positionVector = 0
for (i in 1:length(position)){
  positionVector[i] = position[[i]]
}

positionVector = as.numeric(positionVector)


#joining (�sszekapcsol�s)
selected_data = cbind(selected_data,positionVector)


selected_data <- subset( selected_data, select = -ID )
selected_data[,'Preferred.Positions'] = as.factor(selected_data[,'Preferred.Positions'])

kable(head(selected_data))
View(selected_data)

#data segmentation (adatszegment�ci�)
porciento <- 70/100

set.seed(3)

trainRowsNumber<-sample(1:nrow(selected_data),porciento*nrow(selected_data))
train<-selected_data[trainRowsNumber,] 
test<-selected_data[-trainRowsNumber,] 

ataque = selected_data[selected_data[,'positionVector'] ==1,-positionVector]
ataque$Preferred.Positions = factor(ataque$Preferred.Positions)
defensa = selected_data[selected_data[,'positionVector'] ==0,-positionVector]
defensa$Preferred.Positions = factor(defensa$Preferred.Positions)

trainRowsNumber<-sample(1:nrow(ataque),porciento*nrow(ataque))
trainA<-ataque[trainRowsNumber,] 
testA<-ataque[-trainRowsNumber,] 

trainRowsNumber<-sample(1:nrow(defensa),porciento*nrow(defensa))
trainD<-defensa[trainRowsNumber,] 
testD<-defensa[-trainRowsNumber,] 

kable(head(train)) 

View(FullData)

#Logistic Regresion
modelo<-glm(positionVector~., data = train)
pred<-predict(modelo,newdata = test)
pred = round(pred)
cfmLR = confusionMatrix(pred,test$positionVector)
cfmLR

#Attack
model <- svm(Preferred.Positions~. ,data=trainA,kernel = "linear")

#Confusion Matrix (T�mad�kn�l)
prediccion <- predict(model,testA)
#prediccion = round(prediccion, digits = 0)
cfmSVM<-confusionMatrix(prediccion,testA[,'Preferred.Positions'])
cfmSVM

#Defense
modelD <- svm(Preferred.Positions~. ,data=trainD,kernel = "linear")

#Confusion Matrix (v�dekez�ekn�l)
prediccion2 <- predict(modelD,testD)
#prediccion = round(prediccion, digits = 0)
cfmSVM2<-confusionMatrix(prediccion2,testD[,'Preferred.Positions'])
cfmSVM2


View(testD)
View(testA)
View(FullData)
