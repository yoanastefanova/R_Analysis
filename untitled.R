library(readxl)
df <- read_excel("Отговори.xlsx") #прочитаме информацията от таблица
library(tidyverse)
dat<-df %>% select("Интровертност","Екстравертност","Мисли",
                   "Чувства","Пол")
p1 <- table(dat$Интровертност)


men <- filter(dat, Пол == "Мъж")
women <- filter(dat, Пол == "Жена")

ggplot(women, aes(x = Чувства)) + geom_dotplot(fill = "red") +
 scale_y_continuous(NULL, breaks = NULL)
ggplot(men, aes(x = Мисли)) + 
  geom_dotplot(fill = "blue") + scale_y_continuous(NULL, breaks = NULL)



ggplot(dat$Интровертност, aes(x = Мисли)) + 
  geom_dotplot() + scale_y_continuous(NULL, breaks = NULL)

dat$Мисли <- as.factor(dat$Мисли)
ggplot(dat, aes(x=Мисли, y=Интровертност)) + 
  geom_dotplot(binaxis='y', stackdir='center')


boxplot(df$Интровертност~df$Пол,col=(c("red","darkblue")),
        xlab="Пол и Интровертност", ylab = "")

wilcox.test(Чувства ~ Пол, data = dat, exact = FALSE)


kruskal.test(Мисли ~ Пол, data = dat)


cor.test(men$Интровертност, men$Чувства)
cor.test(women$Интровертност, women$Чувства)


cor.test(men$Интровертност, men$Мисли)

cor.test(women$Интровертност, women$Мисли)
