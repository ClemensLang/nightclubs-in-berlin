library(rgdal)
library(raster)
library(e1071)
library(randomForest)
library(ggplot2)
library(caTools)
library(gridExtra)
library(readxl)
library(dplyr)
library(ggplot2)
library(randomForest)
library(caret)
library(corrplot)
library(rstatix)



raster <- stack("/Users/clemens/Desktop/Spatial Modeling/MAP/raster_tests/ratsers/clipped_stack.tif")


df <- read.csv("/Users/clemens/Desktop/shape_files/tp_id_landuse.csv", header = TRUE)
str(df)
df$type <- as.factor(df$type)
df$club <- as.character(df$club)
df$simple_cat <- as.character(df$simple_cat)

names <- c("id", "club.name", "land.use",
           "noisiness", "building.age", "building.height", "land.value",
           "bar.dist", "train.dist", "rail.dist", "bus.dist",
           "water.dist", "police.dist", "pop.density", "pop.slope", "type" )

colnames(df) <- names
names(raster) <- names[-c(1:3,16)]


ggplot(df, aes(x=bar.dist, y=pop.density, color=type))+
  geom_point()



set.seed(444)
split_sample <- sample.split(df , SplitRatio = 0.75)
  
train <- subset(df, split_sample == TRUE)
reference <- subset(df, split_sample == FALSE)

model <- randomForest(type~.,data = train[,-(1:3)], importance=TRUE)


importance <- varImp(model, conditional=TRUE)
importance <- importance %>% tibble::rownames_to_column("var") 
importance$var<- importance$var %>% as.factor()


importance <- importance[,-3]
colnames(importance) <- c("var", "varImp")
importance$MDA <- importance(model, type=1)[,1]
importance$MDG <- importance(model, type=2)[,1]
importance

                          
importance_plots <- function(data, type){
  bar <- ggplot(data = data) + 
    geom_bar(
      stat = "identity",#it leaves the data without count and bin
      mapping = aes(x = var, y=eval(parse(text=type)), fill = var), 
      show.legend = FALSE,
      width = 1
    ) + 
    scale_fill_manual(values=c(
      "#655643", "#3B4EE1", "#DDE304", "#FFAF00", "#BF4D28", "#F6AE9A",
      "#29742F", "#C8C8A9", "#FFE700", "#56D1D1", "#EF443C", "#A610AE")) +
    labs(x = NULL, y = NULL)
  #p1 <- bar + coord_polar() + theme_minimal()
  #p2 <- bar + coord_flip() + theme_minimal()
  #grid.arrange(p1, p2, nrow = 2)
  bar + coord_polar() + theme_minimal() #+ scale_color_manual(values=c(
  #"#655643", "#80BCA3", "#F6F7BD", "#E6AC27", "#BF4D28", "#C6A49A",
  #"#FF9C5B", "#C8C8A9", "#D4EE5E", "#86B8B1", "#EF746F", "#D3CE3D"))
}


no_club <- df[which(df$type == 0),]

ggplot(data = no_club, aes(x=land.use)) + 
  geom_bar(show.legend = FALSE, aes(fill = land.use)) + 
  geom_text(stat='count', aes(label= ..count..)) +
  scale_fill_manual(values=c(
    "#655643", "#3B4EE1", "#DDE304", "#FFAF00", "#BF4D28", "#F6AE9A",
    "#29742F", "#C8C8A9", "#FFE700", "#56D1D1", "#EF443C", "#A610AE")) +
  labs(x = NULL, y = NULL) + theme_minimal()





importance_plots(importance, "MDA")
importance_plots(importance, "MDG")
importance_plots(importance, "varImp")

par(mfrow=c(1,2))
clubs <- df[which(df$type == "1"),]
no_clubs <- df[which(df$type == "0"),]



cor.mat_c <- cor(clubs[,-c(1:3,16)], method = "pearson")

cor_plot_p <- function(data){
  cor.mat_c <- cor_mat(
    data[,-c(1:3,16)],
    vars = NULL,
    method = "pearson",
    alternative = "two.sided",
    conf.level = 0.95
  )
  cor.mat_c %>% cor_get_pval()
  cor.mat_c %>%
    cor_reorder() %>%
    pull_lower_triangle() %>%
    cor_plot(label = TRUE)
}

cor_plot_p(clubs)
cor_plot_p(no_clubs)
cor_plot_p(train)

cor.mat_c <- cor_mat(
  clubs[,-c(1:3,16)],
  vars = NULL,
  method = "pearson",
  alternative = "two.sided",
  conf.level = 0.95
)

cor.mat_c %>% cor_get_pval()
cor.mat_c %>%
  cor_reorder() %>%
  pull_lower_triangle() %>%
  cor_plot(label = TRUE)

cor.mat_nc <- cor(no_clubs[,-c(1:3,16)], method = "pearson")
cor.mat <- cor(train[,-c(1:3,16)], method = "pearson")#check Correlation among drivers

corrplot(cor.mat_c)
corrplot(cor.mat_nc)
corrplot(cor.mat)

par(mfrow=c(1,1))




round(data.matrix(cor.mat_c),2)
round(data.matrix(cor.mat),2)







model$importance
importance(model, type=1)
importance(model, type=2)

varImp(model)












reference$pred <- as.numeric(predict(model, reference[,-c(1:3,16)]))-1

# first classified points then validation for confusion-matrix
cfm <- as.data.frame.matrix(table(reference[,17:16]))

colnames(cfm) <- c("nightclub", "no-club")
rownames(cfm) <- c("nightclub", "no-club")

cfm$user.accuracy[1] <- round(cfm[1,1]/sum(cfm[1,1:2]),2)
cfm$user.accuracy[2] <- round(cfm[2,2]/sum(cfm[2,1:2]),2)

cfm["producer.accuracy",1] <- round(cfm[1,1]/sum(cfm[1:2,1]),2)
cfm["producer.accuracy",2] <- round(cfm[2,2]/sum(cfm[1:2,2]),2)

cfm[3,3] <- round(sum(diag(table(reference[,17:16]))/ sum(cfm[1:2,1:2])),2)



# print the confusion matrix with accuracies
cfm




pred <- predict(raster, model)
pred.prob <- predict(raster, model, type="prob")

writeRaster(pred, "/users/clemens/Desktop/prediction_classes_new","GTiff",  overwrite=T)
writeRaster(pred.prob, "/users/clemens/Desktop/prediction_probability_new","GTiff",  overwrite=T)

train_id <- train[,1:3]
train_id$train <- 1


errors <- reference[which(reference$type != reference$pred),c(1:3,16:17)]
write.csv(errors, "/users/clemens/Desktop/errors.csv")
write.csv(train_id, "/users/clemens/Desktop/train_id.csv")
