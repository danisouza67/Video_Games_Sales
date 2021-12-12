install.packages("plotly")
library(plotly)

vg_dataset <- read.csv(file = "vgsales.csv", stringsAsFactors = FALSE)

#adding one more row to have a properly Index as "rank" col is not as consistent
vg_dataset$Index <- 1:nrow(vg_dataset)
#organising dataset's on R Studio, staring from Index and so on.
vg_dataset <- vg_dataset[,c(12,1,2,3,4,5,6,7,8,9,10,11)]

#Starting Cleaning and Normalizing the data
#first run some checkups to indentify NA values in the dataset
complete.cases(vg_dataset)
summary(vg_dataset)
str(vg_dataset)

#here we wan to take an action on Year's column:  
#First a basic histogram to see the data,
hist(vg_dataset$Year)
#Second transforming the 0 data into NA
vg_dataset$Year[vg_dataset$Year == 0] <- NA
#Third we copies NA items to another dataset (NAs_vg_dataset)
NAs_vg_dataset <- subset(vg_dataset, is.na(vg_dataset$Year))
#Then remove those NAs from main dataset
vg_dataset <- vg_dataset[complete.cases(vg_dataset),]
#Afterwards, we copy outliers to a outliers dataset, and remove them from the main dataset
outliers_vg_dataset <- vg_dataset[!(vg_dataset$Year > 1979 & vg_dataset$Year < 2017),]
vg_dataset <- vg_dataset[vg_dataset$Year > 1979 & vg_dataset$Year < 2017,]




#redefining column Year as numeric and trying to show it as 4 digits
vg_dataset$Year <- as.numeric(format(vg_dataset$Year, '%Y'))  # This works
# dont need that > vg_dataset$Year <- as.Date(vg_dataset$Year, format = "%Y")
# dont need that > format(as.Date(vg_dataset$Year, format="%Y/%d/%m"),"%Y")
# dont need that > vg_dataset$Year <- format(as.Date(vg_dataset$Year, format="%Y/%d/%m"),"%Y")



#----Min-Max Normalization----
ggplot(vg_dataset, aes( x = NA_Sales, y = EU_Sales )) + geom_point()
normaData <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}
vg_dataset$NA_Sales_Norm <- normaData(vg_dataset$NA_Sales)
vg_dataset$EU_Sales_Norm <- normaData(vg_dataset$EU_Sales)
vg_dataset$JP_Sales_Norm <- normaData(vg_dataset$JP_Sales)
vg_dataset$Other_Sales_Norm <- normaData(vg_dataset$Other_Sales)
vg_dataset$Global_Sales_Norm <- normaData(vg_dataset$Global_Sales)
ggplot(vg_dataset, aes( x = NA_Sales_Norm, y = EU_Sales_Norm )) + geom_point()
#------------------------------



#----Z Score----
#Z-Score
vg_dataset$NA_Sales_Scaled <- scale(vg_dataset$NA_Sales)
vg_dataset$EU_Sales_Scaled <- scale(vg_dataset$EU_Sales)
vg_dataset$JP_Sales_Scaled <- scale(vg_dataset$JP_Sales)
vg_dataset$Other_Sales_Scaled <- scale(vg_dataset$Other_Sales)
vg_dataset$Global_Sales_Scaled <- scale(vg_dataset$Global_Sales)
ggplot(vg_dataset, aes( x = NA_Sales_Scaled, y = EU_Sales_Scaled )) + geom_point()
ggplot(vg_dataset, aes( x = EU_Sales_Scaled, y = JP_Sales_Scaled )) + geom_point()
#----------------



#----Robust Scaler----
#Robust Scaler
robust_scalar<- function(x){
  +     (x- median(x)) / (quantile(x, probs = .75) - 
                            +                           quantile(x, probs = .25))
  +     }
#creating variables
vg_dataset$NA_Sales_RobustS <- robust_scalar(vg_dataset$NA_Sales)
vg_dataset$EU_Sales_RobustS <- robust_scalar(vg_dataset$EU_Sales)
vg_dataset$JP_Sales_RobustS <- robust_scalar(vg_dataset$JP_Sales)
vg_dataset$Other_Sales_RobustS <- robust_scalar(vg_dataset$Other_Sales)
vg_dataset$Global_Sales_RobustS <- robust_scalar(vg_dataset$Global_Sales)
ggplot(vg_dataset, aes( x = EU_Sales_RobustS, y = JP_Sales_RobustS )) + geom_point()
#---------------------


#----Correlation data----
corr_data <- subset(vg_dataset, select = c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales"))
view(corr_data)
cor(corr_data)
cor.test(corr_data)
cor.test(corr_data$NA_Sales, corr_data$EU_Sales, method = "pearson")
cor.test(corr_data$NA_Sales, corr_data$EU_Sales, method = "kendall")
m_corr_data <- cor(corr_data)

#corrplot
install.packages("corrplot")
m_corr_data <- cor(corr_data)
corrplot(m_corr_data)
corrplot(m_corr_data, method = "number", type = "upper")
#------------------------


#----Scatter, Line, HeatMap----
library(ggplot2)

#Scatter Plot
#Scatterplot with basic line, visualizing North America Sales and Japan Sales, in Robust Scale.
ggplot(vg_dataset, aes(x= NA_Sales_RobustS, y = JP_Sales_RobustS)) + geom_point() + geom_abline(aes(intercept= 0, slope = 2), colour = "red", linetype = 6)
# Smoother Line Scatterplot visualizing North America Sales and Japan Sales, in Robust Scale.
ggplot(vg_dataset, aes(x= NA_Sales_RobustS, y = JP_Sales_RobustS)) + geom_point() + geom_smooth(formula = y ~ poly(x,2), method = "lm")


#Line Plot 
#variable for colors
colors <- c("Europe Sales" = "green", "Japan Sales" = "orange")
#Line plot analysing Europe and Japan Sales against Global Sales, in Z-score Scale
ggplot(vg_dataset)+
  geom_line( mapping = aes(x = Global_Sales_Scaled, y = JP_Sales_Scaled, color = "Japan Sales")) +
  geom_point( mapping = aes(x = Global_Sales_Scaled, y = JP_Sales_Scaled, color = "Japan Sales")) +
  geom_line( mapping = aes(x = Global_Sales_Scaled, y = EU_Sales_Scaled, color = "Europe Sales")) +
  geom_point( mapping = aes(x = Global_Sales_Scaled, y = EU_Sales_Scaled, color = "Europe Sales"))+
  labs(x = "Global Sales (Scaled)", y = "Japan Sales, Europe Sales (Scaled)", color = "Legend", title = "Line Plot comparing Europe and Japan sales against Global sales (Scaled)")+
  theme( legend.position = c(.71, .95),
         legend.justification = c("right", "top"),
         legend.box.just = "right",
         legend.margin = margin(6, 6, 6, 6))+
  scale_color_manual(name = "Colors",values = colors)

#Line plot analysing Europe and Japan Sales against Global Sales, in Robust Scalar
ggplot(vg_dataset)+geom_line( mapping = aes(x = Global_Sales_RobustS, y = JP_Sales_RobustS, color = "Japan Sales")) +
  geom_point( mapping = aes(x = Global_Sales_RobustS, y = JP_Sales_RobustS, color = "Japan Sales")) +
  geom_line( mapping = aes(x = Global_Sales_RobustS, y = EU_Sales_RobustS, color = "Europe Sales")) +
  geom_point( mapping = aes(x = Global_Sales_RobustS, y = EU_Sales_RobustS, color = "Europe Sales"))+
  labs(x = "Global Sales (Robust Scalar)", y = "Japan Sales, Europe Sales (Robust Scalar)", color = "Legend", title = "Line Plot comparing Europe and Japan sales against Global sales (Robust Scalar)")+
  theme( legend.position = c(.71, .95),
         legend.justification = c("right", "top"),
         legend.box.just = "right",
         legend.margin = margin(6, 6, 6, 6))+
  scale_color_manual(name = "Colors",values = colors)

#HeatMap
install.packages("reshape")

scale_data <- subset(vg_dataset, select = c("NA_Sales_Scaled", "EU_Sales_Scaled", "JP_Sales_Scaled", "Other_Sales_Scaled", "Global_Sales_Scaled"))
robust_data <- subset(vg_dataset, select = c("NA_Sales_RobustS", "EU_Sales_RobustS", "JP_Sales_RobustS", "Other_Sales_RobustS", "Global_Sales_RobustS"))	

color_heatmap <- colorRampPalette(c("cyan", "deeppink3"))
heatmap(as.matrix(corr_data[,-1]), Rowv = NA, Colv = NA, col = color_heatmap(100))

#------------------------------------



#----------EDA--------------------
#Function to get Mode
Mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#check variation
var(vg_dataset)

#chack quantile values (Numeric values)
quantile(vg_dataset$NA_Sales)
quantile(vg_dataset$EU_Sales)
quantile(vg_dataset$JP_Sales)
quantile(vg_dataset$Other_Sales)
quantile(vg_dataset$Global_Sales)


#code retrieved from https://stackoverflow.com/questions/48728322/how-to-change-colors-on-barplot/48728672
# colors progressing from gray to dark blue, and checking how many items on next line
colorBarPlot.function <- colorRampPalette( c( "#CCCCCC" , "#104E8B" ) )
colorBarPlot.ramp <- colorBarPlot.function( n = nrow( x = genre ) )
#view color
color.ramp
#creating a 
genre$Genre_Color <-
  as.character(
    x = cut(
      x = rank( x = genre )  # used to assign order in the event of ties
      , breaks = nrow( x = genre )  # same as the 'n' supplied in color.function()
      , labels = colorBarPlot.ramp  # label the groups with the color in color.ramp
    )
  )
# making barplot
library(RColorBrewer)
coul <- brewer.pal(nrow(genre), "Set3") 
barplot( table(vg_dataset$Genre)
         , col = coul
         , border = NA
         , main = "Bar Plot of Titles per Genre Sold"
         , ylab = "Games Sold"
         , xlab = "Genre"
         , ylim = c(0,3500)
)


#reorganizing the datasets
sales_data <- subset(vg_dataset, select = c(NA_Sales, EU_Sales, JP_Sales, Other_Sales, Global_Sales))
test_vg <- vg_dataset
vg_dataset <- subset(test_vg, select = -c(Genre_Color, NA_Sales_VAR, Name_Mode, Global_Sales_Mode, Global_Sales_RobustS, Other_Sales_RobustS, JP_Sales_RobustS, EU_Sales_RobustS, NA_Sales_RobustS, Global_Sales_Scaled, Other_Sales_Scaled, JP_Sales_Scaled, NA_Sales_Scaled, EU_Sales_Scaled, Global_Sales_Norm, Other_Sales_Norm, JP_Sales_Norm, EU_Sales_Norm, NA_Sales_Norm))
robust_vg <- subset(test_vg, select = c( NA_Sales_RobustS, EU_Sales_RobustS, Other_Sales_RobustS, JP_Sales_RobustS, Global_Sales_RobustS))
scaled_vg <- subset(test_vg, select = c(NA_Sales_Scaled, EU_Sales_Scaled, JP_Sales_Scaled, Other_Sales_Scaled, Global_Sales_Scaled))
norm_vg <- subset(test_vg, select = c( NA_Sales_Norm, EU_Sales_Norm, Other_Sales_Norm, JP_Sales_Norm, Global_Sales_Norm))
full_vg <- test_vg
rm(test_vg)
#if needed
# rm(test_vg)
# rm(corr_data)
# rm(data_melt)
# rm(robust_data)
# rm(scale_data)

#genre <- subset(vg_dataset, !duplicated(Genre))
#genre <- subset(genre, select = -c(Name, Platform, Year, Publisher, NA_Sales, EU_Sales, JP_Sales, Other_Sales, Global_Sales))


#bin histogram
ggplot(data = vg_dataset, mapping = aes(x = Year)) +
  geom_histogram(binwidth = 0.25, color="black", fill="gray")

#covariation
ggplot(data = vg_dataset, mapping = aes(x = JP_Sales)) +
  geom_freqpoly(mapping = aes(color = Genre), binwidth = 500)	

#BoxPlot
ggplot(data = vg_dataset, mapping = aes(x = Platform, y = JP_Sales)) +
  geom_boxplot()+
  #coord_flip()+
  labs(y="Japan Sales", title = "BoxPlot of platform titles mostly bought in Japan")+
  theme_minimal() + theme(text = element_text(size = 14))

#Stacked Bar chart of Platform by Year
vg_dataset %>%
  ggplot ( ylim = c(0,1500)) + 
  geom_bar (mapping = aes (x = Year, fill = Platform), color = "black") + 
  labs (title = "Stacked Bar chart of Platform by Year", x = "Year", y = "Number of Platform" ) +
  coord_flip() + 
  theme_minimal() + 
  theme (text = element_text (size = 14))

#Log 10 transformation
vg_dataset %>%
  + select (NA_Sales, EU_Sales, JP_Sales, Other_sales, Global_Sales) %>%
  + mutate (NA_sales_L = log10(NA_sales), EU_sales_L = log10(EU_Sales), JP_sales_L = log10(JP_Sales), Other_sales_L = log10 (Other_Sales), Global_Sales_L = log10(Global_Sales)) %>%
  + summary()


#------------------DUMMY CODE-------------------

install.packages("fastDummies")
library(fastDummies)
#new dataset for dummy data
dummy_data <- dummy_cols(vg_dataset, select_columns = "Genre")
view(dummy_data)

ggplot(vg_dataset, aes(x=Year, y=Name, color = Genre)) +
  geom_point() +
  facet_grid(~Genre)

#-----------------------------------------------


#-----------------PCA---------------------------
# Code extrated from: https://www.youtube.com/watch?v=hmp9KIPb5Ig&ab_channel=CARVALHORIBEIRO by CARVALHORIBEIRO
install.packages("FactoMineR")
install.packages("factoextra")
library(FactoMineR)
library(factoextra)

#generating PCA
res.pca <- PCA(scaled_vg, graph = F)
View(res.pca)
#Extrair a proporcao de variancia dos valores de componentes principais
eig.val <- get_eigenvalue(res.pca)
eig.val
#Bar Plot the eigenvalues | Visualize eigenvalues/variances
fviz_screeplot(res.pca,addlabels=TRUE)
barplot(eig.val[, 2], names.arg=1:nrow(eig.val), 
        main = "Variances by Component",
        xlab = "Principal Components",
        ylab = "% of variances",
        col ="goldenrod3")
#Graphic plot showing variance proportion of each variable
fviz_eig(res.pca, addlabels = T, ylim = c(0,90))
#Extrair os resultados das variaveis do PCA para usar no plot grafico
var <- get_pca_var(res.pca)
ind <- get_pca_ind(res.pca)
#Plot PCA Graph
fviz_pca_var(res.pca, col.var = "blue")
#Creating Cluster group
groupCluster <- as.factor(vg_dataset[,6])
#Plot of Graph Biplot
fviz_pca_biplot(res.pca, habillage = groupCluster, title = "Graph PCA of Sales")
#Additional Plot to check the quality of the representation with COS2 and Graph
library(corrplot)
var$cos2
corrplot(var$cos2, is.corr = F)

#Code extrated from week9_Data Exp Class by Muhammad
# Plot for individuals
ind$cos2
plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage="none", 
         col.ind="#0000ff", col.ind.sup="blue", col.quali="magenta", label=c("ind","ind.sup", "quali"),new.plot=TRUE, title=" Individuals Factor Map")



# Control variable colors using their contributions
# Use red and blue
library(ggplot2)
fviz_pca_var(res.pca, col.var="contrib") +
  scale_color_gradient2(low="red",high="blue",midpoint = 90) +
  theme_minimal()

# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))}
  if (numPlots==1) {
    print(plots[[1]]) } 
  
  else {# Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    } } }
# Variable contributions on axes 1 + 2
p1<-fviz_contrib(res.pca, choice="var", axes = 1:2)
# Individuals contributions on axes 1 + 2
p2<-fviz_contrib(res.pca, choice="ind", axes = 1:2)
multiplot(p1, p2,cols=2)

#Another customized plot, this time to view Individuals (ind)
resVG <- prcomp(scaled_vg[, -5],  scale = TRUE)
# Add Color by groups
p <- fviz_pca_ind(resVG, geom = "point",
                  habillage=vg_dataset$Genre, addEllipses=TRUE,
                  ellipse.level= 0.90)+ theme_minimal()
p

#Performs Principal Component Analysis (PCA) a Shiny application.
PCAshiny(scaled_vg) 
#-----------------------------------------------

#--------------One Hot Enconding----------------
#Code from https://datatricks.co.uk/one-hot-encoding-in-r-three-simple-methods
#one_hot in mltools package
install.packages("mltools")
library(magrittr)
library(caret)
library(mltools)
library(data.table)
oneHotData <- subset(vg_dataset,select =  c(Index, Genre, Year))
newData  <- one_hot(as.data.table(oneHotData))
#dummyVars in caret package
dummy <- dummyVars(" ~ .", data=oneHotData)
newdata <- data.frame(predict(dummy, newData = oneHotData)) 
#dcast in reshape2 package
library(reshape2)
newdata <- dcast(data = oneHotData, Year ~ Genre, length)
#-----------------------------------------------