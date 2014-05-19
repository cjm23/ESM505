GWCD<-read.csv("H:/ESM505/GWCD.csv")
GWCD<-GWCD[,c(1:15)]
library(dplyr)
fam<-unique(GWCD$Family)
famcount<-length(unique(GWCD$Family))
for (i in 1:famcount) {
  famdat<-subset(GWCD, Family == fam[i])
  famdat$Binomial <- as.character(famdat$Binomial)
  famdat$Binomial <- factor(famdat$Binomial)
  plot(famdat$Binomial, famdat$Wood.carbon.content....mass.basis.,)
  
}

hist(GWCD$Wood.carbon.content....mass.basis.)
hist(GWCD$Wood.carbon.content....mass.basis.,breaks=50)
library(ggplot2)
library(dplyr)
ggwcd<- ggplot(GWCD, aes(x=Family, y=Wood.carbon.content....mass.basis.)) 
p<-ggwcd+geom_boxplot()
print(p)
gggwcd<-ggplot(GWCD)
sort.GWCD<-GWCD[order(GWCD$Region),]
ggggwcd<-ggplot(sort.GWCD)

p2<-ggggwcd+geom_boxplot(aes(x=Family, y=Wood.carbon.content....mass.basis., colour=Region))
print(p2)

p3<-gggwcd+geom_boxplot(aes(x=Family, y=Wood.carbon.content....mass.basis., colour=Type))
print(p3)

p4<-gggwcd+geom_boxplot(aes(x=Family, y=Wood.carbon.content....mass.basis., colour=Type)) + facet_wrap(~Region)
print(p4)

p5<-gggwcd+geom_boxplot(aes(x=Region, y=Wood.carbon.content....mass.basis., colour=Type)) + facet_wrap(~Family)
print(p5)

p8<-gggwcd+geom_point(aes(x=Region, y=Wood.carbon.content....mass.basis., colour=Type))+ geom_smooth(aes(x=Region, y=Wood.carbon.content....mass.basis.),method = lm) + facet_wrap(~Family)
print(p8)

wb_by_famreg_plot <- function(GWCD) {
  p6 <- ggplot(GWCD, aes(x=Family, y=Wood.carbon.content....mass.basis.)) 
  p7 <- p6+geom_point()
  title(paste(first(GWCD$Region)), "Region") ##can't seem to make title appear
  print(p7)
}
GWCD %.% group_by(Region) %.% do(wb_by_famreg_plot)

GWCD<-read.csv("H:/ESM505/GWCD.csv")

byregionreturn <- function(GWCD, myregion){
  
  require(dplyr)
  require(ggplot2)
  filtered <- GWCD %.% filter(Region == myregion) %.% group_by(Family) %.% summarise(mean_wb = mean(Wood.carbon.content....mass.basis.))
  subs <- subset(GWCD, GWCD$Region == myregion)
  subs2 <- ggplot(subs)
  p2<-subs2+geom_boxplot(aes(x=Family, y=Wood.carbon.content....mass.basis., colour=Region))
  print(p2)
  return(filtered)
}

regions <- unique(GWCD$Region)
print(regions)
myregion <- "Africa"
byregionreturn(GWCD,myregion)

region<-function(){
  regions <- unique(GWCD$Region)
  print("Choose region from list:")
  return(regions)
}  
region()  


bybiomereturn <- function(GWCD, biome){
  
  require(dplyr)
  require(ggplot2)
  filtered <- GWCD %.% filter(Biome == biome) %.% group_by(Family) %.% summarise(mean_wb = mean(Wood.carbon.content....mass.basis.))
  subs <- subset(GWCD, GWCD$Biome == biome)
  subs2 <- ggplot(subs)
  p2<-subs2+geom_boxplot(aes(x=Family, y=Wood.carbon.content....mass.basis., colour=Region))
  print(p2)
  return(filtered)
}


bybiomereturn(GWCD, "tropical")

biome<-function(){
  biomes <- unique(GWCD$Biome)
  print("Choose biome from list:")
  return(biomes)
}  
biome()