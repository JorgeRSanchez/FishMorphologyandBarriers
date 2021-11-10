


## MORPHOMETRIC ANALYSIS 



### PRincipal COmponent Analysis 



data01<-read.table ('../data/weightmat_v.2.txt', header=T, row.names=1, sep = "\t", na.strings="NA", strip.white=TRUE)
attach (data01)
names (data01)
summary (data01)
data01




pcacov1<-princomp(data01, cor=FALSE)
summary(pcacov1)
pcacov1$scores [1:32,1:5]
biplot(pcacov1)
plot(pcacov1, main="")
result1<-pcacov1$scores[1:32,1:16]
write.table(result1, "exports/resultPCA_v.2.txt")




## Basic 3D Scatter Plot  


data01 <- read.table("../data/data_boga2num.txt",header=TRUE,sep="",na.strings="NA", dec=".",strip.white=TRUE, row.names=1)
attach(data01)
names(data01)
summary(data01)




#install.packages("plotly")
library(plotly)
citation("plotly")




data01$response15[which(data01$response15 == 1)] <- 'Positive'
data01$response15[which(data01$response15 == 0)] <- 'Negative'
data01$response15<- as.factor(data01$response15)



fig <- plot_ly(data01, x = ~comp_1, y = ~comp_2, z = ~comp_3, 
               color = ~response15, colors = c('#BF382A', '#0C4B8E'))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'PC 1'),
                                   yaxis = list(title = 'PC 2'),
                                   zaxis = list(title = 'PC 3')))
fig


  
data01$response25[which(data01$response25 == 1)] <- 'Positive'
data01$response25[which(data01$response25 == 0)] <- 'Negative'
data01$response25<- as.factor(data01$response25)



fig <- plot_ly(data01, x = ~comp_1, y = ~comp_2, z = ~comp_3, 
               color = ~response25, colors = c('#BF382A', '#0C4B8E'))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'PC 1'),
                                   yaxis = list(title = 'PC 2'),
                                   zaxis = list(title = 'PC 3')))
fig



data01$response30[which(data01$response30 == 1)] <- 'Positive'
data01$response30[which(data01$response30 == 0)] <- 'Negative'
data01$response30<- as.factor(data01$response30)



fig <- plot_ly(data01, x = ~comp_1, y = ~comp_2, z = ~comp_3, color = ~response30, colors = c('#BF382A', '#0C4B8E'))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'PC 1'),
                                   yaxis = list(title = 'PC 2'),
                                   zaxis = list(title = 'PC 3')))
fig





#### Normality  



data <-read.table("../data/data_boga2.txt",header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE, row.names=1)
attach(data)
names(data)
summary(data)




shapiro.test(lfcm_tpsdig)
shapiro.test(hcm_tpsdig)
shapiro.test(flcm)
shapiro.test(logfl)
shapiro.test(wg)
shapiro.test(logwg)
shapiro.test(cond_fact)
shapiro.test(approx_1.5)
shapiro.test(attemp_1.5)
shapiro.test(n_attemp_1.5)
shapiro.test(fag_time_1.5)
hist(fag_time_1.5)
shapiro.test(dmax_1.5)
hist(dmax_1.5)
shapiro.test(gspeed_1.5)
hist(gspeed_1.5)
shapiro.test(sspeed_1.5)
hist(sspeed_1.5)
shapiro.test(approx_2.5)
shapiro.test(attemp_2.5)
shapiro.test(n_attemp_2.5)
shapiro.test(fag_time_2.5)
hist(fag_time_2.5)
shapiro.test(dmax_2.5)
hist(dmax_2.5)
shapiro.test(gspeed_2.5)
hist(gspeed_2.5)
shapiro.test(sspeed_2.5)
hist(sspeed_2.5)
shapiro.test(approx_3)
shapiro.test(attemp_3)
shapiro.test(n_attemp_3)
shapiro.test(fag_time_3)
hist(fag_time_3)
shapiro.test(dmax_3)
hist(dmax_3)
shapiro.test(gspeed_3)
hist(gspeed_3)
shapiro.test(sspeed_3)
hist(sspeed_3)


shapiro.test(csize)
shapiro.test(X1X)
shapiro.test(X1Y)
shapiro.test(X2X)
shapiro.test(X2Y)
shapiro.test(X3X)
shapiro.test(X3Y)
shapiro.test(X4X)
shapiro.test(X4Y)
shapiro.test(X5X)
shapiro.test(X5Y)
shapiro.test(X6X)
shapiro.test(X6Y)
shapiro.test(X7X)
shapiro.test(X7Y)
shapiro.test(UniX)
shapiro.test(UniY)


shapiro.test(comp_1)
shapiro.test(comp_2)
shapiro.test(comp_3)
shapiro.test(comp_4)
shapiro.test(comp_5)
shapiro.test(comp_6)
shapiro.test(comp_7)
shapiro.test(comp_8)
shapiro.test(comp_9)
shapiro.test(comp_10)
shapiro.test(comp_11)
shapiro.test(comp_12)
shapiro.test(comp_13)
shapiro.test(comp_14)
shapiro.test(comp_15)
shapiro.test(comp_16)
  

#### Normality by Treatments


data01 <-read.table("../data/data_boga2.txt",header=TRUE, sep="", na.strings="NA",dec=".", strip.white=TRUE, row.names=1)
attach(data01)
names(data01)
summary(data01)




data02<-subset(data01, response=="yes")
attach(data02)
data02
summary(data02)




shapiro.test(lfcm_tpsdig)
shapiro.test(hcm_tpsdig)
shapiro.test(flcm)
shapiro.test(logfl)
shapiro.test(wg)
shapiro.test(logwg)
shapiro.test(cond_fact)
shapiro.test(csize)
shapiro.test(approx_1.5)
shapiro.test(attemp_1.5)
shapiro.test(n_attemp_1.5)
shapiro.test(fag_time_1.5)
shapiro.test(dmax_1.5)
shapiro.test(gspeed_1.5)
shapiro.test(sspeed_1.5)
shapiro.test(approx_2.5)
shapiro.test(attemp_2.5)
shapiro.test(n_attemp_2.5)
shapiro.test(fag_time_2.5)
shapiro.test(dmax_2.5)
shapiro.test(gspeed_2.5)
shapiro.test(sspeed_2.5)
shapiro.test(approx_3)
shapiro.test(attemp_3)
shapiro.test(n_attemp_3)
shapiro.test(fag_time_3)
shapiro.test(dmax_3)
shapiro.test(gspeed_3)
shapiro.test(sspeed_3)


shapiro.test(X1X)
shapiro.test(X1Y)
shapiro.test(X2X)
shapiro.test(X2Y)
shapiro.test(X3X)
shapiro.test(X3Y)
shapiro.test(X4X)
shapiro.test(X4Y)
shapiro.test(X5X)
shapiro.test(X5Y)
shapiro.test(X6X)
shapiro.test(X6Y)
shapiro.test(X7X)
shapiro.test(X7Y)
shapiro.test(UniX)
shapiro.test(UniY)


shapiro.test(comp_1)
shapiro.test(comp_2)
shapiro.test(comp_3)
shapiro.test(comp_4)
shapiro.test(comp_5)
shapiro.test(comp_6)
shapiro.test(comp_7)
shapiro.test(comp_8)
shapiro.test(comp_9)
shapiro.test(comp_10)
shapiro.test(comp_11)
shapiro.test(comp_12)
shapiro.test(comp_13)
shapiro.test(comp_14)
shapiro.test(comp_15)
shapiro.test(comp_16)



data02<-subset(data01, response=="no")
attach(data02)
data02
summary(data02)



shapiro.test(lfcm_tpsdig)
shapiro.test(hcm_tpsdig)
shapiro.test(flcm)
shapiro.test(logfl)
shapiro.test(wg)
shapiro.test(logwg)
shapiro.test(cond_fact)
shapiro.test(csize)
#shapiro.test(approx_1.5)
#shapiro.test(attemp_1.5)
#shapiro.test(n_attemp_1.5)
#shapiro.test(fag_time_1.5)
#shapiro.test(dmax_1.5)
#shapiro.test(gspeed_1.5)
#shapiro.test(sspeed_1.5)
shapiro.test(approx_2.5)
#shapiro.test(attemp_2.5)
#shapiro.test(n_attemp_2.5)
#shapiro.test(fag_time_2.5)
#shapiro.test(dmax_2.5)
#shapiro.test(gspeed_2.5)
#shapiro.test(sspeed_2.5)
#shapiro.test(approx_3)
#shapiro.test(dmax_3)
#shapiro.test(gspeed_3)
#shapiro.test(sspeed_3)


shapiro.test(X1X)
shapiro.test(X1Y)
shapiro.test(X2X)
shapiro.test(X2Y)
shapiro.test(X3X)
shapiro.test(X3Y)
shapiro.test(X4X)
shapiro.test(X4Y)
shapiro.test(X5X)
shapiro.test(X5Y)
shapiro.test(X6X)
shapiro.test(X6Y)
shapiro.test(X7X)
shapiro.test(X7Y)
shapiro.test(UniX)
shapiro.test(UniY)


shapiro.test(comp_1)
shapiro.test(comp_2)
shapiro.test(comp_3)
shapiro.test(comp_4)
shapiro.test(comp_5)
shapiro.test(comp_6)
shapiro.test(comp_7)
shapiro.test(comp_8)
shapiro.test(comp_9)
shapiro.test(comp_10)
shapiro.test(comp_11)
shapiro.test(comp_12)
shapiro.test(comp_13)
shapiro.test(comp_14)
shapiro.test(comp_15)
shapiro.test(comp_16)



  
data02<-subset(data01, response15=="yes")
attach(data02)
data02
summary(data02)




shapiro.test(lfcm_tpsdig)
shapiro.test(hcm_tpsdig)
shapiro.test(flcm)
shapiro.test(logfl)
shapiro.test(wg)
shapiro.test(logwg)
shapiro.test(cond_fact)
shapiro.test(csize)
shapiro.test(approx_1.5)
#shapiro.test(attemp_1.5)
#shapiro.test(n_attemp_1.5)
#shapiro.test(fag_time_1.5)
shapiro.test(dmax_1.5)
shapiro.test(gspeed_1.5)
shapiro.test(sspeed_1.5)
shapiro.test(approx_2.5)
#shapiro.test(attemp_2.5)
#shapiro.test(n_attemp_2.5)
#shapiro.test(fag_time_2.5)
shapiro.test(dmax_2.5)
shapiro.test(gspeed_2.5)
shapiro.test(sspeed_2.5)
shapiro.test(approx_3)
#shapiro.test(attemp_3)
#shapiro.test(n_attemp_3)
#shapiro.test(fag_time_3)
#shapiro.test(dmax_3)
#shapiro.test(gspeed_3)
#shapiro.test(sspeed_3)


shapiro.test(X1X)
shapiro.test(X1Y)
shapiro.test(X2X)
shapiro.test(X2Y)
shapiro.test(X3X)
shapiro.test(X3Y)
shapiro.test(X4X)
shapiro.test(X4Y)
shapiro.test(X5X)
shapiro.test(X5Y)
shapiro.test(X6X)
shapiro.test(X6Y)
shapiro.test(X7X)
shapiro.test(X7Y)
shapiro.test(UniX)
shapiro.test(UniY)


shapiro.test(comp_1)
shapiro.test(comp_2)
shapiro.test(comp_3)
shapiro.test(comp_4)
shapiro.test(comp_5)
shapiro.test(comp_6)
shapiro.test(comp_7)
shapiro.test(comp_8)
shapiro.test(comp_9)
shapiro.test(comp_10)
shapiro.test(comp_11)
shapiro.test(comp_12)
shapiro.test(comp_13)
shapiro.test(comp_14)
shapiro.test(comp_15)
shapiro.test(comp_16)



  
data02<-subset(data01, response15=="no")
attach(data02)
data02
summary(data02)



shapiro.test(lfcm_tpsdig)
shapiro.test(hcm_tpsdig)
shapiro.test(flcm)
shapiro.test(logfl)
shapiro.test(wg)
shapiro.test(logwg)
shapiro.test(cond_fact)
shapiro.test(csize)
#shapiro.test(approx_1.5)
#shapiro.test(attemp_1.5)
#shapiro.test(n_attemp_1.5)
#shapiro.test(fag_time_1.5)
#shapiro.test(dmax_1.5)
#shapiro.test(gspeed_1.5)
#shapiro.test(sspeed_1.5)
shapiro.test(approx_2.5)
shapiro.test(attemp_2.5)
shapiro.test(n_attemp_2.5)
shapiro.test(fag_time_2.5)
shapiro.test(dmax_2.5)
shapiro.test(gspeed_2.5)
shapiro.test(sspeed_2.5)
shapiro.test(approx_3)
shapiro.test(attemp_3)
shapiro.test(n_attemp_3)
shapiro.test(fag_time_3)
shapiro.test(dmax_3)
shapiro.test(gspeed_3)
shapiro.test(sspeed_3)


shapiro.test(X1X)
shapiro.test(X1Y)
shapiro.test(X2X)
shapiro.test(X2Y)
shapiro.test(X3X)
shapiro.test(X3Y)
shapiro.test(X4X)
shapiro.test(X4Y)
shapiro.test(X5X)
shapiro.test(X5Y)
shapiro.test(X6X)
shapiro.test(X6Y)
shapiro.test(X7X)
shapiro.test(X7Y)
shapiro.test(UniX)
shapiro.test(UniY)


shapiro.test(comp_1)
shapiro.test(comp_2)
shapiro.test(comp_3)
shapiro.test(comp_4)
shapiro.test(comp_5)
shapiro.test(comp_6)
shapiro.test(comp_7)
shapiro.test(comp_8)
shapiro.test(comp_9)
shapiro.test(comp_10)
shapiro.test(comp_11)
shapiro.test(comp_12)
shapiro.test(comp_13)
shapiro.test(comp_14)
shapiro.test(comp_15)
shapiro.test(comp_16)



  
data02<-subset(data01, response25=="yes")
attach(data02)
data02
summary(data02)


A continuación se realiza el test  


shapiro.test(lfcm_tpsdig)
shapiro.test(hcm_tpsdig)
shapiro.test(flcm)
shapiro.test(logfl)
shapiro.test(wg)
shapiro.test(logwg)
shapiro.test(cond_fact)
shapiro.test(csize)
shapiro.test(approx_1.5)
shapiro.test(attemp_1.5)
shapiro.test(n_attemp_1.5)
shapiro.test(fag_time_1.5)
shapiro.test(dmax_1.5)
shapiro.test(gspeed_1.5)
shapiro.test(sspeed_1.5)
shapiro.test(approx_2.5)
shapiro.test(attemp_2.5)
shapiro.test(n_attemp_2.5)
shapiro.test(fag_time_2.5)
shapiro.test(dmax_2.5)
shapiro.test(gspeed_2.5)
shapiro.test(sspeed_2.5)
shapiro.test(approx_3)
shapiro.test(attemp_3)
shapiro.test(n_attemp_3)
shapiro.test(fag_time_3)
shapiro.test(dmax_3)
shapiro.test(gspeed_3)
shapiro.test(sspeed_3)

shapiro.test(X1X)
shapiro.test(X1Y)
shapiro.test(X2X)
shapiro.test(X2Y)
shapiro.test(X3X)
shapiro.test(X3Y)
shapiro.test(X4X)
shapiro.test(X4Y)
shapiro.test(X5X)
shapiro.test(X5Y)
shapiro.test(X6X)
shapiro.test(X6Y)
shapiro.test(X7X)
shapiro.test(X7Y)
shapiro.test(UniX)
shapiro.test(UniY)


shapiro.test(comp_1)
shapiro.test(comp_2)
shapiro.test(comp_3)
shapiro.test(comp_4)
shapiro.test(comp_5)
shapiro.test(comp_6)
shapiro.test(comp_7)
shapiro.test(comp_8)
shapiro.test(comp_9)
shapiro.test(comp_10)
shapiro.test(comp_11)
shapiro.test(comp_12)
shapiro.test(comp_13)
shapiro.test(comp_14)
shapiro.test(comp_15)
shapiro.test(comp_16)


  
data02<-subset(data01, response25=="no")
attach(data02)
data02
summary(data02)


A continuación se realiza el test  


shapiro.test(lfcm_tpsdig)
shapiro.test(hcm_tpsdig)
shapiro.test(flcm)
shapiro.test(logfl)
shapiro.test(wg)
shapiro.test(logwg)
shapiro.test(cond_fact)
shapiro.test(csize)
#shapiro.test(approx_1.5)
shapiro.test(attemp_1.5)
shapiro.test(n_attemp_1.5)
shapiro.test(fag_time_1.5)
shapiro.test(dmax_1.5)
shapiro.test(gspeed_1.5)
shapiro.test(sspeed_1.5)
shapiro.test(approx_2.5)
#shapiro.test(attemp_2.5)
#shapiro.test(n_attemp_2.5)
#shapiro.test(fag_time_2.5)
#shapiro.test(dmax_2.5)
#shapiro.test(gspeed_2.5)
#shapiro.test(sspeed_2.5)
shapiro.test(approx_3)
shapiro.test(attemp_3)
shapiro.test(n_attemp_3)
shapiro.test(fag_time_3)
shapiro.test(dmax_3)
shapiro.test(gspeed_3)
shapiro.test(sspeed_3)


shapiro.test(X1X)
shapiro.test(X1Y)
shapiro.test(X2X)
shapiro.test(X2Y)
shapiro.test(X3X)
shapiro.test(X3Y)
shapiro.test(X4X)
shapiro.test(X4Y)
shapiro.test(X5X)
shapiro.test(X5Y)
shapiro.test(X6X)
shapiro.test(X6Y)
shapiro.test(X7X)
shapiro.test(X7Y)
shapiro.test(UniX)
shapiro.test(UniY)


shapiro.test(comp_1)
shapiro.test(comp_2)
shapiro.test(comp_3)
shapiro.test(comp_4)
shapiro.test(comp_5)
shapiro.test(comp_6)
shapiro.test(comp_7)
shapiro.test(comp_8)
shapiro.test(comp_9)
shapiro.test(comp_10)
shapiro.test(comp_11)
shapiro.test(comp_12)
shapiro.test(comp_13)
shapiro.test(comp_14)
shapiro.test(comp_15)
shapiro.test(comp_16)


  
data02<-subset(data01, response30=="yes")
attach(data02)
data02
summary(data02)


A continuación se realiza el test  


shapiro.test(lfcm_tpsdig)
shapiro.test(hcm_tpsdig)
shapiro.test(flcm)
shapiro.test(logfl)
shapiro.test(wg)
shapiro.test(logwg)
shapiro.test(cond_fact)
shapiro.test(csize)
#shapiro.test(approx_1.5)
#shapiro.test(attemp_1.5)
#shapiro.test(n_attemp_1.5)
#shapiro.test(fag_time_1.5)
#shapiro.test(dmax_1.5)
#shapiro.test(gspeed_1.5)
#shapiro.test(sspeed_1.5)
shapiro.test(approx_2.5)
shapiro.test(attemp_2.5)
shapiro.test(n_attemp_2.5)
shapiro.test(fag_time_2.5)
shapiro.test(dmax_2.5)
shapiro.test(gspeed_2.5)
shapiro.test(sspeed_2.5)
shapiro.test(approx_3)
#shapiro.test(attemp_3)
#shapiro.test(n_attemp_3)
shapiro.test(fag_time_3)
shapiro.test(dmax_3)
shapiro.test(gspeed_3)
shapiro.test(sspeed_3)

shapiro.test(X1X)
shapiro.test(X1Y)
shapiro.test(X2X)
shapiro.test(X2Y)
shapiro.test(X3X)
shapiro.test(X3Y)
shapiro.test(X4X)
shapiro.test(X4Y)
shapiro.test(X5X)
shapiro.test(X5Y)
shapiro.test(X6X)
shapiro.test(X6Y)
shapiro.test(X7X)
shapiro.test(X7Y)
shapiro.test(UniX)
shapiro.test(UniY)


shapiro.test(comp_1)
shapiro.test(comp_2)
shapiro.test(comp_3)
shapiro.test(comp_4)
shapiro.test(comp_5)
shapiro.test(comp_6)
shapiro.test(comp_7)
shapiro.test(comp_8)
shapiro.test(comp_9)
shapiro.test(comp_10)
shapiro.test(comp_11)
shapiro.test(comp_12)
shapiro.test(comp_13)
shapiro.test(comp_14)
shapiro.test(comp_15)
shapiro.test(comp_16)



  
data02<-subset(data01, response30=="no")
attach(data02)
data02
summary(data02)


A continuación se realiza el test  


shapiro.test(lfcm_tpsdig)
shapiro.test(hcm_tpsdig)
shapiro.test(flcm)
shapiro.test(logfl)
shapiro.test(wg)
shapiro.test(logwg)
shapiro.test(cond_fact)
shapiro.test(csize)
shapiro.test(approx_1.5)
shapiro.test(attemp_1.5)
shapiro.test(n_attemp_1.5)
shapiro.test(fag_time_1.5)
shapiro.test(dmax_1.5)
shapiro.test(gspeed_1.5)
shapiro.test(sspeed_1.5)
shapiro.test(approx_2.5)
shapiro.test(attemp_2.5)
shapiro.test(n_attemp_2.5)
shapiro.test(fag_time_2.5)
shapiro.test(dmax_2.5)
shapiro.test(gspeed_2.5)
shapiro.test(sspeed_2.5)
shapiro.test(approx_3)
shapiro.test(attemp_3)
shapiro.test(n_attemp_3)
shapiro.test(fag_time_3)
shapiro.test(dmax_3)
shapiro.test(gspeed_3)
shapiro.test(sspeed_3)

shapiro.test(X1X)
shapiro.test(X1Y)
shapiro.test(X2X)
shapiro.test(X2Y)
shapiro.test(X3X)
shapiro.test(X3Y)
shapiro.test(X4X)
shapiro.test(X4Y)
shapiro.test(X5X)
shapiro.test(X5Y)
shapiro.test(X6X)
shapiro.test(X6Y)
shapiro.test(X7X)
shapiro.test(X7Y)
shapiro.test(UniX)
shapiro.test(UniY)


shapiro.test(comp_1)
shapiro.test(comp_2)
shapiro.test(comp_3)
shapiro.test(comp_4)
shapiro.test(comp_5)
shapiro.test(comp_6)
shapiro.test(comp_7)
shapiro.test(comp_8)
shapiro.test(comp_9)
shapiro.test(comp_10)
shapiro.test(comp_11)
shapiro.test(comp_12)
shapiro.test(comp_13)
shapiro.test(comp_14)
shapiro.test(comp_15)
shapiro.test(comp_16)



#### Homogenity of variances  



data <-read.table("../data/data_boga2.txt",header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE, row.names=1)
attach(data)
names(data)
summary(data)

knitr::opts_chunk$set(echo = TRUE)


  

#install.packages("car")



  

library(car)




leveneTest(lfcm_tpsdig,response)
leveneTest(hcm_tpsdig,response)
leveneTest(flcm,response)
leveneTest(logfl,response)
leveneTest(wg,response)
leveneTest(logwg,response)
leveneTest(cond_fact,response)
leveneTest(csize,response)
leveneTest(flow_vel_1.5,response)
leveneTest(temp_w_1.5,response)
leveneTest(approx_1.5,response)
leveneTest(attemp_1.5,response)
leveneTest(n_attemp_1.5,response)
leveneTest(fag_time_1.5,response)
leveneTest(dmax_1.5,response)
leveneTest(gspeed_1.5,response)
leveneTest(sspeed_1.5,response)
leveneTest(flow_vel_2.5,response)
leveneTest(temp_w_2.5,response)
leveneTest(approx_2.5,response)
leveneTest(attemp_2.5,response)
leveneTest(n_attemp_2.5,response)
leveneTest(fag_time_2.5,response)
leveneTest(dmax_2.5,response)
leveneTest(gspeed_2.5,response)
leveneTest(sspeed_2.5,response)
leveneTest(flow_vel_3,response)
leveneTest(temp_w_3,response)
leveneTest(approx_3,response)
leveneTest(attemp_3,response)
leveneTest(n_attemp_3,response)
leveneTest(fag_time_3,response)
leveneTest(dmax_3,response)
leveneTest(gspeed_3,response)
leveneTest(sspeed_3,response)


leveneTest(X1X,response)
leveneTest(X1Y,response)
leveneTest(X2X,response)
leveneTest(X2Y,response)
leveneTest(X3X,response)
leveneTest(X3Y,response)
leveneTest(X4X,response)
leveneTest(X4Y,response)
leveneTest(X5X,response)
leveneTest(X5Y,response)
leveneTest(X6X,response)
leveneTest(X6Y,response)
leveneTest(X7X,response)
leveneTest(X7Y,response)
leveneTest(UniX,response)
leveneTest(UniY,response)


leveneTest(comp_1,response)
leveneTest(comp_2,response)
leveneTest(comp_3,response)
leveneTest(comp_4,response)
leveneTest(comp_5,response)
leveneTest(comp_6,response)
leveneTest(comp_7,response)
leveneTest(comp_8,response)
leveneTest(comp_9,response)
leveneTest(comp_10,response)
leveneTest(comp_11,response)
leveneTest(comp_12,response)
leveneTest(comp_13,response)
leveneTest(comp_14,response)
leveneTest(comp_15,response)
leveneTest(comp_16,response)




bartlett.test(lfcm_tpsdig,response)
bartlett.test(hcm_tpsdig,response)
bartlett.test(flcm,response)
bartlett.test(logfl,response)
bartlett.test(wg,response)
bartlett.test(logwg,response)
bartlett.test(cond_fact,response)
bartlett.test(csize,response)
bartlett.test(flow_vel_1.5,response)
bartlett.test(temp_w_1.5,response)
bartlett.test(approx_1.5,response)
bartlett.test(attemp_1.5,response)
bartlett.test(n_attemp_1.5,response)
bartlett.test(fag_time_1.5,response)
bartlett.test(dmax_1.5,response)
bartlett.test(gspeed_1.5,response)
bartlett.test(sspeed_1.5,response)
bartlett.test(flow_vel_2.5,response)
bartlett.test(temp_w_2.5,response)
bartlett.test(approx_2.5,response)
bartlett.test(attemp_2.5,response)
bartlett.test(n_attemp_2.5,response)
bartlett.test(fag_time_2.5,response)
bartlett.test(dmax_2.5,response)
bartlett.test(gspeed_2.5,response)
bartlett.test(sspeed_2.5,response)
bartlett.test(flow_vel_3,response)
bartlett.test(temp_w_3,response)
bartlett.test(approx_3,response)
bartlett.test(attemp_3,response)
bartlett.test(n_attemp_3,response)
bartlett.test(fag_time_3,response)
bartlett.test(dmax_3,response)
bartlett.test(gspeed_3,response)
bartlett.test(sspeed_3,response)


bartlett.test(X1X,response)
bartlett.test(X1Y,response)
bartlett.test(X2X,response)
bartlett.test(X2Y,response)
bartlett.test(X3X,response)
bartlett.test(X3Y,response)
bartlett.test(X4X,response)
bartlett.test(X4Y,response)
bartlett.test(X5X,response)
bartlett.test(X5Y,response)
bartlett.test(X6X,response)
bartlett.test(X6Y,response)
bartlett.test(X7X,response)
bartlett.test(X7Y,response)
bartlett.test(UniX,response)
bartlett.test(UniY,response)


bartlett.test(comp_1,response)
bartlett.test(comp_2,response)
bartlett.test(comp_3,response)
bartlett.test(comp_4,response)
bartlett.test(comp_5,response)
bartlett.test(comp_6,response)
bartlett.test(comp_7,response)
bartlett.test(comp_8,response)
bartlett.test(comp_9,response)
bartlett.test(comp_10,response)
bartlett.test(comp_11,response)
bartlett.test(comp_12,response)
bartlett.test(comp_13,response)
bartlett.test(comp_14,response)
bartlett.test(comp_15,response)
bartlett.test(comp_16,response)




fligner.test(lfcm_tpsdig,response)
fligner.test(hcm_tpsdig,response)
fligner.test(flcm,response)
fligner.test(logfl,response)
fligner.test(wg,response)
fligner.test(logwg,response)
fligner.test(cond_fact,response)
fligner.test(csize,response)
fligner.test(flow_vel_1.5,response)
fligner.test(temp_w_1.5,response)
fligner.test(approx_1.5,response)
fligner.test(attemp_1.5,response)
fligner.test(n_attemp_1.5,response)
fligner.test(fag_time_1.5,response)
fligner.test(dmax_1.5,response)
fligner.test(gspeed_1.5,response)
fligner.test(sspeed_1.5,response)
fligner.test(flow_vel_2.5,response)
fligner.test(temp_w_2.5,response)
fligner.test(approx_2.5,response)
fligner.test(attemp_2.5,response)
fligner.test(n_attemp_2.5,response)
fligner.test(fag_time_2.5,response)
fligner.test(dmax_2.5,response)
fligner.test(gspeed_2.5,response)
fligner.test(sspeed_2.5,response)
fligner.test(flow_vel_3,response)
fligner.test(temp_w_3,response)
fligner.test(approx_3,response)
fligner.test(attemp_3,response)
fligner.test(n_attemp_3,response)
fligner.test(fag_time_3,response)
fligner.test(dmax_3,response)
fligner.test(gspeed_3,response)
fligner.test(sspeed_3,response)


fligner.test(X1X,response)
fligner.test(X1Y,response)
fligner.test(X2X,response)
fligner.test(X2Y,response)
fligner.test(X3X,response)
fligner.test(X3Y,response)
fligner.test(X4X,response)
fligner.test(X4Y,response)
fligner.test(X5X,response)
fligner.test(X5Y,response)
fligner.test(X6X,response)
fligner.test(X6Y,response)
fligner.test(X7X,response)
fligner.test(X7Y,response)
fligner.test(UniX,response)
fligner.test(UniY,response)


fligner.test(comp_1,response)
fligner.test(comp_2,response)
fligner.test(comp_3,response)
fligner.test(comp_4,response)
fligner.test(comp_5,response)
fligner.test(comp_6,response)
fligner.test(comp_7,response)
fligner.test(comp_8,response)
fligner.test(comp_9,response)
fligner.test(comp_10,response)
fligner.test(comp_11,response)
fligner.test(comp_12,response)
fligner.test(comp_13,response)
fligner.test(comp_14,response)
fligner.test(comp_15,response)
fligner.test(comp_16,response)



  
leveneTest(lfcm_tpsdig,response15)
leveneTest(hcm_tpsdig,response15)
leveneTest(flcm,response15)
leveneTest(logfl,response15)
leveneTest(wg,response15)
leveneTest(logwg,response15)
leveneTest(cond_fact,response15)
leveneTest(csize,response15)
leveneTest(flow_vel_1.5,response15)
leveneTest(temp_w_1.5,response15)
leveneTest(approx_1.5,response15)
leveneTest(attemp_1.5,response15)
leveneTest(n_attemp_1.5,response15)
leveneTest(fag_time_1.5,response15)
leveneTest(dmax_1.5,response15)
leveneTest(gspeed_1.5,response15)
leveneTest(sspeed_1.5,response15)
leveneTest(flow_vel_2.5,response15)
leveneTest(temp_w_2.5,response15)
leveneTest(approx_2.5,response15)
leveneTest(attemp_2.5,response15)
leveneTest(n_attemp_2.5,response15)
leveneTest(fag_time_2.5,response15)
leveneTest(dmax_2.5,response15)
leveneTest(gspeed_2.5,response15)
leveneTest(sspeed_2.5,response15)
leveneTest(flow_vel_3,response15)
leveneTest(temp_w_3,response15)
leveneTest(approx_3,response15)
leveneTest(attemp_3,response15)
leveneTest(n_attemp_3,response15)
leveneTest(fag_time_3,response15)
leveneTest(dmax_3,response15)
leveneTest(gspeed_3,response15)
leveneTest(sspeed_3,response15)


leveneTest(X1X,response15)
leveneTest(X1Y,response15)
leveneTest(X2X,response15)
leveneTest(X2Y,response15)
leveneTest(X3X,response15)
leveneTest(X3Y,response15)
leveneTest(X4X,response15)
leveneTest(X4Y,response15)
leveneTest(X5X,response15)
leveneTest(X5Y,response15)
leveneTest(X6X,response15)
leveneTest(X6Y,response15)
leveneTest(X7X,response15)
leveneTest(X7Y,response15)
leveneTest(UniX,response15)
leveneTest(UniY,response15)


leveneTest(comp_1,response15)
leveneTest(comp_2,response15)
leveneTest(comp_3,response15)
leveneTest(comp_4,response15)
leveneTest(comp_5,response15)
leveneTest(comp_6,response15)
leveneTest(comp_7,response15)
leveneTest(comp_8,response15)
leveneTest(comp_9,response15)
leveneTest(comp_10,response15)
leveneTest(comp_11,response15)
leveneTest(comp_12,response15)
leveneTest(comp_13,response15)
leveneTest(comp_14,response15)
leveneTest(comp_15,response15)
leveneTest(comp_16,response15)


  
bartlett.test(lfcm_tpsdig,response15)
bartlett.test(hcm_tpsdig,response15)
bartlett.test(flcm,response15)
bartlett.test(logfl,response15)
bartlett.test(wg,response15)
bartlett.test(logwg,response15)
bartlett.test(cond_fact,response15)
bartlett.test(csize,response15)
bartlett.test(flow_vel_1.5,response15)
bartlett.test(temp_w_1.5,response15)
bartlett.test(approx_1.5,response15)
bartlett.test(attemp_1.5,response15)
bartlett.test(n_attemp_1.5,response15)
bartlett.test(fag_time_1.5,response15)
bartlett.test(dmax_1.5,response15)
bartlett.test(gspeed_1.5,response15)
bartlett.test(sspeed_1.5,response15)
bartlett.test(flow_vel_2.5,response15)
bartlett.test(temp_w_2.5,response15)
bartlett.test(approx_2.5,response15)
bartlett.test(attemp_2.5,response15)
bartlett.test(n_attemp_2.5,response15)
bartlett.test(fag_time_2.5,response15)
bartlett.test(dmax_2.5,response15)
bartlett.test(gspeed_2.5,response15)
bartlett.test(sspeed_2.5,response15)
bartlett.test(flow_vel_3,response15)
bartlett.test(temp_w_3,response15)
bartlett.test(approx_3,response15)
bartlett.test(attemp_3,response15)
bartlett.test(n_attemp_3,response15)
bartlett.test(fag_time_3,response15)
bartlett.test(dmax_3,response15)
bartlett.test(gspeed_3,response15)
bartlett.test(sspeed_3,response15)


bartlett.test(X1X,response15)
bartlett.test(X1Y,response15)
bartlett.test(X2X,response15)
bartlett.test(X2Y,response15)
bartlett.test(X3X,response15)
bartlett.test(X3Y,response15)
bartlett.test(X4X,response15)
bartlett.test(X4Y,response15)
bartlett.test(X5X,response15)
bartlett.test(X5Y,response15)
bartlett.test(X6X,response15)
bartlett.test(X6Y,response15)
bartlett.test(X7X,response15)
bartlett.test(X7Y,response15)
bartlett.test(UniX,response15)
bartlett.test(UniY,response15)


bartlett.test(comp_1,response15)
bartlett.test(comp_2,response15)
bartlett.test(comp_3,response15)
bartlett.test(comp_4,response15)
bartlett.test(comp_5,response15)
bartlett.test(comp_6,response15)
bartlett.test(comp_7,response15)
bartlett.test(comp_8,response15)
bartlett.test(comp_9,response15)
bartlett.test(comp_10,response15)
bartlett.test(comp_11,response15)
bartlett.test(comp_12,response15)
bartlett.test(comp_13,response15)
bartlett.test(comp_14,response15)
bartlett.test(comp_15,response15)
bartlett.test(comp_16,response15)


  
fligner.test(lfcm_tpsdig,response15)
fligner.test(hcm_tpsdig,response15)
fligner.test(flcm,response15)
fligner.test(logfl,response15)
fligner.test(wg,response15)
fligner.test(logwg,response15)
fligner.test(cond_fact,response15)
fligner.test(csize,response15)
fligner.test(flow_vel_1.5,response15)
fligner.test(temp_w_1.5,response15)
fligner.test(approx_1.5,response15)
fligner.test(attemp_1.5,response15)
fligner.test(n_attemp_1.5,response15)
fligner.test(fag_time_1.5,response15)
fligner.test(dmax_1.5,response15)
fligner.test(gspeed_1.5,response15)
fligner.test(sspeed_1.5,response15)
fligner.test(flow_vel_2.5,response15)
fligner.test(temp_w_2.5,response15)
fligner.test(approx_2.5,response15)
fligner.test(attemp_2.5,response15)
fligner.test(n_attemp_2.5,response15)
fligner.test(fag_time_2.5,response15)
fligner.test(dmax_2.5,response15)
fligner.test(gspeed_2.5,response15)
fligner.test(sspeed_2.5,response15)
fligner.test(flow_vel_3,response15)
fligner.test(temp_w_3,response15)
fligner.test(approx_3,response15)
fligner.test(attemp_3,response15)
fligner.test(n_attemp_3,response15)
fligner.test(fag_time_3,response15)
fligner.test(dmax_3,response15)
fligner.test(gspeed_3,response15)
fligner.test(sspeed_3,response15)


fligner.test(X1X,response15)
fligner.test(X1Y,response15)
fligner.test(X2X,response15)
fligner.test(X2Y,response15)
fligner.test(X3X,response15)
fligner.test(X3Y,response15)
fligner.test(X4X,response15)
fligner.test(X4Y,response15)
fligner.test(X5X,response15)
fligner.test(X5Y,response15)
fligner.test(X6X,response15)
fligner.test(X6Y,response15)
fligner.test(X7X,response15)
fligner.test(X7Y,response15)
fligner.test(UniX,response15)
fligner.test(UniY,response15)


fligner.test(comp_1,response15)
fligner.test(comp_2,response15)
fligner.test(comp_3,response15)
fligner.test(comp_4,response15)
fligner.test(comp_5,response15)
fligner.test(comp_6,response15)
fligner.test(comp_7,response15)
fligner.test(comp_8,response15)
fligner.test(comp_9,response15)
fligner.test(comp_10,response15)
fligner.test(comp_11,response15)
fligner.test(comp_12,response15)
fligner.test(comp_13,response15)
fligner.test(comp_14,response15)
fligner.test(comp_15,response15)
fligner.test(comp_16,response15)


  
leveneTest(lfcm_tpsdig,response25)
leveneTest(hcm_tpsdig,response25)
leveneTest(flcm,response25)
leveneTest(logfl,response25)
leveneTest(wg,response25)
leveneTest(logwg,response25)
leveneTest(cond_fact,response25)
leveneTest(csize,response25)
leveneTest(flow_vel_1.5,response25)
leveneTest(temp_w_1.5,response25)
leveneTest(approx_1.5,response25)
leveneTest(attemp_1.5,response25)
leveneTest(n_attemp_1.5,response25)
leveneTest(fag_time_1.5,response25)
leveneTest(dmax_1.5,response25)
leveneTest(gspeed_1.5,response25)
leveneTest(sspeed_1.5,response25)
leveneTest(flow_vel_2.5,response25)
leveneTest(temp_w_2.5,response25)
leveneTest(approx_2.5,response25)
leveneTest(attemp_2.5,response25)
leveneTest(n_attemp_2.5,response25)
leveneTest(fag_time_2.5,response25)
leveneTest(dmax_2.5,response25)
leveneTest(gspeed_2.5,response25)
leveneTest(sspeed_2.5,response25)
leveneTest(flow_vel_3,response25)
leveneTest(temp_w_3,response25)
leveneTest(approx_3,response25)
leveneTest(attemp_3,response25)
leveneTest(n_attemp_3,response25)
leveneTest(fag_time_3,response25)
leveneTest(dmax_3,response25)
leveneTest(gspeed_3,response25)
leveneTest(sspeed_3,response25)


leveneTest(X1X,response25)
leveneTest(X1Y,response25)
leveneTest(X2X,response25)
leveneTest(X2Y,response25)
leveneTest(X3X,response25)
leveneTest(X3Y,response25)
leveneTest(X4X,response25)
leveneTest(X4Y,response25)
leveneTest(X5X,response25)
leveneTest(X5Y,response25)
leveneTest(X6X,response25)
leveneTest(X6Y,response25)
leveneTest(X7X,response25)
leveneTest(X7Y,response25)
leveneTest(UniX,response25)
leveneTest(UniY,response25)


leveneTest(comp_1,response25)
leveneTest(comp_2,response25)
leveneTest(comp_3,response25)
leveneTest(comp_4,response25)
leveneTest(comp_5,response25)
leveneTest(comp_6,response25)
leveneTest(comp_7,response25)
leveneTest(comp_8,response25)
leveneTest(comp_9,response25)
leveneTest(comp_10,response25)
leveneTest(comp_11,response25)
leveneTest(comp_12,response25)
leveneTest(comp_13,response25)
leveneTest(comp_14,response25)
leveneTest(comp_15,response25)
leveneTest(comp_16,response25)


  
bartlett.test(lfcm_tpsdig,response25)
bartlett.test(hcm_tpsdig,response25)
bartlett.test(flcm,response25)
bartlett.test(logfl,response25)
bartlett.test(wg,response25)
bartlett.test(logwg,response25)
bartlett.test(cond_fact,response25)
bartlett.test(csize,response25)
bartlett.test(flow_vel_1.5,response25)
bartlett.test(temp_w_1.5,response25)
bartlett.test(approx_1.5,response25)
bartlett.test(attemp_1.5,response25)
bartlett.test(n_attemp_1.5,response25)
bartlett.test(fag_time_1.5,response25)
bartlett.test(dmax_1.5,response25)
bartlett.test(gspeed_1.5,response25)
bartlett.test(sspeed_1.5,response25)
bartlett.test(flow_vel_2.5,response25)
bartlett.test(temp_w_2.5,response25)
bartlett.test(approx_2.5,response25)
bartlett.test(attemp_2.5,response25)
bartlett.test(n_attemp_2.5,response25)
bartlett.test(fag_time_2.5,response25)
bartlett.test(dmax_2.5,response25)
bartlett.test(gspeed_2.5,response25)
bartlett.test(sspeed_2.5,response25)
bartlett.test(flow_vel_3,response25)
bartlett.test(temp_w_3,response25)
bartlett.test(approx_3,response25)
bartlett.test(attemp_3,response25)
bartlett.test(n_attemp_3,response25)
bartlett.test(fag_time_3,response25)
bartlett.test(dmax_3,response25)
bartlett.test(gspeed_3,response25)
bartlett.test(sspeed_3,response25)


bartlett.test(X1X,response25)
bartlett.test(X1Y,response25)
bartlett.test(X2X,response25)
bartlett.test(X2Y,response25)
bartlett.test(X3X,response25)
bartlett.test(X3Y,response25)
bartlett.test(X4X,response25)
bartlett.test(X4Y,response25)
bartlett.test(X5X,response25)
bartlett.test(X5Y,response25)
bartlett.test(X6X,response25)
bartlett.test(X6Y,response25)
bartlett.test(X7X,response25)
bartlett.test(X7Y,response25)
bartlett.test(UniX,response25)
bartlett.test(UniY,response25)


bartlett.test(comp_1,response25)
bartlett.test(comp_2,response25)
bartlett.test(comp_3,response25)
bartlett.test(comp_4,response25)
bartlett.test(comp_5,response25)
bartlett.test(comp_6,response25)
bartlett.test(comp_7,response25)
bartlett.test(comp_8,response25)
bartlett.test(comp_9,response25)
bartlett.test(comp_10,response25)
bartlett.test(comp_11,response25)
bartlett.test(comp_12,response25)
bartlett.test(comp_13,response25)
bartlett.test(comp_14,response25)
bartlett.test(comp_15,response25)
bartlett.test(comp_16,response25)


  
fligner.test(lfcm_tpsdig,response25)
fligner.test(hcm_tpsdig,response25)
fligner.test(flcm,response25)
fligner.test(logfl,response25)
fligner.test(wg,response25)
fligner.test(logwg,response25)
fligner.test(cond_fact,response25)
fligner.test(csize,response25)
fligner.test(dmax_3,response25)
fligner.test(gspeed_3,response25)
fligner.test(sspeed_3,response25)
fligner.test(flow_vel_1.5,response25)
fligner.test(temp_w_1.5,response25)
fligner.test(approx_1.5,response25)
fligner.test(attemp_1.5,response25)
fligner.test(n_attemp_1.5,response25)
fligner.test(fag_time_1.5,response25)
fligner.test(dmax_1.5,response25)
fligner.test(gspeed_1.5,response25)
fligner.test(sspeed_1.5,response25)
fligner.test(flow_vel_2.5,response25)
fligner.test(temp_w_2.5,response25)
fligner.test(approx_2.5,response25)
fligner.test(attemp_2.5,response25)
fligner.test(n_attemp_2.5,response25)
fligner.test(fag_time_2.5,response25)
fligner.test(dmax_2.5,response25)
fligner.test(gspeed_2.5,response25)
fligner.test(sspeed_2.5,response25)
fligner.test(flow_vel_3,response25)
fligner.test(temp_w_3,response25)
fligner.test(approx_3,response25)
fligner.test(attemp_3,response25)
fligner.test(n_attemp_3,response25)
fligner.test(fag_time_3,response25)


fligner.test(X1X,response25)
fligner.test(X1Y,response25)
fligner.test(X2X,response25)
fligner.test(X2Y,response25)
fligner.test(X3X,response25)
fligner.test(X3Y,response25)
fligner.test(X4X,response25)
fligner.test(X4Y,response25)
fligner.test(X5X,response25)
fligner.test(X5Y,response25)
fligner.test(X6X,response25)
fligner.test(X6Y,response25)
fligner.test(X7X,response25)
fligner.test(X7Y,response25)
fligner.test(UniX,response25)
fligner.test(UniY,response25)


fligner.test(comp_1,response25)
fligner.test(comp_2,response25)
fligner.test(comp_3,response25)
fligner.test(comp_4,response25)
fligner.test(comp_5,response25)
fligner.test(comp_6,response25)
fligner.test(comp_7,response25)
fligner.test(comp_8,response25)
fligner.test(comp_9,response25)
fligner.test(comp_10,response25)
fligner.test(comp_11,response25)
fligner.test(comp_12,response25)
fligner.test(comp_13,response25)
fligner.test(comp_14,response25)
fligner.test(comp_15,response25)
fligner.test(comp_16,response25)



  
leveneTest(lfcm_tpsdig,response30)
leveneTest(hcm_tpsdig,response30)
leveneTest(flcm,response30)
leveneTest(logfl,response30)
leveneTest(wg,response30)
leveneTest(logwg,response30)
leveneTest(cond_fact,response30)
leveneTest(csize,response30)
leveneTest(flow_vel_1.5,response30)
leveneTest(temp_w_1.5,response30)
leveneTest(approx_1.5,response30)
leveneTest(attemp_1.5,response30)
leveneTest(n_attemp_1.5,response30)
leveneTest(fag_time_1.5,response30)
leveneTest(dmax_1.5,response30)
leveneTest(gspeed_1.5,response30)
leveneTest(sspeed_1.5,response30)
leveneTest(flow_vel_2.5,response30)
leveneTest(temp_w_2.5,response30)
leveneTest(approx_2.5,response30)
leveneTest(attemp_2.5,response30)
leveneTest(n_attemp_2.5,response30)
leveneTest(fag_time_2.5,response30)
leveneTest(dmax_2.5,response30)
leveneTest(gspeed_2.5,response30)
leveneTest(sspeed_2.5,response30)
leveneTest(flow_vel_3,response30)
leveneTest(temp_w_3,response30)
leveneTest(approx_3,response30)
leveneTest(attemp_3,response30)
leveneTest(n_attemp_3,response30)
leveneTest(fag_time_3,response30)
leveneTest(dmax_3,response30)
leveneTest(gspeed_3,response30)
leveneTest(sspeed_3,response30)

leveneTest(X1X,response30)
leveneTest(X1Y,response30)
leveneTest(X2X,response30)
leveneTest(X2Y,response30)
leveneTest(X3X,response30)
leveneTest(X3Y,response30)
leveneTest(X4X,response30)
leveneTest(X4Y,response30)
leveneTest(X5X,response30)
leveneTest(X5Y,response30)
leveneTest(X6X,response30)
leveneTest(X6Y,response30)
leveneTest(X7X,response30)
leveneTest(X7Y,response30)
leveneTest(UniX,response30)
leveneTest(UniY,response30)


leveneTest(comp_1,response30)
leveneTest(comp_2,response30)
leveneTest(comp_3,response30)
leveneTest(comp_4,response30)
leveneTest(comp_5,response30)
leveneTest(comp_6,response30)
leveneTest(comp_7,response30)
leveneTest(comp_8,response30)
leveneTest(comp_9,response30)
leveneTest(comp_10,response30)
leveneTest(comp_11,response30)
leveneTest(comp_12,response30)
leveneTest(comp_13,response30)
leveneTest(comp_14,response30)
leveneTest(comp_15,response30)
leveneTest(comp_16,response30)



  
bartlett.test(lfcm_tpsdig,response30)
bartlett.test(hcm_tpsdig,response30)
bartlett.test(flcm,response30)
bartlett.test(logfl,response30)
bartlett.test(wg,response30)
bartlett.test(logwg,response30)
bartlett.test(cond_fact,response30)
bartlett.test(csize,response30)
bartlett.test(flow_vel_1.5,response30)
bartlett.test(temp_w_1.5,response30)
bartlett.test(approx_1.5,response30)
bartlett.test(attemp_1.5,response30)
bartlett.test(n_attemp_1.5,response30)
bartlett.test(fag_time_1.5,response30)
bartlett.test(dmax_1.5,response30)
bartlett.test(gspeed_1.5,response30)
bartlett.test(sspeed_1.5,response30)
bartlett.test(flow_vel_2.5,response30)
bartlett.test(temp_w_2.5,response30)
bartlett.test(approx_2.5,response30)
bartlett.test(attemp_2.5,response30)
bartlett.test(n_attemp_2.5,response30)
bartlett.test(fag_time_2.5,response30)
bartlett.test(dmax_2.5,response30)
bartlett.test(gspeed_2.5,response30)
bartlett.test(sspeed_2.5,response30)
bartlett.test(flow_vel_3,response30)
bartlett.test(temp_w_3,response30)
bartlett.test(approx_3,response30)
bartlett.test(attemp_3,response30)
bartlett.test(n_attemp_3,response30)
bartlett.test(fag_time_3,response30)
bartlett.test(dmax_3,response30)
bartlett.test(gspeed_3,response30)
bartlett.test(sspeed_3,response30)


bartlett.test(X1X,response30)
bartlett.test(X1Y,response30)
bartlett.test(X2X,response30)
bartlett.test(X2Y,response30)
bartlett.test(X3X,response30)
bartlett.test(X3Y,response30)
bartlett.test(X4X,response30)
bartlett.test(X4Y,response30)
bartlett.test(X5X,response30)
bartlett.test(X5Y,response30)
bartlett.test(X6X,response30)
bartlett.test(X6Y,response30)
bartlett.test(X7X,response30)
bartlett.test(X7Y,response30)
bartlett.test(UniX,response30)
bartlett.test(UniY,response30)


bartlett.test(comp_1,response30)
bartlett.test(comp_2,response30)
bartlett.test(comp_3,response30)
bartlett.test(comp_4,response30)
bartlett.test(comp_5,response30)
bartlett.test(comp_6,response30)
bartlett.test(comp_7,response30)
bartlett.test(comp_8,response30)
bartlett.test(comp_9,response30)
bartlett.test(comp_10,response30)
bartlett.test(comp_11,response30)
bartlett.test(comp_12,response30)
bartlett.test(comp_13,response30)
bartlett.test(comp_14,response30)
bartlett.test(comp_15,response30)
bartlett.test(comp_16,response30)


  
fligner.test(lfcm_tpsdig,response30)
fligner.test(hcm_tpsdig,response30)
fligner.test(flcm,response30)
fligner.test(logfl,response30)
fligner.test(wg,response30)
fligner.test(logwg,response30)
fligner.test(cond_fact,response30)
fligner.test(csize,response30)
fligner.test(flow_vel_1.5,response30)
fligner.test(temp_w_1.5,response30)
fligner.test(approx_1.5,response30)
fligner.test(attemp_1.5,response30)
fligner.test(n_attemp_1.5,response30)
fligner.test(fag_time_1.5,response30)
fligner.test(dmax_1.5,response30)
fligner.test(gspeed_1.5,response30)
fligner.test(sspeed_1.5,response30)
fligner.test(flow_vel_2.5,response30)
fligner.test(temp_w_2.5,response30)
fligner.test(approx_2.5,response30)
fligner.test(attemp_2.5,response30)
fligner.test(n_attemp_2.5,response30)
fligner.test(fag_time_2.5,response30)
fligner.test(dmax_2.5,response30)
fligner.test(gspeed_2.5,response30)
fligner.test(sspeed_2.5,response30)
fligner.test(flow_vel_3,response30)
fligner.test(temp_w_3,response30)
fligner.test(approx_3,response30)
fligner.test(attemp_3,response30)
fligner.test(n_attemp_3,response30)
fligner.test(fag_time_3,response30)
fligner.test(dmax_3,response30)
fligner.test(gspeed_3,response30)
fligner.test(sspeed_3,response30)


fligner.test(X1X,response30)
fligner.test(X1Y,response30)
fligner.test(X2X,response30)
fligner.test(X2Y,response30)
fligner.test(X3X,response30)
fligner.test(X3Y,response30)
fligner.test(X4X,response30)
fligner.test(X4Y,response30)
fligner.test(X5X,response30)
fligner.test(X5Y,response30)
fligner.test(X6X,response30)
fligner.test(X6Y,response30)
fligner.test(X7X,response30)
fligner.test(X7Y,response30)
fligner.test(UniX,response30)
fligner.test(UniY,response30)


fligner.test(comp_1,response30)
fligner.test(comp_2,response30)
fligner.test(comp_3,response30)
fligner.test(comp_4,response30)
fligner.test(comp_5,response30)
fligner.test(comp_6,response30)
fligner.test(comp_7,response30)
fligner.test(comp_8,response30)
fligner.test(comp_9,response30)
fligner.test(comp_10,response30)
fligner.test(comp_11,response30)
fligner.test(comp_12,response30)
fligner.test(comp_13,response30)
fligner.test(comp_14,response30)
fligner.test(comp_15,response30)
fligner.test(comp_16,response30)


#### Testing differences 



data <-read.table("../data/data_boga2.txt",header=TRUE, sep="", na.strings="NA",
                  dec=".", strip.white=TRUE, row.names=1)
attach(data)
names(data)



t.test(lfcm_tpsdig~response, var.equal =T)
t.test(hcm_tpsdig~response, var.equal =T)
t.test(flcm~response, var.equal =T)
t.test(logfl~response, var.equal =T)
t.test(wg~response, var.equal =T)
t.test(logwg~response, var.equal =T)
t.test(cond_fact~response, var.equal =T)
t.test(csize~response, var.equal =T)
#t.test(flow_vel_1.5~response, var.equal =T)
#t.test(temp_w_1.5~response, var.equal =T)
t.test(approx_1.5~response, var.equal =T)
t.test(attemp_1.5~response, var.equal =T)
t.test(n_attemp_1.5~response, var.equal =T)
t.test(fag_time_1.5~response, var.equal =T)
t.test(dmax_1.5~response, var.equal =T)
t.test(gspeed_1.5~response, var.equal =T)
t.test(sspeed_1.5~response, var.equal =T)
#t.test(flow_vel_2.5~response, var.equal =T)
#t.test(temp_w_2.5~response, var.equal =T)
t.test(approx_2.5~response, var.equal =T)
t.test(attemp_2.5~response, var.equal =T)
t.test(n_attemp_2.5~response, var.equal =T)
t.test(fag_time_2.5~response, var.equal =T)
t.test(dmax_2.5~response, var.equal =T)
t.test(gspeed_2.5~response, var.equal =T)
t.test(sspeed_2.5~response, var.equal =T)
#t.test(flow_vel_3~response, var.equal =T)
#t.test(temp_w_3~response, var.equal =T)
t.test(approx_3~response, var.equal =T)
t.test(attemp_3~response, var.equal =T)
t.test(n_attemp_3~response, var.equal =T)
t.test(fag_time_3~response, var.equal =T)
t.test(dmax_3~response, var.equal =T)
t.test(gspeed_3~response, var.equal =T)
t.test(sspeed_3~response, var.equal =T)



t.test(X1X~response, var.equal =T)
t.test(X1Y~response, var.equal =T)
t.test(X2X~response, var.equal =T)
t.test(X2Y~response, var.equal =T)
t.test(X3X~response, var.equal =T)
t.test(X3Y~response, var.equal =T)
t.test(X4X~response, var.equal =T)
t.test(X4Y~response, var.equal =T)
t.test(X5X~response, var.equal =T)
t.test(X5Y~response, var.equal =T)
t.test(X6X~response, var.equal =T)
t.test(X6Y~response, var.equal =T)
t.test(X7X~response, var.equal =T)
t.test(X7Y~response, var.equal =T)
t.test(UniX~response, var.equal =T)
t.test(UniY~response, var.equal =T)


t.test(comp_1~response, var.equal =T)
t.test(comp_2~response, var.equal =T)
t.test(comp_3~response, var.equal =T)
t.test(comp_4~response, var.equal =T)
t.test(comp_5~response, var.equal =T)
t.test(comp_6~response, var.equal =T)
t.test(comp_7~response, var.equal =T)
t.test(comp_8~response, var.equal =T)
t.test(comp_9~response, var.equal =T)
t.test(comp_10~response, var.equal =T)
t.test(comp_11~response, var.equal =T)
t.test(comp_12~response, var.equal =T)
t.test(comp_13~response, var.equal =T)
t.test(comp_14~response, var.equal =T)
t.test(comp_15~response, var.equal =T)
t.test(comp_16~response, var.equal =T)



t.test(lfcm_tpsdig~response)
t.test(hcm_tpsdig~response)
t.test(flcm~response)
t.test(logfl~response)
t.test(wg~response)
t.test(logwg~response)
t.test(cond_fact~response)
t.test(csize~response)
#t.test(flow_vel_1.5~response)
#t.test(temp_w_1.5~response)
t.test(approx_1.5~response)
t.test(attemp_1.5~response)
t.test(n_attemp_1.5~response)
t.test(fag_time_1.5~response)
t.test(dmax_1.5~response)
t.test(gspeed_1.5~response)
t.test(sspeed_1.5~response)
#t.test(flow_vel_2.5~response)
#t.test(temp_w_2.5~response)
t.test(approx_2.5~response)
t.test(attemp_2.5~response)
t.test(n_attemp_2.5~response)
t.test(fag_time_2.5~response)
t.test(dmax_2.5~response)
t.test(gspeed_2.5~response)
t.test(sspeed_2.5~response)
#t.test(flow_vel_3~response)
#t.test(temp_w_3~response)
t.test(approx_3~response)
t.test(attemp_3~response)
t.test(n_attemp_3~response)
t.test(fag_time_3~response)
t.test(dmax_3~response)
t.test(gspeed_3~response)
t.test(sspeed_3~response)



t.test(X1X~response)
t.test(X1Y~response)
t.test(X2X~response)
t.test(X2Y~response)
t.test(X3X~response)
t.test(X3Y~response)
t.test(X4X~response)
t.test(X4Y~response)
t.test(X5X~response)
t.test(X5Y~response)
t.test(X6X~response)
t.test(X6Y~response)
t.test(X7X~response)
t.test(X7Y~response)
t.test(UniX~response)
t.test(UniY~response)


t.test(comp_1~response)
t.test(comp_2~response)
t.test(comp_3~response)
t.test(comp_4~response)
t.test(comp_5~response)
t.test(comp_6~response)
t.test(comp_7~response)
t.test(comp_8~response)
t.test(comp_9~response)
t.test(comp_10~response)
t.test(comp_11~response)
t.test(comp_12~response)
t.test(comp_13~response)
t.test(comp_14~response)
t.test(comp_15~response)
t.test(comp_16~response)



wilcox.test(lfcm_tpsdig~response)
wilcox.test(hcm_tpsdig~response)
wilcox.test(flcm~response)
wilcox.test(logfl~response)
wilcox.test(wg~response)
wilcox.test(logwg~response)
wilcox.test(cond_fact~response)
wilcox.test(csize~response)
wilcox.test(flow_vel_1.5~response)
wilcox.test(temp_w_1.5~response)
wilcox.test(approx_1.5~response)
wilcox.test(attemp_1.5~response)
wilcox.test(n_attemp_1.5~response)
wilcox.test(fag_time_1.5~response)
wilcox.test(dmax_1.5~response)
wilcox.test(gspeed_1.5~response)
wilcox.test(sspeed_1.5~response)
wilcox.test(flow_vel_2.5~response)
wilcox.test(temp_w_2.5~response)
wilcox.test(approx_2.5~response)
wilcox.test(attemp_2.5~response)
wilcox.test(n_attemp_2.5~response)
wilcox.test(fag_time_2.5~response)
wilcox.test(dmax_2.5~response)
wilcox.test(gspeed_2.5~response)
wilcox.test(sspeed_2.5~response)
wilcox.test(flow_vel_3~response)
wilcox.test(temp_w_3~response)
wilcox.test(approx_3~response)
wilcox.test(attemp_3~response)
wilcox.test(n_attemp_3~response)
wilcox.test(fag_time_3~response)
wilcox.test(dmax_3~response)
wilcox.test(gspeed_3~response)
wilcox.test(sspeed_3~response)


wilcox.test(X1X~response)
wilcox.test(X1Y~response)
wilcox.test(X2X~response)
wilcox.test(X2Y~response)
wilcox.test(X3X~response)
wilcox.test(X3Y~response)
wilcox.test(X4X~response)
wilcox.test(X4Y~response)
wilcox.test(X5X~response)
wilcox.test(X5Y~response)
wilcox.test(X6X~response)
wilcox.test(X6Y~response)
wilcox.test(X7X~response)
wilcox.test(X7Y~response)
wilcox.test(UniX~response)
wilcox.test(UniY~response)


wilcox.test(comp_1~response)
wilcox.test(comp_2~response)
wilcox.test(comp_3~response)
wilcox.test(comp_4~response)
wilcox.test(comp_5~response)
wilcox.test(comp_6~response)
wilcox.test(comp_7~response)
wilcox.test(comp_8~response)
wilcox.test(comp_9~response)
wilcox.test(comp_10~response)
wilcox.test(comp_11~response)
wilcox.test(comp_12~response)
wilcox.test(comp_13~response)
wilcox.test(comp_14~response)
wilcox.test(comp_15~response)
wilcox.test(comp_16~response)



t.test(lfcm_tpsdig~response15, var.equal =T)
t.test(hcm_tpsdig~response15, var.equal =T)
t.test(flcm~response15, var.equal =T)
t.test(logfl~response15, var.equal =T)
t.test(wg~response15, var.equal =T)
t.test(logwg~response15, var.equal =T)
t.test(cond_fact~response15, var.equal =T)
t.test(csize~response15, var.equal =T)
#t.test(flow_vel_1.5~response15, var.equal =T)
#t.test(temp_w_1.5~response15, var.equal =T)
t.test(approx_1.5~response15, var.equal =T)
t.test(attemp_1.5~response15, var.equal =T)
t.test(n_attemp_1.5~response15, var.equal =T)
t.test(fag_time_1.5~response15, var.equal =T)
t.test(dmax_1.5~response15, var.equal =T)
t.test(gspeed_1.5~response15, var.equal =T)
t.test(sspeed_1.5~response15, var.equal =T)
#t.test(flow_vel_2.5~response15, var.equal =T)
#t.test(temp_w_2.5~response15, var.equal =T)
t.test(approx_2.5~response15, var.equal =T)
t.test(attemp_2.5~response15, var.equal =T)
t.test(n_attemp_2.5~response15, var.equal =T)
t.test(fag_time_2.5~response15, var.equal =T)
t.test(dmax_2.5~response15, var.equal =T)
t.test(gspeed_2.5~response15, var.equal =T)
t.test(sspeed_2.5~response15, var.equal =T)
#t.test(flow_vel_3~response15, var.equal =T)
#t.test(temp_w_3~response15, var.equal =T)
t.test(approx_3~response15, var.equal =T)
t.test(attemp_3~response15, var.equal =T)
t.test(n_attemp_3~response15, var.equal =T)
t.test(fag_time_3~response15, var.equal =T)
t.test(dmax_3~response15, var.equal =T)
t.test(gspeed_3~response15, var.equal =T)
t.test(sspeed_3~response15, var.equal =T)


t.test(X1X~response15, var.equal =T)
t.test(X1Y~response15, var.equal =T)
t.test(X2X~response15, var.equal =T)
t.test(X2Y~response15, var.equal =T)
t.test(X3X~response15, var.equal =T)
t.test(X3Y~response15, var.equal =T)
t.test(X4X~response15, var.equal =T)
t.test(X4Y~response15, var.equal =T)
t.test(X5X~response15, var.equal =T)
t.test(X5Y~response15, var.equal =T)
t.test(X6X~response15, var.equal =T)
t.test(X6Y~response15, var.equal =T)
t.test(X7X~response15, var.equal =T)
t.test(X7Y~response15, var.equal =T)
t.test(UniX~response15, var.equal =T)
t.test(UniY~response15, var.equal =T)


t.test(comp_1~response15, var.equal =T)
t.test(comp_2~response15, var.equal =T)
t.test(comp_3~response15, var.equal =T)
t.test(comp_4~response15, var.equal =T)
t.test(comp_5~response15, var.equal =T)
t.test(comp_6~response15, var.equal =T)
t.test(comp_7~response15, var.equal =T)
t.test(comp_8~response15, var.equal =T)
t.test(comp_9~response15, var.equal =T)
t.test(comp_10~response15, var.equal =T)
t.test(comp_11~response15, var.equal =T)
t.test(comp_12~response15, var.equal =T)
t.test(comp_13~response15, var.equal =T)
t.test(comp_14~response15, var.equal =T)
t.test(comp_15~response15, var.equal =T)
t.test(comp_16~response15, var.equal =T)



  
t.test(lfcm_tpsdig~response15)
t.test(hcm_tpsdig~response15)
t.test(flcm~response15)
t.test(logfl~response15)
t.test(wg~response15)
t.test(logwg~response15)
t.test(cond_fact~response15)
t.test(csize~response15)
#t.test(flow_vel_1.5~response15)
#t.test(temp_w_1.5~response15)
t.test(approx_1.5~response15)
t.test(attemp_1.5~response15)
t.test(n_attemp_1.5~response15)
t.test(fag_time_1.5~response15)
t.test(dmax_1.5~response15)
t.test(gspeed_1.5~response15)
t.test(sspeed_1.5~response15)
#t.test(flow_vel_2.5~response15)
#t.test(temp_w_2.5~response15)
t.test(approx_2.5~response15)
t.test(attemp_2.5~response15)
t.test(n_attemp_2.5~response15)
t.test(fag_time_2.5~response15)
t.test(dmax_2.5~response15)
t.test(gspeed_2.5~response15)
t.test(sspeed_2.5~response15)
#t.test(flow_vel_3~response15)
#t.test(temp_w_3~response15)
t.test(approx_3~response15)
t.test(attemp_3~response15)
t.test(n_attemp_3~response15)
t.test(fag_time_3~response15)
t.test(dmax_3~response15)
t.test(gspeed_3~response15)
t.test(sspeed_3~response15)


t.test(X1X~response15)
t.test(X1Y~response15)
t.test(X2X~response15)
t.test(X2Y~response15)
t.test(X3X~response15)
t.test(X3Y~response15)
t.test(X4X~response15)
t.test(X4Y~response15)
t.test(X5X~response15)
t.test(X5Y~response15)
t.test(X6X~response15)
t.test(X6Y~response15)
t.test(X7X~response15)
t.test(X7Y~response15)
t.test(UniX~response15)
t.test(UniY~response15)


t.test(comp_1~response15)
t.test(comp_2~response15)
t.test(comp_3~response15)
t.test(comp_4~response15)
t.test(comp_5~response15)
t.test(comp_6~response15)
t.test(comp_7~response15)
t.test(comp_8~response15)
t.test(comp_9~response15)
t.test(comp_10~response15)
t.test(comp_11~response15)
t.test(comp_12~response15)
t.test(comp_13~response15)
t.test(comp_14~response15)
t.test(comp_15~response15)
t.test(comp_16~response15)



wilcox.test(lfcm_tpsdig~response15)
wilcox.test(hcm_tpsdig~response15)
wilcox.test(flcm~response15)
wilcox.test(logfl~response15)
wilcox.test(wg~response15)
wilcox.test(logwg~response15)
wilcox.test(cond_fact~response15)
wilcox.test(csize~response15)
wilcox.test(flow_vel_1.5~response15)
wilcox.test(temp_w_1.5~response15)
wilcox.test(approx_1.5~response15)
wilcox.test(attemp_1.5~response15)
wilcox.test(n_attemp_1.5~response15)
wilcox.test(fag_time_1.5~response15)
wilcox.test(dmax_1.5~response15)
wilcox.test(gspeed_1.5~response15)
wilcox.test(sspeed_1.5~response15)
wilcox.test(flow_vel_2.5~response15)
wilcox.test(temp_w_2.5~response15)
wilcox.test(approx_2.5~response15)
wilcox.test(attemp_2.5~response15)
wilcox.test(n_attemp_2.5~response15)
wilcox.test(fag_time_2.5~response15)
wilcox.test(dmax_2.5~response15)
wilcox.test(gspeed_2.5~response15)
wilcox.test(sspeed_2.5~response15)
wilcox.test(flow_vel_3~response15)
wilcox.test(temp_w_3~response15)
wilcox.test(approx_3~response15)
wilcox.test(attemp_3~response15)
wilcox.test(n_attemp_3~response15)
wilcox.test(fag_time_3~response15)
wilcox.test(dmax_3~response15)
wilcox.test(gspeed_3~response15)
wilcox.test(sspeed_3~response15)


wilcox.test(X1X~response15)
wilcox.test(X1Y~response15)
wilcox.test(X2X~response15)
wilcox.test(X2Y~response15)
wilcox.test(X3X~response15)
wilcox.test(X3Y~response15)
wilcox.test(X4X~response15)
wilcox.test(X4Y~response15)
wilcox.test(X5X~response15)
wilcox.test(X5Y~response15)
wilcox.test(X6X~response15)
wilcox.test(X6Y~response15)
wilcox.test(X7X~response15)
wilcox.test(X7Y~response15)
wilcox.test(UniX~response15)
wilcox.test(UniY~response15)


wilcox.test(comp_1~response15)
wilcox.test(comp_2~response15)
wilcox.test(comp_3~response15)
wilcox.test(comp_4~response15)
wilcox.test(comp_5~response15)
wilcox.test(comp_6~response15)
wilcox.test(comp_7~response15)
wilcox.test(comp_8~response15)
wilcox.test(comp_9~response15)
wilcox.test(comp_10~response15)
wilcox.test(comp_11~response15)
wilcox.test(comp_12~response15)
wilcox.test(comp_13~response15)
wilcox.test(comp_14~response15)
wilcox.test(comp_15~response15)
wilcox.test(comp_16~response15)



t.test(lfcm_tpsdig~response25, var.equal =T)
t.test(hcm_tpsdig~response25, var.equal =T)
t.test(flcm~response25, var.equal =T)
t.test(logfl~response25, var.equal =T)
t.test(wg~response25, var.equal =T)
t.test(logwg~response25, var.equal =T)
t.test(cond_fact~response25, var.equal =T)
t.test(csize~response25, var.equal =T)
#t.test(flow_vel_1.5~response25, var.equal =T)
#t.test(temp_w_1.5~response25, var.equal =T)
t.test(approx_1.5~response25, var.equal =T)
t.test(attemp_1.5~response25, var.equal =T)
t.test(n_attemp_1.5~response25, var.equal =T)
t.test(fag_time_1.5~response25, var.equal =T)
t.test(dmax_1.5~response25, var.equal =T)
t.test(gspeed_1.5~response25, var.equal =T)
t.test(sspeed_1.5~response25, var.equal =T)
#t.test(flow_vel_2.5~response25, var.equal =T)
#t.test(temp_w_2.5~response25, var.equal =T)
t.test(approx_2.5~response25, var.equal =T)
t.test(attemp_2.5~response25, var.equal =T)
t.test(n_attemp_2.5~response25, var.equal =T)
t.test(fag_time_2.5~response25, var.equal =T)
t.test(dmax_2.5~response25, var.equal =T)
t.test(gspeed_2.5~response25, var.equal =T)
t.test(sspeed_2.5~response25, var.equal =T)
#t.test(flow_vel_3~response25, var.equal =T)
#t.test(temp_w_3~response25, var.equal =T)
t.test(approx_3~response25, var.equal =T)
t.test(attemp_3~response25, var.equal =T)
t.test(n_attemp_3~response25, var.equal =T)
t.test(fag_time_3~response25, var.equal =T)
t.test(dmax_3~response25, var.equal =T)
t.test(gspeed_3~response25, var.equal =T)
t.test(sspeed_3~response25, var.equal =T)



t.test(X1X~response25, var.equal =T)
t.test(X1Y~response25, var.equal =T)
t.test(X2X~response25, var.equal =T)
t.test(X2Y~response25, var.equal =T)
t.test(X3X~response25, var.equal =T)
t.test(X3Y~response25, var.equal =T)
t.test(X4X~response25, var.equal =T)
t.test(X4Y~response25, var.equal =T)
t.test(X5X~response25, var.equal =T)
t.test(X5Y~response25, var.equal =T)
t.test(X6X~response25, var.equal =T)
t.test(X6Y~response25, var.equal =T)
t.test(X7X~response25, var.equal =T)
t.test(X7Y~response25, var.equal =T)
t.test(UniX~response25, var.equal =T)
t.test(UniY~response25, var.equal =T)


t.test(comp_1~response25, var.equal =T)
t.test(comp_2~response25, var.equal =T)
t.test(comp_3~response25, var.equal =T)
t.test(comp_4~response25, var.equal =T)
t.test(comp_5~response25, var.equal =T)
t.test(comp_6~response25, var.equal =T)
t.test(comp_7~response25, var.equal =T)
t.test(comp_8~response25, var.equal =T)
t.test(comp_9~response25, var.equal =T)
t.test(comp_10~response25, var.equal =T)
t.test(comp_11~response25, var.equal =T)
t.test(comp_12~response25, var.equal =T)
t.test(comp_13~response25, var.equal =T)
t.test(comp_14~response25, var.equal =T)
t.test(comp_15~response25, var.equal =T)
t.test(comp_16~response25, var.equal =T)



t.test(lfcm_tpsdig~response25)
t.test(hcm_tpsdig~response25)
t.test(flcm~response25)
t.test(logfl~response25)
t.test(wg~response25)
t.test(logwg~response25)
t.test(cond_fact~response25)
t.test(csize~response25)
#t.test(flow_vel_1.5~response25)
#t.test(temp_w_1.5~response25)
t.test(approx_1.5~response25)
t.test(attemp_1.5~response25)
t.test(n_attemp_1.5~response25)
t.test(fag_time_1.5~response25)
t.test(dmax_1.5~response25)
t.test(gspeed_1.5~response25)
t.test(sspeed_1.5~response25)
#t.test(flow_vel_2.5~response25)
#t.test(temp_w_2.5~response25)
t.test(approx_2.5~response25)
t.test(attemp_2.5~response25)
t.test(n_attemp_2.5~response25)
t.test(fag_time_2.5~response25)
t.test(dmax_2.5~response25)
t.test(gspeed_2.5~response25)
t.test(sspeed_2.5~response25)
#t.test(flow_vel_3~response25)
#t.test(temp_w_3~response25)
t.test(approx_3~response25)
t.test(attemp_3~response25)
t.test(n_attemp_3~response25)
t.test(fag_time_3~response25)
t.test(dmax_3~response25)
t.test(gspeed_3~response25)
t.test(sspeed_3~response25)


t.test(X1X~response25)
t.test(X1Y~response25)
t.test(X2X~response25)
t.test(X2Y~response25)
t.test(X3X~response25)
t.test(X3Y~response25)
t.test(X4X~response25)
t.test(X4Y~response25)
t.test(X5X~response25)
t.test(X5Y~response25)
t.test(X6X~response25)
t.test(X6Y~response25)
t.test(X7X~response25)
t.test(X7Y~response25)
t.test(UniX~response25)
t.test(UniY~response25)


t.test(comp_1~response25)
t.test(comp_2~response25)
t.test(comp_3~response25)
t.test(comp_4~response25)
t.test(comp_5~response25)
t.test(comp_6~response25)
t.test(comp_7~response25)
t.test(comp_8~response25)
t.test(comp_9~response25)
t.test(comp_10~response25)
t.test(comp_11~response25)
t.test(comp_12~response25)
t.test(comp_13~response25)
t.test(comp_14~response25)
t.test(comp_15~response25)
t.test(comp_16~response25)



wilcox.test(lfcm_tpsdig~response25)
wilcox.test(hcm_tpsdig~response25)
wilcox.test(flcm~response25)
wilcox.test(logfl~response25)
wilcox.test(wg~response25)
wilcox.test(logwg~response25)
wilcox.test(cond_fact~response25)
wilcox.test(csize~response25)
wilcox.test(flow_vel_1.5~response25)
wilcox.test(temp_w_1.5~response25)
wilcox.test(approx_1.5~response25)
wilcox.test(attemp_1.5~response25)
wilcox.test(n_attemp_1.5~response25)
wilcox.test(fag_time_1.5~response25)
wilcox.test(dmax_1.5~response25)
wilcox.test(gspeed_1.5~response25)
wilcox.test(sspeed_1.5~response25)
wilcox.test(flow_vel_2.5~response25)
wilcox.test(temp_w_2.5~response25)
wilcox.test(approx_2.5~response25)
wilcox.test(attemp_2.5~response25)
wilcox.test(n_attemp_2.5~response25)
wilcox.test(fag_time_2.5~response25)
wilcox.test(dmax_2.5~response25)
wilcox.test(gspeed_2.5~response25)
wilcox.test(sspeed_2.5~response25)
wilcox.test(flow_vel_3~response25)
wilcox.test(temp_w_3~response25)
wilcox.test(approx_3~response25)
wilcox.test(attemp_3~response25)
wilcox.test(n_attemp_3~response25)
wilcox.test(fag_time_3~response25)
wilcox.test(dmax_3~response25)
wilcox.test(gspeed_3~response25)
wilcox.test(sspeed_3~response25)


wilcox.test(X1X~response25)
wilcox.test(X1Y~response25)
wilcox.test(X2X~response25)
wilcox.test(X2Y~response25)
wilcox.test(X3X~response25)
wilcox.test(X3Y~response25)
wilcox.test(X4X~response25)
wilcox.test(X4Y~response25)
wilcox.test(X5X~response25)
wilcox.test(X5Y~response25)
wilcox.test(X6X~response25)
wilcox.test(X6Y~response25)
wilcox.test(X7X~response25)
wilcox.test(X7Y~response25)
wilcox.test(UniX~response25)
wilcox.test(UniY~response25)


wilcox.test(comp_1~response25)
wilcox.test(comp_2~response25)
wilcox.test(comp_3~response25)
wilcox.test(comp_4~response25)
wilcox.test(comp_5~response25)
wilcox.test(comp_6~response25)
wilcox.test(comp_7~response25)
wilcox.test(comp_8~response25)
wilcox.test(comp_9~response25)
wilcox.test(comp_10~response25)
wilcox.test(comp_11~response25)
wilcox.test(comp_12~response25)
wilcox.test(comp_13~response25)
wilcox.test(comp_14~response25)
wilcox.test(comp_15~response25)
wilcox.test(comp_16~response25)



  
t.test(lfcm_tpsdig~response30, var.equal =T)
t.test(hcm_tpsdig~response30, var.equal =T)
t.test(flcm~response30, var.equal =T)
t.test(logfl~response30, var.equal =T)
t.test(wg~response30, var.equal =T)
t.test(logwg~response30, var.equal =T)
t.test(cond_fact~response30, var.equal =T)
t.test(csize~response30, var.equal =T)
#t.test(flow_vel_1.5~response30, var.equal =T)
#t.test(temp_w_1.5~response30, var.equal =T)
t.test(approx_1.5~response30, var.equal =T)
t.test(attemp_1.5~response30, var.equal =T)
t.test(n_attemp_1.5~response30, var.equal =T)
t.test(fag_time_1.5~response30, var.equal =T)
t.test(dmax_1.5~response30, var.equal =T)
t.test(gspeed_1.5~response30, var.equal =T)
t.test(sspeed_1.5~response30, var.equal =T)
#t.test(flow_vel_2.5~response30, var.equal =T)
#t.test(temp_w_2.5~response30, var.equal =T)
t.test(approx_2.5~response30, var.equal =T)
t.test(attemp_2.5~response30, var.equal =T)
t.test(n_attemp_2.5~response30, var.equal =T)
t.test(fag_time_2.5~response30, var.equal =T)
t.test(dmax_2.5~response30, var.equal =T)
t.test(gspeed_2.5~response30, var.equal =T)
t.test(sspeed_2.5~response30, var.equal =T)
#t.test(flow_vel_3~response30, var.equal =T)
#t.test(temp_w_3~response30, var.equal =T)
t.test(approx_3~response30, var.equal =T)
t.test(attemp_3~response30, var.equal =T)
t.test(n_attemp_3~response30, var.equal =T)
t.test(fag_time_3~response30, var.equal =T)
t.test(dmax_3~response30, var.equal =T)
t.test(gspeed_3~response30, var.equal =T)
t.test(sspeed_3~response30, var.equal =T)



t.test(X1X~response30, var.equal =T)
t.test(X1Y~response30, var.equal =T)
t.test(X2X~response30, var.equal =T)
t.test(X2Y~response30, var.equal =T)
t.test(X3X~response30, var.equal =T)
t.test(X3Y~response30, var.equal =T)
t.test(X4X~response30, var.equal =T)
t.test(X4Y~response30, var.equal =T)
t.test(X5X~response30, var.equal =T)
t.test(X5Y~response30, var.equal =T)
t.test(X6X~response30, var.equal =T)
t.test(X6Y~response30, var.equal =T)
t.test(X7X~response30, var.equal =T)
t.test(X7Y~response30, var.equal =T)
t.test(UniX~response30, var.equal =T)
t.test(UniY~response30, var.equal =T)


t.test(comp_1~response30, var.equal =T)
t.test(comp_2~response30, var.equal =T)
t.test(comp_3~response30, var.equal =T)
t.test(comp_4~response30, var.equal =T)
t.test(comp_5~response30, var.equal =T)
t.test(comp_6~response30, var.equal =T)
t.test(comp_7~response30, var.equal =T)
t.test(comp_8~response30, var.equal =T)
t.test(comp_9~response30, var.equal =T)
t.test(comp_10~response30, var.equal =T)
t.test(comp_11~response30, var.equal =T)
t.test(comp_12~response30, var.equal =T)
t.test(comp_13~response30, var.equal =T)
t.test(comp_14~response30, var.equal =T)
t.test(comp_15~response30, var.equal =T)
t.test(comp_16~response30, var.equal =T)



t.test(lfcm_tpsdig~response30)
t.test(hcm_tpsdig~response30)
t.test(flcm~response30)
t.test(logfl~response30)
t.test(wg~response30)
t.test(logwg~response30)
t.test(cond_fact~response30)
t.test(csize~response30)
#t.test(flow_vel_1.5~response30)
#t.test(temp_w_1.5~response30)
t.test(approx_1.5~response30)
t.test(attemp_1.5~response30)
t.test(n_attemp_1.5~response30)
t.test(fag_time_1.5~response30)
t.test(dmax_1.5~response30)
t.test(gspeed_1.5~response30)
t.test(sspeed_1.5~response30)
#t.test(flow_vel_2.5~response30)
#t.test(temp_w_2.5~response30)
t.test(approx_2.5~response30)
t.test(attemp_2.5~response30)
t.test(n_attemp_2.5~response30)
t.test(fag_time_2.5~response30)
t.test(dmax_2.5~response30)
t.test(gspeed_2.5~response30)
t.test(sspeed_2.5~response30)
#t.test(flow_vel_3~response30)
#t.test(temp_w_3~response30)
t.test(approx_3~response30)
#t.test(attemp_3~response30)
#t.test(n_attemp_3~response30)
t.test(fag_time_3~response30)
t.test(dmax_3~response30)
t.test(gspeed_3~response30)
t.test(sspeed_3~response30)



t.test(X1X~response30)
t.test(X1Y~response30)
t.test(X2X~response30)
t.test(X2Y~response30)
t.test(X3X~response30)
t.test(X3Y~response30)
t.test(X4X~response30)
t.test(X4Y~response30)
t.test(X5X~response30)
t.test(X5Y~response30)
t.test(X6X~response30)
t.test(X6Y~response30)
t.test(X7X~response30)
t.test(X7Y~response30)
t.test(UniX~response30)
t.test(UniY~response30)


t.test(comp_1~response30)
t.test(comp_2~response30)
t.test(comp_3~response30)
t.test(comp_4~response30)
t.test(comp_5~response30)
t.test(comp_6~response30)
t.test(comp_7~response30)
t.test(comp_8~response30)
t.test(comp_9~response30)
t.test(comp_10~response30)
t.test(comp_11~response30)
t.test(comp_12~response30)
t.test(comp_13~response30)
t.test(comp_14~response30)
t.test(comp_15~response30)
t.test(comp_16~response30)



wilcox.test(lfcm_tpsdig~response30)
wilcox.test(hcm_tpsdig~response30)
wilcox.test(flcm~response30)
wilcox.test(logfl~response30)
wilcox.test(wg~response30)
wilcox.test(logwg~response30)
wilcox.test(cond_fact~response30)
wilcox.test(csize~response30)
wilcox.test(flow_vel_1.5~response30)
wilcox.test(temp_w_1.5~response30)
wilcox.test(approx_1.5~response30)
wilcox.test(attemp_1.5~response30)
wilcox.test(n_attemp_1.5~response30)
wilcox.test(fag_time_1.5~response30)
wilcox.test(dmax_1.5~response30)
wilcox.test(gspeed_1.5~response30)
wilcox.test(sspeed_1.5~response30)
wilcox.test(flow_vel_2.5~response30)
wilcox.test(temp_w_2.5~response30)
wilcox.test(approx_2.5~response30)
wilcox.test(attemp_2.5~response30)
wilcox.test(n_attemp_2.5~response30)
wilcox.test(fag_time_2.5~response30)
wilcox.test(dmax_2.5~response30)
wilcox.test(gspeed_2.5~response30)
wilcox.test(sspeed_2.5~response30)
wilcox.test(flow_vel_3~response30)
wilcox.test(temp_w_3~response30)
wilcox.test(approx_3~response30)
wilcox.test(attemp_3~response30)
wilcox.test(n_attemp_3~response30)
wilcox.test(fag_time_3~response30)
wilcox.test(dmax_3~response30)
wilcox.test(gspeed_3~response30)
wilcox.test(sspeed_3~response30)


wilcox.test(X1X~response30)
wilcox.test(X1Y~response30)
wilcox.test(X2X~response30)
wilcox.test(X2Y~response30)
wilcox.test(X3X~response30)
wilcox.test(X3Y~response30)
wilcox.test(X4X~response30)
wilcox.test(X4Y~response30)
wilcox.test(X5X~response30)
wilcox.test(X5Y~response30)
wilcox.test(X6X~response30)
wilcox.test(X6Y~response30)
wilcox.test(X7X~response30)
wilcox.test(X7Y~response30)
wilcox.test(UniX~response30)
wilcox.test(UniY~response30)


wilcox.test(comp_1~response30)
wilcox.test(comp_2~response30)
wilcox.test(comp_3~response30)
wilcox.test(comp_4~response30)
wilcox.test(comp_5~response30)
wilcox.test(comp_6~response30)
wilcox.test(comp_7~response30)
wilcox.test(comp_8~response30)
wilcox.test(comp_9~response30)
wilcox.test(comp_10~response30)
wilcox.test(comp_11~response30)
wilcox.test(comp_12~response30)
wilcox.test(comp_13~response30)
wilcox.test(comp_14~response30)
wilcox.test(comp_15~response30)
wilcox.test(comp_16~response30)



### DISCRIMINANT ANALYSIS  


####  DA with bootstrap with 1000 repetitions in loop with 75% of data with "reply" at a any flow




library(MASS)




morpho <- read.table("../data/morphoboga_da_wm.txt", header=TRUE, sep="",na.strings="NA", dec=".", strip.white=TRUE, row.names=1)
attach(morpho)
names(morpho)
summary(morpho)
#edit(morpho)



mis.resultados <- data.frame(matrix(data=-99, nrow=1000, ncol=3))
dimnames(mis.resultados)[[2]] <- c("prop.total","prop.a", "prop.b")


#initial probabilities: YES=>25/32=0.78125 NO=>7/32=0.21875

# starting:

for(i in 1:nrow(mis.resultados)){
  dis2 <- lda(response ~ X1X+X1Y+X2X+X2Y+X3X+X3Y+X4X+X4Y+X5X+X5Y+
                X6X+X6Y+X7X+X7Y+UniX+UniY, data=morpho, prior=c(0.78125,0.21875))
  
  Train <- sort(sample(1:32,24))
  m2 <- lda(response~., morpho,subset=Train)
  predict(m2)
  unused <- morpho [-Train,]
  predict(m2,unused)$class
  mi.tabla <- table(predict(m2,unused)$class, unused$response) # horizontal: observaciones reales
  acierto.total <- (mi.tabla[1,1] + mi.tabla[2,2]) / sum(mi.tabla)
  acierto.a <- mi.tabla[1,1] / (mi.tabla[1,1] + mi.tabla[1,2])
  acierto.b <- mi.tabla[2,2] / (mi.tabla[2,1] + mi.tabla[2,2])
  mis.resultados[i,] <- c(acierto.total, acierto.a, acierto.b)
}

summary (mis.resultados)
mi.tabla
acierto.a
acierto.b
acierto.total
Train
plot(m2)


###### prop.total 0.501  
###### prop.a     0.10   
###### prop.b     0.72   


####  DA with bootstrap with 1000 repetitions in loop with 75% of data with "reply" at 1.5 m·s^-1^    



morpho <- read.table("../data/morphoboga_da_wm15.txt", header=TRUE, sep="",na.strings="NA", dec=".", strip.white=TRUE, row.names=1)
attach(morpho)
names(morpho)
summary(morpho)
#edit(morpho)



mis.resultados <- data.frame(matrix(data=-99, nrow=1000, ncol=3))
dimnames(mis.resultados)[[2]] <- c("prop.total","prop.a", "prop.b")


#initial propabilities: YES=>15/32=0.46875 NO=>17/32=0.53125

# starting:

for(i in 1:nrow(mis.resultados)){
  dis2 <- lda(response15 ~ X1X+X1Y+X2X+X2Y+X3X+X3Y+X4X+X4Y+X5X+X5Y+
                X6X+X6Y+X7X+X7Y+UniX+UniY, data=morpho, prior=c(0.46875,0.53125))
  
  Train <- sort(sample(1:32,24))
  m2 <- lda(response15~., morpho,subset=Train)
  predict(m2)
  unused <- morpho [-Train,]
  predict(m2,unused)$class
  mi.tabla <- table(predict(m2,unused)$class, unused$response15) # horizontal: observaciones reales
  acierto.total <- (mi.tabla[1,1] + mi.tabla[2,2]) / sum(mi.tabla)
  acierto.a <- mi.tabla[1,1] / (mi.tabla[1,1] + mi.tabla[1,2])
  acierto.b <- mi.tabla[2,2] / (mi.tabla[2,1] + mi.tabla[2,2])
  mis.resultados[i,] <- c(acierto.total, acierto.a, acierto.b)
}

mi.tabla
acierto.a
acierto.b
acierto.total
Train
plot(m2)

summary(mis.resultados)


###### prop.total 0.5566    
###### prop.a     0.6045    
###### prop.b     0.5311   



#### DA with bootstrap with 1000 repetitions in loop with 75% of data with "reply" at 2.5 m·s^-1^    


Cargamos los datos:  
  **NOTA**: (recordar que hay que hacer un .txt propio solo con las variables a analizar)  


morpho <- read.table("../data/morphoboga_da_wm25.txt", header=TRUE, sep="",na.strings="NA", dec=".", strip.white=TRUE, row.names=1)
attach(morpho)
names(morpho)
summary(morpho)
#edit(morpho)


Cargamos los datos:  
  **NOTA**: (recordar que hay que hacer un .txt propio solo con las variables a analizar)  


mis.resultados <- data.frame(matrix(data=-99, nrow=1000, ncol=3))
dimnames(mis.resultados)[[2]] <- c("prop.total","prop.a", "prop.b")


#initial propabilities: YES=>12/32=0.375 NO=>20/32=0.625

# starting:

for(i in 1:nrow(mis.resultados)){
  dis2 <- lda(response25 ~ X1X+X1Y+X2X+X2Y+X3X+X3Y+X4X+X4Y+X5X+X5Y+
                X6X+X6Y+X7X+X7Y+UniX+UniY, data=morpho, prior=c(0.375,0.625))
  
  Train <- sort(sample(1:32,24))
  m2 <- lda(response25~., morpho,subset=Train)
  predict(m2)
  unused <- morpho [-Train,]
  predict(m2,unused)$class
  mi.tabla <- table(predict(m2,unused)$class, unused$response25) # horizontal: observaciones reales
  acierto.total <- (mi.tabla[1,1] + mi.tabla[2,2]) / sum(mi.tabla)
  acierto.a <- mi.tabla[1,1] / (mi.tabla[1,1] + mi.tabla[1,2])
  acierto.b <- mi.tabla[2,2] / (mi.tabla[2,1] + mi.tabla[2,2])
  mis.resultados[i,] <- c(acierto.total, acierto.a, acierto.b)
}

mi.tabla
acierto.a
acierto.b
acierto.total
Train
plot(m2)


summary(mis.resultados)


#prop.total 0.5259  
#prop.a     0.8000  
#prop.b     0.3968  



#### DA with bootstrap with 1000 repetitions in loop with 75% of data with "reply" at 3.0 m·s^-1^    


Cargamos los datos:  
  **NOTA**: (recordar que hay que hacer un .txt propio solo con las variables a analizar)    


morpho <- read.table("../data/morphoboga_da_wm30.txt", header=TRUE, sep="",na.strings="NA", dec=".", strip.white=TRUE, row.names=1)
attach(morpho)
names(morpho)
summary(morpho)
#edit(morpho)


Cargamos los datos:  
  **NOTA**: (recordar que hay que hacer un .txt propio solo con las variables a analizar)    


mis.resultados <- data.frame(matrix(data=-99, nrow=1000, ncol=3))
dimnames(mis.resultados)[[2]] <- c("prop.total","prop.a", "prop.b")


#initial propabilities:  YES=>7/32=0.21875  NO=>25/32=0.78125

# starting:

for(i in 1:nrow(mis.resultados)){
  dis2 <- lda(response30 ~ X1X+X1Y+X2X+X2Y+X3X+X3Y+X4X+X4Y+X5X+X5Y+
                X6X+X6Y+X7X+X7Y+UniX+UniY, data=morpho, prior=c(0.21875,0.78125))
  
  Train <- sort(sample(1:32,24))
  m2 <- lda(response30~., morpho,subset=Train)
  predict(m2)
  unused <- morpho [-Train,]
  predict(m2,unused)$class
  mi.tabla <- table(predict(m2,unused)$class, unused$response30) # horizontal: observaciones reales
  acierto.total <- (mi.tabla[1,1] + mi.tabla[2,2]) / sum(mi.tabla)
  acierto.a <- mi.tabla[1,1] / (mi.tabla[1,1] + mi.tabla[1,2])
  acierto.b <- mi.tabla[2,2] / (mi.tabla[2,1] + mi.tabla[2,2])
  mis.resultados[i,] <- c(acierto.total, acierto.a, acierto.b)
}

mi.tabla
acierto.a
acierto.b
acierto.total
Train
plot(m2)

summary(mis.resultados)


###### prop.total 0.8333  
###### prop.a     0.5  
###### prop.b     0.75  


####DA with bootstrap with 1000 repetitions in loop with 75% of data with "tries" at any flow  


Cargamos los datos:  
  **NOTA**: (recordar que hay que hacer un .txt propio solo con las variables a analizar)    


morpho <- read.table("../data/morphoboga_da_wm_at.txt", header=TRUE, sep="",na.strings="NA", dec=".", strip.white=TRUE, row.names=1)
attach(morpho)
names(morpho)
summary(morpho)
#edit(morpho)


Cargamos los datos:  
  **NOTA**: (recordar que hay que hacer un .txt propio solo con las variables a analizar)    


mis.resultados <- data.frame(matrix(data=-99, nrow=1000, ncol=3))
dimnames(mis.resultados)[[2]] <- c("prop.total","prop.a", "prop.b")


#initial propabilities: YEs=>7/32=0.21875  NO=>25/32=0.78125 

# starting:

for(i in 1:nrow(mis.resultados)){
  dis2 <- lda(attemp ~ X1X+X1Y+X2X+X2Y+X3X+X3Y+X4X+X4Y+X5X+X5Y+
                X6X+X6Y+X7X+X7Y+UniX+UniY, data=morpho, prior=c(0.21875,0.78125))
  
  Train <- sort(sample(1:32,24))
  m2 <- lda(attemp~., morpho,subset=Train)
  predict(m2)
  unused <- morpho [-Train,]
  predict(m2,unused)$class
  mi.tabla <- table(predict(m2,unused)$class, unused$attemp) # horizontal: observaciones reales
  acierto.total <- (mi.tabla[1,1] + mi.tabla[2,2]) / sum(mi.tabla)
  acierto.a <- mi.tabla[1,1] / (mi.tabla[1,1] + mi.tabla[1,2])
  acierto.b <- mi.tabla[2,2] / (mi.tabla[2,1] + mi.tabla[2,2])
  mis.resultados[i,] <- c(acierto.total, acierto.a, acierto.b)
}

summary (mis.resultados)
mi.tabla
acierto.a
acierto.b
acierto.total
Train
plot(m2)


###### prop.total 0.0.47
###### prop.a     0.087
###### prop.b     0.711



####DA with bootstrap with 1000 repetitions in loop with 75% of data with "tries" a 1.5 m·s^-1^  


Cargamos los datos:  
  **NOTA**: (recordar que hay que hacer un .txt propio solo con las variables a analizar)  


morpho <- read.table("../data/morphoboga_da_wm_at15.txt", header=TRUE, sep="",na.strings="NA", dec=".", strip.white=TRUE, row.names=1)
attach(morpho)
names(morpho)
summary(morpho)
#edit(morpho)


Cargamos los datos:  
  **NOTA**: (recordar que hay que hacer un .txt propio solo con las variables a analizar)  


mis.resultados <- data.frame(matrix(data=-99, nrow=1000, ncol=3))
dimnames(mis.resultados)[[2]] <- c("prop.total","prop.a", "prop.b")


#initial propabilities: YES=>15/32=0.46875 NO=>17/32=0.53125

# starting:

for(i in 1:nrow(mis.resultados)){
  dis2 <- lda(attemp15 ~ X1X+X1Y+X2X+X2Y+X3X+X3Y+X4X+X4Y+X5X+X5Y+
                X6X+X6Y+X7X+X7Y+UniX+UniY, data=morpho, prior=c(0.46875,0.53125))
  
  Train <- sort(sample(1:32,24))
  m2 <- lda(attemp15~., morpho,subset=Train)
  predict(m2)
  unused <- morpho [-Train,]
  predict(m2,unused)$class
  mi.tabla <- table(predict(m2,unused)$class, unused$attemp15) # horizontal: observaciones reales
  acierto.total <- (mi.tabla[1,1] + mi.tabla[2,2]) / sum(mi.tabla)
  acierto.a <- mi.tabla[1,1] / (mi.tabla[1,1] + mi.tabla[1,2])
  acierto.b <- mi.tabla[2,2] / (mi.tabla[2,1] + mi.tabla[2,2])
  mis.resultados[i,] <- c(acierto.total, acierto.a, acierto.b)
}

summary (mis.resultados)
mi.tabla
acierto.a
acierto.b
acierto.total
Train
plot(m2)


###### prop.total 0.5686  
###### prop.a     0.6037  
###### prop.b     0.5484  



####DA with bootstrap with 1000 repetitions in loop with 75% of data with "tries" a 2.5 m·s^-1^    


Cargamos los datos:  
  **NOTA**: (recordar que hay que hacer un .txt propio solo con las variables a analizar)    


morpho <- read.table("../data/morphoboga_da_wm_at25.txt", header=TRUE, sep="",na.strings="NA", dec=".", strip.white=TRUE, row.names=1)
attach(morpho)
names(morpho)
summary(morpho)
#edit(morpho)


Cargamos los datos:  
  **NOTA**: (recordar que hay que hacer un .txt propio solo con las variables a analizar)    


mis.resultados <- data.frame(matrix(data=-99, nrow=1000, ncol=3))
dimnames(mis.resultados)[[2]] <- c("prop.total","prop.a", "prop.b")


#initial propabilities: YES=>20/32=0.625 NO=>12/32=0.375

# starting:

for(i in 1:nrow(mis.resultados)){dis2 <-lda(attemp25~X1X+X1Y+X2X+X2Y+X3X+X3Y+X4X+X4Y+X5X+X5Y+X6X+X6Y+X7X+X7Y+UniX+UniY,data=morpho, prior=c(0.625,0.375))

Train <- sort(sample(1:32,24))
m2 <- lda(attemp25~., morpho,subset=Train)
predict(m2)
unused <- morpho [-Train,]
predict(m2,unused)$class
mi.tabla <- table(predict(m2,unused)$class, unused$attemp25) #horizontal: observaciones reales
acierto.total <- (mi.tabla[1,1] + mi.tabla[2,2]) / sum(mi.tabla)
acierto.a <- mi.tabla[1,1] / (mi.tabla[1,1] + mi.tabla[1,2])
acierto.b <- mi.tabla[2,2] / (mi.tabla[2,1] + mi.tabla[2,2])
mis.resultados[i,] <- c(acierto.total, acierto.a, acierto.b)
}

summary (mis.resultados)
mi.tabla
acierto.a
acierto.b
acierto.total
Train
plot(m2)


###### prop.total 0.5214  
###### prop.a     0.6305  
###### prop.b     0.3937  


#### DA with bootstrap with 1000 repetitions in loop with 75% of data with "tries" a 3.0 m·s^-1^  


Cargamos los datos:  
  **NOTA**: (recordar que hay que hacer un .txt propio solo con las variables a analizar)    


morpho <- read.table("../data/morphoboga_da_wm_at30.txt", header=TRUE, sep="",na.strings="NA", dec=".", strip.white=TRUE, row.names=1)
attach(morpho)
names(morpho)
summary(morpho)
#edit(morpho)


Cargamos los datos:  
  **NOTA**: (recordar que hay que hacer un .txt propio solo con las variables a analizar)    


mis.resultados <- data.frame(matrix(data=-99, nrow=1000, ncol=3))
dimnames(mis.resultados)[[2]] <- c("prop.total","prop.a", "prop.b")


#initial propabilities: yes=>7/32=0.21875 no=>25/32=0.78125 

# starting:

for(i in 1:nrow(mis.resultados)){
  dis2 <- lda(attemp30 ~ X1X+X1Y+X2X+X2Y+X3X+X3Y+X4X+X4Y+X5X+X5Y+
                X6X+X6Y+X7X+X7Y+UniX+UniY, data=morpho, prior=c(0.21875,0.78125))
  
  Train <- sort(sample(1:32,24))
  m2 <- lda(attemp30~., morpho,subset=Train)
  predict(m2)
  unused <- morpho [-Train,]
  predict(m2,unused)$class
  mi.tabla <- table(predict(m2,unused)$class, unused$attemp30) # horizontal: observaciones reales
  acierto.total <- (mi.tabla[1,1] + mi.tabla[2,2]) / sum(mi.tabla)
  acierto.a <- mi.tabla[1,1] / (mi.tabla[1,1] + mi.tabla[1,2])
  acierto.b <- mi.tabla[2,2] / (mi.tabla[2,1] + mi.tabla[2,2])
  mis.resultados[i,] <- c(acierto.total, acierto.a, acierto.b)
}

summary (mis.resultados)
mi.tabla
acierto.a
acierto.b
acierto.total
Train
plot(m2)


###### prop.total 0.6619
###### prop.a     0.8161
###### prop.b     0.3046



### MANOVA  


Se cargan las librerias necesarias:    
  
  
library(MASS)
library(evd)


#### Any reply with relative warps    


morpho <- read.table("../data/morphoboga_da_wm.txt", header=TRUE, sep="",na.strings="NA", dec=".", strip.white=TRUE, row.names=1)
attach(morpho)
names(morpho)
summary(morpho)




manova3<-manova(cbind(X1X,X1Y,X2X,X2Y,X3X,X3Y,X4X,X4Y,X5X,X5Y,X6X,X6Y,X7X,X7Y,UniX,UniY)~response)
manova3
summary(manova3)
summary(manova3, test="Pillai")
summary(manova3, test="Wilks")
summary(manova3, test="Hotelling")
summary(manova3, test="Roy")
qf(.95,1,16)#REVISAR
summary.aov(manova3)
summary.lm(manova3)



par(mfcol=c(2,2))
chiplot(residuals(manova3))
plot (residuals(manova3))



#### Any reply with Principal Components



morpho <- read.table("../data/morphoboga_da_pca.txt", header=TRUE, sep="",na.strings="NA", dec=".", strip.white=TRUE, row.names=1)
attach(morpho)
names(morpho)
summary(morpho)




manova3<-manova(cbind(comp_1,comp_2,comp_3,comp_4,comp_5,comp_6,comp_7,comp_8,comp_9,comp_10,comp_11,comp_12,comp_13,comp_14,comp_15,comp_16)~response)

manova3
summary(manova3)
summary(manova3, test="Pillai")
summary(manova3, test="Wilks")
summary(manova3, test="Hotelling")
summary(manova3, test="Roy")
qf(.95,1,16)#REVISAR
summary.aov(manova3)
summary.lm(manova3)


Representación gráfica  


par(mfcol=c(2,2))
chiplot(residuals(manova3))
plot (residuals(manova3))


#### Reply at 1.5 m·s^-1^ with relative and partial warps   


morpho <- read.table("../data/morphoboga_da_wm15.txt", header=TRUE, sep="",na.strings="NA", dec=".", strip.white=TRUE, row.names=1)
attach(morpho)
names(morpho)
summary(morpho)




manova3<-manova(cbind(X1X,X1Y,X2X,X2Y,X3X,X3Y,X4X,X4Y,X5X,X5Y,X6X,X6Y,X7X,X7Y,UniX,UniY)~response15)
manova3
summary(manova3)
summary(manova3, test="Pillai")
summary(manova3, test="Wilks")
summary(manova3, test="Hotelling")
summary(manova3, test="Roy")
qf(.95,1,16)#REVISAR
summary.aov(manova3)
summary.lm(manova3)


 gráfica  


par(mfcol=c(2,2))
chiplot(residuals(manova3))
plot (residuals(manova3))


#### Reply 1.5 m·s^-1^ with principal components


morpho <- read.table("../data/morphoboga_da_pca15.txt", header=TRUE, sep="",na.strings="NA", dec=".", strip.white=TRUE, row.names=1)
attach(morpho)
names(morpho)
summary(morpho)




manova3<-manova(cbind(comp_1,comp_2,comp_3,comp_4,comp_5,comp_6,comp_7,comp_8,comp_9,comp_10,comp_11,comp_12,comp_13,comp_14,comp_15,comp_16)~response15)
manova3
summary(manova3)
summary(manova3, test="Pillai")
summary(manova3, test="Wilks")
summary(manova3, test="Hotelling")
summary(manova3, test="Roy")
qf(.95,1,16)#REVISAR
summary.aov(manova3)
summary.lm(manova3)




par(mfcol=c(2,2))
chiplot(residuals(manova3))
plot (residuals(manova3))


#### Tries 1.5 m·s^-1^ with relative and partial warps     


morpho <- read.table("../data/morphoboga_da_wm_at15.txt", header=TRUE, sep="",na.strings="NA", dec=".", strip.white=TRUE, row.names=1)
attach(morpho)
names(morpho)
summary(morpho)




manova3<-manova(cbind(X1X,X1Y,X2X,X2Y,X3X,X3Y,X4X,X4Y,X5X,X5Y,X6X,X6Y,X7X,X7Y,UniX,UniY)~attemp15)
manova3
summary(manova3)
summary(manova3, test="Pillai")
summary(manova3, test="Wilks")
summary(manova3, test="Hotelling")
summary(manova3, test="Roy")
qf(.95,1,16)#REVISAR
summary.aov(manova3)
summary.lm(manova3)




par(mfcol=c(2,2))
chiplot(residuals(manova3))
plot (residuals(manova3))


#### Reply 2.5 m·s^-1^ with relative and partial warps     


morpho <- read.table("../data/morphoboga_da_wm25.txt", header=TRUE, sep="",na.strings="NA", dec=".", strip.white=TRUE, row.names=1)
attach(morpho)
names(morpho)
summary(morpho)



manova3<-manova(cbind(X1X,X1Y,X2X,X2Y,X3X,X3Y,X4X,X4Y,X5X,X5Y,X6X,X6Y,X7X,X7Y,UniX,UniY)~response25)
manova3
summary(manova3)
summary(manova3, test="Pillai")
summary(manova3, test="Wilks")
summary(manova3, test="Hotelling")
summary(manova3, test="Roy")
qf(.95,1,16)#REVISAR
summary.aov(manova3)
summary.lm(manova3)




par(mfcol=c(2,2))
chiplot(residuals(manova3))
plot (residuals(manova3))


#### Reply 2.5 m·s^-1^ with principal components 


morpho <- read.table("../data/morphoboga_da_pca25.txt", header=TRUE, sep="",na.strings="NA", dec=".", strip.white=TRUE, row.names=1)
attach(morpho)
names(morpho)
summary(morpho)




manova3<-manova(cbind(comp_1,comp_2,comp_3,comp_4,comp_5,comp_6,comp_7,comp_8,comp_9,comp_10,comp_11,comp_12,comp_13,comp_14,comp_15,comp_16)~response25)
manova3
summary(manova3)
summary(manova3, test="Pillai")
summary(manova3, test="Wilks")
summary(manova3, test="Hotelling")
summary(manova3, test="Roy")
qf(.95,12,1275)#REVISAR
summary.aov(manova3)
summary.lm(manova3)



par(mfcol=c(2,2))
chiplot(residuals(manova3))
plot (residuals(manova3))


#### Reply 2.5 m·s^-1^ with relative and partial warps


morpho <- read.table("../data/morphoboga_da_wm_at25.txt", header=TRUE, sep="",na.strings="NA", dec=".", strip.white=TRUE, row.names=1)
attach(morpho)
names(morpho)
summary(morpho)




manova3<-manova(cbind(X1X,X1Y,X2X,X2Y,X3X,X3Y,X4X,X4Y,X5X,X5Y,X6X,X6Y,X7X,X7Y,UniX,UniY)~attemp25)
manova3
summary(manova3)
summary(manova3, test="Pillai")
summary(manova3, test="Wilks")
summary(manova3, test="Hotelling")
summary(manova3, test="Roy")
qf(.95,1,16)#REVISAR
summary.aov(manova3)
summary.lm(manova3)




par(mfcol=c(2,2))
chiplot(residuals(manova3))
plot (residuals(manova3))


#### REply 3.0 m·s^-1^ with relative and partial warps    


morpho <- read.table("../data/morphoboga_da_wm30.txt", header=TRUE, sep="",na.strings="NA", dec=".", strip.white=TRUE, row.names=1)
attach(morpho)
names(morpho)
summary(morpho)




manova3<-manova(cbind(X1X,X1Y,X2X,X2Y,X3X,X3Y,X4X,X4Y,X5X,X5Y,X6X,X6Y,X7X,X7Y,UniX,UniY)~response30)
manova3
summary(manova3)
summary(manova3, test="Pillai")
summary(manova3, test="Wilks")
summary(manova3, test="Hotelling")
summary(manova3, test="Roy")
qf(.95,1,16)#REVISAR
summary.aov(manova3)
summary.lm(manova3)




par(mfcol=c(2,2))
chiplot(residuals(manova3))
plot (residuals(manova3))


#### REply 3.0 m·s^-1^ with principal components    


morpho <- read.table("../data/morphoboga_da_pca30.txt", header=TRUE, sep="",na.strings="NA", dec=".", strip.white=TRUE, row.names=1)
attach(morpho)
names(morpho)
summary(morpho)




manova3<-manova(cbind(comp_1,comp_2,comp_3,comp_4,comp_5,comp_6,comp_7,comp_8,comp_9,comp_10,comp_11,comp_12,comp_13,comp_14,comp_15,comp_16)~response30)
manova3
summary(manova3)
summary(manova3, test="Pillai")
summary(manova3, test="Wilks")
summary(manova3, test="Hotelling")
summary(manova3, test="Roy")
qf(.95,1,16)#REVISAR
summary.aov(manova3)
summary.lm(manova3)



par(mfcol=c(2,2))
chiplot(residuals(manova3))
plot (residuals(manova3))


#### Reply 3.0 m·s^-1^ with relative and partial warps   



morpho <- read.table("data/morphoboga_da_wm_at30.txt", header=TRUE, sep="",na.strings="NA", dec=".", strip.white=TRUE, row.names=1)
attach(morpho)
names(morpho)
summary(morpho)




manova3<-manova(cbind(X1X,X1Y,X2X,X2Y,X3X,X3Y,X4X,X4Y,X5X,X5Y,X6X,X6Y,X7X,X7Y,UniX,UniY)~attemp30)
manova3
summary(manova3)
summary(manova3, test="Pillai")
summary(manova3, test="Wilks")
summary(manova3, test="Hotelling")
summary(manova3, test="Roy")
qf(.95,1,16)#REVISAR
qf(.95,1,1.7211)#REVISAR
summary.aov(manova3)
summary.lm(manova3)



par(mfcol=c(2,2))
chiplot(residuals(manova3))
plot (residuals(manova3))



## CORRELATION MATRIX   


  
#install.packages("psych")
library(psych)
#citation("psych")
library (car)



  
data01 <- read.table("../data/data_boga2.txt",header=TRUE,sep="",na.strings="NA",dec=".",strip.white=TRUE, row.names=1)
attach(data01)
names(data01)
summary(data01)




cor(data01[,c("lfcm_tpsdig","hcm_tpsdig","flcm","logfl","wg","logwg","cond_fact","csize","comp_1","comp_2","comp_3","comp_4","comp_5","approx_3","attemp_3","n_attemp_3","fag_time_3","dmax_3","gspeed_3","sspeed_3")],use="complete.obs")



cor(data01[,c("lfcm_tpsdig","hcm_tpsdig","flcm","logfl","wg","logwg","cond_fact","csize","comp_1","comp_2","comp_3","comp_4","comp_5","approx_1.5","attemp_1.5","n_attemp_1.5","fag_time_1.5","dmax_1.5","gspeed_1.5","sspeed_1.5")],use="complete.obs")



cor(data01[,c("lfcm_tpsdig","hcm_tpsdig","flcm","logfl","wg","logwg","cond_fact","csize","comp_1","comp_2","comp_3","comp_4","comp_5","approx_2.5","attemp_2.5","n_attemp_2.5","fag_time_2.5","dmax_2.5","gspeed_2.5","sspeed_2.5")],use="complete.obs")



disper01<-data01[,c("lfcm_tpsdig","hcm_tpsdig","flcm","logfl","wg","logwg","cond_fact","csize","comp_1","comp_2","comp_3","comp_4","comp_5","approx_1.5","attemp_1.5","n_attemp_1.5","fag_time_1.5","dmax_1.5","gspeed_1.5","sspeed_1.5")]



disper02<-data01[,c("lfcm_tpsdig","hcm_tpsdig","flcm","logfl","wg","logwg","cond_fact","csize","comp_1","comp_2","comp_3","comp_4","comp_5","approx_2.5","attemp_2.5","n_attemp_2.5","fag_time_2.5","dmax_2.5","gspeed_2.5","sspeed_2.5")]



disper03<-data01[,c("lfcm_tpsdig","hcm_tpsdig","flcm","logfl","wg","logwg","cond_fact","csize","comp_1","comp_2","comp_3","comp_4","comp_5","approx_3","attemp_3","n_attemp_3","fag_time_3","dmax_3","gspeed_3","sspeed_3")]



disper04<-data01[,c("lfcm_tpsdig","hcm_tpsdig","flcm","logfl","wg","logwg","cond_fact","csize","comp_1","comp_2","comp_3","comp_4","comp_5","approx_1.5","attemp_1.5","n_attemp_1.5","fag_time_1.5","rdmax_1.5","rgspeed_1.5","rsspeed_1.5")]



disper05<-data01[,c("lfcm_tpsdig","hcm_tpsdig","flcm","logfl","wg","logwg","cond_fact","csize","comp_1","comp_2","comp_3","comp_4","comp_5", "approx_2.5","attemp_2.5","n_attemp_2.5","fag_time_2.5","rdmax_2.5","rgspeed_2.5","rsspeed_2.5")]



disper06<-data01[,c("lfcm_tpsdig","hcm_tpsdig","flcm","logfl","wg","logwg","cond_fact","csize","comp_1","comp_2","comp_3","comp_4","comp_5","approx_3","attemp_3","n_attemp_3","fag_time_3","rdmax_3","rgspeed_3","rsspeed_3")]




cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}




cor(disper01)
cor.prob(disper01)
flattenSquareMatrix(cor.prob(disper01))



cor(disper02)
cor.prob(disper02)
flattenSquareMatrix(cor.prob(disper02))



cor(disper03)
cor.prob(disper03)
flattenSquareMatrix(cor.prob(disper03))



cor(disper04)
cor.prob(disper04)
flattenSquareMatrix(cor.prob(disper04))



cor(disper05)
cor.prob(disper05)
flattenSquareMatrix(cor.prob(disper05))



cor(disper06)
cor.prob(disper06)
flattenSquareMatrix(cor.prob(disper06))





#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
#citation("PerformanceAnalytics")


Para elaborar la representación gráfica de las matrices de correlación  


chart.Correlation(disper01)
chart.Correlation(disper02)
chart.Correlation(disper03)
chart.Correlation(disper04)
chart.Correlation(disper05)
chart.Correlation(disper06)




data01 <- read.table("../data/data_boga2NA.txt", header=TRUE, sep="",na.strings="NA", dec=".",strip.white=TRUE, row.names=1)
attach(data01)
names(data01)
summary(data01)



{r  message=FALSE, warning=FALSE}
#install.packages("psych")
library(psych)
#citation("psych")
library (car)




cor(data01[,c("lfcm_tpsdig","hcm_tpsdig","flcm","logfl","wg","logwg","cond_fact","csize","comp_1","comp_2","comp_3","comp_4","comp_5","approx_3","attemp_3","n_attemp_3","fag_time_3","dmax_3","gspeed_3","sspeed_3")],use="complete.obs")



cor(data01[,c("lfcm_tpsdig","hcm_tpsdig","flcm","logfl","wg","logwg","cond_fact","csize","comp_1","comp_2","comp_3","comp_4","comp_5","approx_1.5","attemp_1.5","n_attemp_1.5","fag_time_1.5","dmax_1.5","gspeed_1.5","sspeed_1.5")],use="complete.obs")



cor(data01[,c("lfcm_tpsdig","hcm_tpsdig","flcm","logfl","wg","logwg","cond_fact","csize","comp_1","comp_2","comp_3","comp_4","comp_5","approx_2.5","attemp_2.5","n_attemp_2.5","fag_time_2.5","dmax_2.5","gspeed_2.5","sspeed_2.5")],use="complete.obs")




disper01<-data01[,c("lfcm_tpsdig","hcm_tpsdig","flcm","logfl","wg","logwg","cond_fact","csize","comp_1","comp_2","comp_3","comp_4","comp_5","approx_1.5","attemp_1.5","n_attemp_1.5","fag_time_1.5","dmax_1.5","gspeed_1.5","sspeed_1.5")]



disper02<-data01[,c("lfcm_tpsdig","hcm_tpsdig","flcm","logfl","wg","logwg","cond_fact","csize","comp_1","comp_2","comp_3","comp_4","comp_5","approx_2.5","attemp_2.5","n_attemp_2.5","fag_time_2.5","dmax_2.5","gspeed_2.5","sspeed_2.5")]



disper03<-data01[,c("lfcm_tpsdig","hcm_tpsdig","flcm","logfl","wg","logwg","cond_fact","csize","comp_1","comp_2","comp_3","comp_4","comp_5","approx_3","attemp_3","n_attemp_3","fag_time_3","dmax_3","gspeed_3","sspeed_3")]



disper04<-data01[,c("lfcm_tpsdig","hcm_tpsdig","flcm","logfl","wg","logwg","cond_fact","csize","comp_1","comp_2","comp_3","comp_4","comp_5","approx_1.5","attemp_1.5","n_attemp_1.5","fag_time_1.5","rdmax_1.5","rsspeed_1.5")]



disper05<-data01[,c("lfcm_tpsdig","hcm_tpsdig","flcm","logfl","wg","logwg","cond_fact","csize","comp_1","comp_2","comp_3","comp_4","comp_5","approx_2.5","attemp_2.5","n_attemp_2.5","fag_time_2.5","rdmax_2.5","rgspeed_2.5","rsspeed_2.5")]



disper06<-data01[,c("lfcm_tpsdig","hcm_tpsdig","flcm","logfl","wg","logwg","cond_fact","csize","comp_1","comp_2","comp_3","comp_4","comp_5","approx_3","attemp_3","n_attemp_3","fag_time_3","rdmax_3","rgspeed_3","rsspeed_3")]




cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}



cor(disper01)
cor.prob(disper01)
flattenSquareMatrix(cor.prob(disper01))



cor(disper02)
cor.prob(disper02)
flattenSquareMatrix(cor.prob(disper02))



cor(disper03)
cor.prob(disper03)
flattenSquareMatrix(cor.prob(disper03))



cor(disper04)
cor.prob(disper04)
flattenSquareMatrix(cor.prob(disper04))



cor(disper05)
cor.prob(disper05)
flattenSquareMatrix(cor.prob(disper05))



cor(disper06)
cor.prob(disper06)
flattenSquareMatrix(cor.prob(disper06))




#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
#citation("PerformanceAnalytics")



chart.Correlation(disper01)
chart.Correlation(disper02)
chart.Correlation(disper03)
chart.Correlation(disper04)
chart.Correlation(disper05)
chart.Correlation(disper06)



install.packages('ggplot2')
library("ggplot2")
install.packages('ggpubr')
library("ggpubr")



data01 <- read.table("../data/data_boga2NA.txt", header=TRUE, sep="",na.strings="NA", dec=".",strip.white=TRUE, row.names=1)
attach(data01)
names(data01)
summary(data01)




cor.test(comp_3,fag_time_3, method='spearman')
model4<-lm(fag_time_3~comp_3)
model4
anova (model4)
summary(model4)

plot(model4)




cor.test(comp_3,dmax_3, method='spearman')
model5<-lm(dmax_3~comp_3)
model5
anova (model5)
summary(model5)

plot(model5)




cor.test(comp_3,gspeed_3, method='spearman')
model6<-lm(gspeed_3~comp_3)
model6
anova (model6)
summary(model6)

plot(model6)



cor.test(comp_3,sspeed_3, method='spearman')
model7<-lm(sspeed_3~comp_3)
model7
anova (model7)
summary(model7)

plot(model7)


cor.test(comp_1,dmax_1.5, method='spearman')
model1<-lm(dmax_1.5~comp_2)
model1
anova (model1)
summary(model1)

plot(model1)





cor.test(comp_2,dmax_1.5, method='spearman')
model1<-lm(dmax_1.5~comp_2)
model1
anova (model1)
summary(model1)

plot(model1)





cor.test(comp_1,dmax_2.5, method='spearman')
model2<-lm(dmax_2.5~comp_1)
model2
anova (model2)
summary(model2)

plot(model2)



cor.test(comp_2,dmax_2.5, method='spearman')
model2<-lm(dmax_2.5~comp_2)
model2
anova (model2)
summary(model2)

plot(model2)



cor.test(comp_1,sspeed_1.5, method='spearman')
model7<-lm(sspeed_1.5~comp_1)
model7
anova (model7)
summary(model7)

plot(model7)



cor.test(comp_2,gspeed_1.5, method='spearman')
model3<-lm(gspeed_2.5~comp_1)
model3
anova (model3)
summary(model3)

plot(model3)





cor.test(comp_1,sspeed_2.5, method='spearman')
model7<-lm(sspeed_2.5~comp_1)
model7
anova (model7)
summary(model7)

plot(model7)



cor.test(comp_2,sspeed_2.5, method='spearman')
model7<-lm(sspeed_2.5~comp_2)
model7
anova (model7)
summary(model7)

plot(model7)



cor.test(comp_1,fag_time_1.5, method='spearman')
model7<-lm(fag_time_1.5~comp_1)
model7
anova (model7)
summary(model7)

plot(model7)



cor.test(comp_2,fag_time_1.5, method='spearman')
model3<-lm(fag_time_1.5~comp_1)
model3
anova (model3)
summary(model3)

plot(model3)





cor.test(comp_1,fag_time_2.5, method='spearman')
model7<-lm(fag_time_2.5~comp_1)
model7
anova (model7)
summary(model7)

plot(model7)



cor.test(comp_2,fag_time_2.5, method='spearman')
model7<-lm(fag_time_2.5~comp_2)
model7
anova (model7)
summary(model7)

plot(model7)




datacor <- read.table("../data/pc3vsus.txt",header=TRUE,sep="",na.strings="NA", dec=".",strip.white=TRUE, row.names=1)
attach(datacor)
names(datacor)
summary(datacor)




cor.test(comp_3,sspeed_3, method='spearman')
model7<-lm(sspeed_3~comp_3)
model7
anova (model7)
summary(model7)

plot(model7)

cor
cor(sspeed_3,comp_3)
??lm
model2<-lm(sspeed_3~comp_3)
model2
summary(model2)

summary.lm(model2)

confint (model2)

install.packages('ggpubr')
library("ggpubr")

ggscatter(datacor, x = "comp_3", y = "sspeed_3", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "PC3", ylab = "Swimming Speed m·s-1")



## MODELO

 

#install.packages("MASS")
#install.packages("readxl")
install.packages("ggplot2")
#install.packages("car")
library (MASS)
library (readxl)
library (ggplot2)
library (car)



data01 <- read.table("../data/data_boga2NA.txt",header=TRUE,sep="",na.strings="NA", dec=".",strip.white=TRUE, row.names=1)
attach(data01)
names(data01)
summary(data01)
data01




RespuestaTmodel<-glm(as.factor(response)~comp_1+comp_2+comp_3+csize+cond_fact+lfcm_tpsdig+wg, family=binomial)
RespuestaTmodel
summary(RespuestaTmodel)




RespuestaTmodelr<-stepAIC(RespuestaTmodel)
summary(RespuestaTmodelr)
anova(RespuestaTmodelr)
RespuestaTmodelr
summary.glm(RespuestaTmodelr)

hist(resid(RespuestaTmodelr)) #Vemos q  existe distr norm de los residuos
qqnorm(resid(RespuestaTmodelr)) #Vemos q existe distr norm por linealidad
qqline(resid(RespuestaTmodelr)) #lo que deberiamos obtener siguiendo normalidad
qqPlot(resid(RespuestaTmodelr)) #esto es del paquete "car"
plot(fitted(RespuestaTmodelr), resid(RespuestaTmodelr)) #primero fitted i dsps los residuos #vemos que NO hay homoscedasticidad
plot(RespuestaTmodelr)



Respuesta15model<-glm(as.factor(response15)~comp_1+comp_2+comp_3+csize+cond_fact+lfcm_tpsdig+wg, family=binomial)
Respuesta15model
summary(Respuesta15model)
anova(Respuesta15model)




Respuesta15modelr<-stepAIC(Respuesta15model)
summary(Respuesta15modelr)
anova(Respuesta15modelr)
Respuesta15modelr
summary.glm(Respuesta15modelr)

hist(resid(Respuesta15modelr)) #Vemos q  existe distr norm de los residuos
qqnorm(resid(Respuesta15modelr)) #Vemos q existe distr norm por linealidad
qqline(resid(Respuesta15modelr)) #lo que deberiamos obtener siguiendo normalidad
qqPlot(resid(Respuesta15modelr)) #esto es del paquete "car"
plot(fitted(Respuesta15modelr), resid(Respuesta15modelr)) #primero fitted i dsps los residuos #vemos que NO hay homoscedasticidad
plot(Respuesta15modelr)




Respuesta25model<-glm(as.factor(response25)~comp_1+comp_2+comp_3+csize+cond_fact+lfcm_tpsdig+wg, family=binomial)
RespuestaTmodel
summary.lm(Respuesta25model)



Respuesta25modelr<-stepAIC(Respuesta25model)
summary(Respuesta25modelr)
anova(Respuesta25modelr)
Respuesta25modelr
summary.glm(Respuesta25modelr)
summary.lm(Respuesta25modelr)


hist(resid(Respuesta25modelr)) #Vemos q  existe distr norm de los residuos
qqnorm(resid(Respuesta25modelr)) #Vemos q existe distr norm por linealidad
qqline(resid(Respuesta25modelr)) #lo que deberiamos obtener siguiendo normalidad
qqPlot(resid(Respuesta25modelr)) #esto es del paquete "car"
plot(fitted(Respuesta25modelr), resid(Respuesta25modelr)) #primero fitted i dsps los residuos #vemos que NO hay homoscedasticidad
plot(Respuesta25modelr)




Respuesta30model<-glm(as.factor(response30)~comp_1+comp_2+comp_3+csize+cond_fact+lfcm_tpsdig+wg, family=binomial)
Respuesta30model
summary(Respuesta30model)


Respuesta30modelr<-stepAIC(Respuesta30model)
summary(Respuesta30modelr)
anova(Respuesta30modelr)
Respuesta30modelr
summary.glm(Respuesta30modelr)
summary.lm(Respuesta30modelr)


hist(resid(Respuesta30modelr)) #Vemos q  existe distr norm de los residuos
qqnorm(resid(Respuesta30modelr)) #Vemos q existe distr norm por linealidad
qqline(resid(Respuesta30modelr)) #lo que deberiamos obtener siguiendo normalidad
qqPlot(resid(Respuesta30modelr)) #esto es del paquete "car"
plot(fitted(Respuesta30modelr), resid(Respuesta30modelr)) #primero fitted i dsps los residuos #vemos que NO hay homoscedasticidad
plot(Respuesta30modelr)





  
model01<-glm(fag_time_1.5~comp_1+comp_2+comp_3+csize+cond_fact+lfcm_tpsdig+wg)
model01
summary(model01)
anova(model01)
summary.lm(model01)



model01r<-stepAIC(model01)
summary(model01r)
anova(model01r)
model01r
summary.glm(model01r)
summary.lm(model01r)

hist(resid(model01r)) #Vemos q  existe distr norm de los residuos
qqnorm(resid(model01r)) #Vemos q existe distr norm por linealidad
qqline(resid(model01r)) #lo que deberiamos obtener siguiendo normalidad
qqPlot(resid(model01r)) #esto es del paquete "car"
plot(fitted(model01r), resid(model01r)) #primero fitted i dsps los residuos #vemos que NO hay homoscedasticidad
plot(model01r)



model02<-glm(dmax_1.5~comp_1+comp_2+comp_3+csize+cond_fact+lfcm_tpsdig+wg)
model02
anova(model02)
summary(model02)
summary.lm(model02)




model02r<-stepAIC(model02)
summary(model02r)
anova(model02r)
model02r
summary.glm(model02r)

hist(resid(model02r)) #Vemos q  existe distr norm de los residuos
qqnorm(resid(model02r)) #Vemos q existe distr norm por linealidad
qqline(resid(model02r)) #lo que deberiamos obtener siguiendo normalidad
qqPlot(resid(model02r)) #esto es del paquete "car"
plot(fitted(model02r), resid(model02r)) #primero fitted i dsps los residuos #vemos que NO hay homoscedasticidad
plot(model02r)



model03<-glm(gspeed_1.5~comp_1+comp_2+comp_3+csize+cond_fact+lfcm_tpsdig+wg)
model03
summary(model03)
summary.lm(model03)



model03r<-stepAIC(model03)
summary(model03r)
anova(model03r)
model03r
summary.glm(model03r)

hist(resid(model03r)) #Vemos q  existe distr norm de los residuos
qqnorm(resid(model03r)) #Vemos q existe distr norm por linealidad
qqline(resid(model03r)) #lo que deberiamos obtener siguiendo normalidad
qqPlot(resid(model03r)) #esto es del paquete "car"
plot(fitted(model03r), resid(model03r)) #primero fitted i dsps los residuos #vemos que NO hay homoscedasticidad
plot(model03r)



model04<-glm(sspeed_1.5~comp_1+comp_2+comp_3+csize+cond_fact+lfcm_tpsdig+wg)
model04
summary(model04)
summary.lm(model04)



model04r<-stepAIC(model04)
summary(model04r)
anova(model04r)
model04r
summary.glm(model04r)

hist(resid(model04r)) #Vemos q  existe distr norm de los residuos
qqnorm(resid(model04r)) #Vemos q existe distr norm por linealidad
qqline(resid(model04r)) #lo que deberiamos obtener siguiendo normalidad
qqPlot(resid(model04r)) #esto es del paquete "car"
plot(fitted(model04r), resid(model04r)) #primero fitted i dsps los residuos #vemos que NO hay homoscedasticidad
plot(model04r)



  
model05<-glm(fag_time_2.5~comp_1+comp_2+comp_3+csize+cond_fact+lfcm_tpsdig+wg)
model05
summary(model05)
summary.lm(model05)




model05r<-stepAIC(model05)
summary(model05r)
anova(model05r)
model05r
summary.glm(model05r)
summary.lm(model05r)

hist(resid(model05r)) #Vemos q  existe distr norm de los residuos
qqnorm(resid(model05r)) #Vemos q existe distr norm por linealidad
qqline(resid(model05r)) #lo que deberiamos obtener siguiendo normalidad
qqPlot(resid(model05r)) #esto es del paquete "car"
plot(fitted(model05r), resid(model04r)) #primero fitted i dsps los residuos #vemos que NO hay homoscedasticidad
plot(model05r)



model06<-glm(dmax_2.5~comp_1+comp_2+comp_3+csize+cond_fact+lfcm_tpsdig+wg)
model06
summary(model06)
summary.lm (model06)



model06r<-stepAIC(model06)
summary(model06r)
anova(model06r)
model06r
summary.glm(model06r)

hist(resid(model06r)) #Vemos q  existe distr norm de los residuos
qqnorm(resid(model06r)) #Vemos q existe distr norm por linealidad
qqline(resid(model06r)) #lo que deberiamos obtener siguiendo normalidad
qqPlot(resid(model06r)) #esto es del paquete "car"
plot(fitted(model06r), resid(model06r)) #primero fitted i dsps los residuos #vemos que NO hay homoscedasticidad
plot(model06r)



model07<-glm(gspeed_2.5~comp_1+comp_2+comp_3+csize+cond_fact+lfcm_tpsdig+wg)
model07
summary(model07)
summary.lm(model07)



model07r<-stepAIC(model07)
summary(model07r)
anova(model07r)
model07r
summary.glm(model07r)

hist(resid(model07r)) #Vemos q  existe distr norm de los residuos
qqnorm(resid(model07r)) #Vemos q existe distr norm por linealidad
qqline(resid(model07r)) #lo que deberiamos obtener siguiendo normalidad
qqPlot(resid(model07r)) #esto es del paquete "car"
plot(fitted(model07r), resid(model07r)) #primero fitted i dsps los residuos #vemos que NO hay homoscedasticidad
plot(model07r)



model08<-glm(sspeed_2.5~comp_1+comp_2+comp_3+csize+cond_fact+lfcm_tpsdig+wg)
model08
summary(model08)
summary.lm(model08)



model08r<-stepAIC(model08)
summary(model08r)
anova(model08r)
model08r
summary.glm(model08r)

hist(resid(model08r)) #Vemos q  existe distr norm de los residuos
qqnorm(resid(model08r)) #Vemos q existe distr norm por linealidad
qqline(resid(model08r)) #lo que deberiamos obtener siguiendo normalidad
qqPlot(resid(model08r)) #esto es del paquete "car"
plot(fitted(model08r), resid(model08r)) #primero fitted i dsps los residuos #vemos que NO hay homoscedasticidad
plot(model08r)




  
model09<-glm(fag_time_3~comp_1+comp_2+comp_3+csize+cond_fact+lfcm_tpsdig+wg)
model09
summary(model09)
summary.lm (model09)



model09r<-stepAIC(model09)
summary(model09r)
anova(model09r)
model09r
summary.glm(model09r)

hist(resid(model09r)) #Vemos q  existe distr norm de los residuos
qqnorm(resid(model09r)) #Vemos q existe distr norm por linealidad
qqline(resid(model09r)) #lo que deberiamos obtener siguiendo normalidad
qqPlot(resid(model09r)) #esto es del paquete "car"
plot(fitted(model09r), resid(model09r)) #primero fitted i dsps los residuos #vemos que NO hay homoscedasticidad
plot(model09r)



model10<-glm(dmax_3~comp_1+comp_2+comp_3+csize+cond_fact+lfcm_tpsdig+wg)
model10
summary(model10)
summary.lm(model10)



model10r<-stepAIC(model10)
summary(model10r)
anova(model10r)
model10r
summary.glm(model10r)

hist(resid(model10r)) #Vemos q  existe distr norm de los residuos
qqnorm(resid(model10r)) #Vemos q existe distr norm por linealidad
qqline(resid(model10r)) #lo que deberiamos obtener siguiendo normalidad
qqPlot(resid(model10r)) #esto es del paquete "car"
plot(fitted(model10r), resid(model10r)) #primero fitted i dsps los residuos #vemos que NO hay homoscedasticidad
plot(model10r)



model11<-glm(gspeed_3~comp_1+comp_2+comp_3+csize+cond_fact+lfcm_tpsdig+wg)
model11
summary(model11)
summary.lm (model11)



model11r<-stepAIC(model11)
summary(model11r)
anova(model11r)
model11r
summary.glm(model11r)

hist(resid(model11r)) #Vemos q  existe distr norm de los residuos
qqnorm(resid(model11r)) #Vemos q existe distr norm por linealidad
qqline(resid(model11r)) #lo que deberiamos obtener siguiendo normalidad
qqPlot(resid(model11r)) #esto es del paquete "car"
plot(fitted(model11r), resid(model11r)) #primero fitted i dsps los residuos #vemos que NO hay homoscedasticidad
plot(model11r)



model12<-glm(sspeed_3~comp_1+comp_2+comp_3+csize+cond_fact+lfcm_tpsdig+wg)
model12
summary(model12)
summary.lm(model12)




model12r<-stepAIC(model12)
summary(model12r)
anova(model12r)
model12r
summary.glm(model12r)

hist(resid(model12r)) #Vemos q  existe distr norm de los residuos
qqnorm(resid(model12r)) #Vemos q existe distr norm por linealidad
qqline(resid(model12r)) #lo que deberiamos obtener siguiendo normalidad
qqPlot(resid(model12r)) #esto es del paquete "car"
plot(fitted(model12r), resid(model12r)) #primero fitted i dsps los residuos #vemos que NO hay homoscedasticidad
plot(model12r)
