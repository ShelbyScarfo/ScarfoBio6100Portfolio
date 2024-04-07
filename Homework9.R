# Generating the mean, sd, and sample size of my data
# Input ALW_CPUE_basin

ALW_CPUE_basin <- read.csv("ALW_CPUE_basin.csv")

Data_Parameters <- function(my_data){
  group_by(ALW_CPUE_basin, minor_basin) %>%
  summarise(
    count = n(),
    mean = mean(CPUE, na.rm = TRUE),
    sd = sd(CPUE, na.rm = TRUE)
  )
}

#f1 <- Data_Parameters(my_data =ALW_CPUE_basin)


Fake_Data <- function(nMean, nSD, nSize){
  nName <- c("North Main Lake", "Central Main Lake", "South Main Lake")
  ID <- 1:(sum(nSize))
  modelCPUE <- c(rtruncnorm(n=nSize[1], a = 0, b = Inf, mean=nMean[1],sd=nSD[1]),
               rtruncnorm(n=nSize[2], a = 0, b = Inf, mean=nMean[2],sd=nSD[2]),
               rtruncnorm(n=nSize[3], a = 0, b = Inf, mean=nMean[3],sd=nSD[3]))
TGroup <- rep(nName,nSize)
ANOdata <- data.frame(ID,TGroup,modelCPUE)
}

#f2 <- Fake_Data(nMean = f1$mean, nSD = f1$sd, nSize = f1$count)

ANO_model_figure <- function(modelCPUE, ANOdata, TGroup){
  model = aov(modelCPUE~TGroup,data=ANOdata)
print(summary(model))
ANOPlot <- ggplot(data=ANOdata,aes(x=TGroup,y=modelCPUE,fill=TGroup)) +
  geom_boxplot()
print(ANOPlot)
}

#f3 <- ANO_model_figure(modelCPUE = f2$modelCPUE, ANOdata = f2, TGroup = f2$TGroup)



#### Question 2

#Reordering the N, C, S boxes
Fake_Data_ordered <- function(nMean, nSD, nSize){
  nName <- c("North Main Lake", "Central Main Lake", "South Main Lake")
  ID <- 1:(sum(nSize))
  modelCPUE <- c(rtruncnorm(n=nSize[1], a = 0, b = Inf, mean=nMean[1],sd=nSD[1]),
                 rtruncnorm(n=nSize[2], a = 0, b = Inf, mean=nMean[2],sd=nSD[2]),
                 rtruncnorm(n=nSize[3], a = 0, b = Inf, mean=nMean[3],sd=nSD[3]))
  TGroup <- rep(nName,nSize)
  ANOdata <- data.frame(ID,TGroup,modelCPUE)
  ANOdata$TGroup <- factor(ANOdata$TGroup, levels = c("North Main Lake", "Central Main Lake", "South Main Lake"))
  ANOdata <- as.data.frame(ANOdata)
}

#f4 <- Fake_Data_ordered(nMean = f1$mean, nSD = f1$sd, nSize = f1$count)

#Adding in a Tukey test to the analysis

ANO_model_figure_PlusTukey <- function(modelCPUE, ANOdata, TGroup){
  model = aov(modelCPUE~TGroup,data=ANOdata)
  print(summary(model))
  ANOPlot <- ggplot(data=ANOdata,aes(x=TGroup,y=modelCPUE,fill=TGroup)) +
    geom_boxplot()
  print(ANOPlot)
  Tukey <- TukeyHSD(model)
  print(Tukey)
}


#f5 <-ANO_model_figure_PlusTukey(modelCPUE = f4$modelCPUE, ANOdata = f4, TGroup = f4$TGroup)
