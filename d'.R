
##  计算分享辨别力d'和分享倾向c ####

##################################计算不同组均值和se的代码####################3

mystats <- function(x,na.omit=FALSE){
  if(na.omit)
    x<-x[!is.na(x)]
  m<-mean(x)
  n<-length(x)
  SD <- sd(x)
  s<-sd(x)/sqrt(n)
  return(c(n = n,mean = m,se =s, sd = SD))
}

###############################################################################

#install.packages("hrbrthemes")
#开始之前需要安装下面的package,安装的exmaple如上所示

# packages --------------------------------------------------------------------
install.packages('hrbrthemes')
install.packages('glue')
install.packages("reshape2")
install.packages("doBy")
install.packages("dplyr")
library(tidyverse)
library(hrbrthemes)
library(glue)
library(doBy)
library(readxl) # Read Excel files
library(plyr)  #用于重命名
library(tibble)
library(reshape2)
library(dplyr)
library(tidyr)

theme_set(theme_ipsum())

################ read data  ##################3

DDD <- read_excel("C:/Users/GL/Desktop/syllogism2/Data.xlsx")
colnames(DDD)

data_analysis <- DDD %>%
  select(Sub_ID,Age,Gender,Material_ID,Group,lc,cc,Sharing,Accuracy,Value,Pleasure,Familiarity,Logic,Discrimination) %>%
  mutate(ACC_decision = 
           case_when(Accuracy <= 3 ~ 'False',
                     Accuracy <= 6 ~ 'True')) %>%
  drop_na()%>%
  reshape2::dcast(Sub_ID + Age + Gender + Material_ID  + Group + lc + Value + Pleasure + Familiarity + Logic + Discrimination~
                    cc + ACC_decision,
                  fun.aggregate = length) %>%
  as_tibble()%>% 
    #########
  #%%是递归，意思是自动调取前面的这个数据，reshape这些名字别改，上面reshape这里是根据左边的条件算右边的变量的个数。所以这里是，算同时满足subject + mist + intv  + sex + iNvalence + iNarousal这些条件，iNreality + sharing_decision（相当于2*2）的个数，就是在counting。+是且的关系
  # computing d' sensitivity index with log-linear rule correction 
  # (Hautus, 1995)
  # >>>
  mutate(fake_False    = fake_False    + .5,
         fake_True           = fake_True        + .5,
         truth_False          = truth_False          + .5,
         truth_True             = truth_True             +.5)  %>%
  #这是一种数据矫正方法,这里+0.5是为了算c和d不出现极端值For such cases, MacMillan and Creelman (2004) suggested to “convert proportions of 0 and 1 to 1/(2N) and 1 ??? 1/(2N),respectively, where N is the number of trials on which the pro-portion is based” (p. 8). An alternative strategy is to “add 0.5 toall data cells regardless of whether zeroes are present” (p. 8).
  #  <<<
  #  <<<
    
  mutate(hit_rate = (truth_True) / (truth_True + truth_False),
         fa_rate  = (fake_True) / (fake_True + fake_False),
         dprime   = qnorm(hit_rate) - qnorm(fa_rate),
         c        = -1 * (qnorm(hit_rate) + qnorm(fa_rate)) / 2 ) 

#这里dprime就是算d'然后是把比率取z分数  用qnorm这个命令，c就是算c 下面的内容（参考文献里原来的代码，算的是信任辨别力d'，accurate为判断新闻真假）只是方便看着代码改
# mutate(fake_inaccurate   = fake_inaccurate   + .5,
#        fake_accurate     = fake_accurate     + .5,
#        true_inaccurate        = true_inaccurate        + .5,
#        true_accurate       = true_accurate       +.5)  %>%
#   
#   #  <<<
#   mutate(hit_rate = (true_accurate) / (true_inaccurate + true_accurate),
#          fa_rate  = (fake_accurate) / (fake_accurate + fake_inaccurate),
#          dprime   = qnorm(hit_rate) - qnorm(fa_rate),
#          c        = -1 * (qnorm(hit_rate) + qnorm(fa_rate)) / 2 ) 

##anova

# data_control_select <- data_analysis_wy_main %>% 
#   subset(mist == 'control' & sex == 'female')
#如果是想选数据 控制住一个变量分析 就用这行命令

######  run Anova in R ######

####  afex ####
#install.packages("afex")
#library(lme4)
install.packages("afex")
install.packages("emmeans")
library(afex)
library(lme4)
library(emmeans)

data_analysis$Group <- factor(data_analysis$Group,levels = c('1','2'))
data_analysis$lc <- factor(data_analysis$lc,levels = c('truth','fake'))
data_analysis$Gender <- factor(data_analysis$Gender,levels = c('1','2'))


contrasts(data_analysis$Group) <- c(0.5, -0.5)
contrasts(data_analysis$Group)
contrasts(data_analysis$lc) <- c(0.5, -0.5)
contrasts(data_analysis$lc)
contrasts(data_analysis$Gender) <- c(0.5, -0.5)
contrasts(data_analysis$Gender)

data_analysis$Value.z = scale(data_analysis$Value)[,1]
data_analysis$Pleasure.z = scale(data_analysis$Pleasure)[,1]
data_analysis$Familiarity.z = scale(data_analysis$Familiarity)[,1]
data_analysis$Logic.z = scale(data_analysis$Logic)[,1]
data_analysis$Discrimination.z = scale(data_analysis$Discrimination)[,1]

model1 <- lmer(dprime ~ Group * lc  + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + Gender + 
                  (1|Material_ID), 
                data = data_analysis,REML = FALSE, 
                control = lmerControl(optimizer = 'bobyqa'))
isSingular(model1)
summary(model1)
emmeans(model1, pairwise ~ Group, adjust = 'sidak')
emmeans(model1, pairwise ~ lc, adjust = 'sidak')


###

#作图   分享评分的2重交互作用 model_1  实验二分享的d'
library(plyr)
library(ggplot2)

data_count2d<- summaryBy(dprime~intv*mist ,data = data_analysis_wy_main,FUN = mystats)
pe2dps <- ggplot(data_count2d, aes(x = intv, y = dprime.mean, fill = mist)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge(),width = 0.68,size = 1)+
  geom_errorbar(aes(ymin = dprime.mean - dprime.se, ymax = dprime.mean + dprime.se), 
                width = 0.2, position = position_dodge(0.68))+
  theme_classic() +labs(x = '干预方式', y = '分享辨别力d’',fill = '诱发方式')+
  scale_x_discrete(breaks=c('blank','mind'),labels =c('空白组','提醒组'))+
  scale_fill_discrete(breaks=c('control','stress'),labels =c('控制组','应激组'))

