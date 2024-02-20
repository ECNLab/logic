install.packages("readxl")
install.packages('data.table')
install.packages('ggplot2')
install.packages('gridExtra')
install.packages('ggthemes')
install.packages('tidyverse')
install.packages('mixedpower')
install.packages('lmerTest')
install.packages('lme4')
install.packages('emmeans')
install.packages('effectsize')
install.packages('xvalglms')
install.packages('Rmisc')
install.packages("openxlsx")
install.packages("writexl")
install.packages("devtools")
install.packages("foreach")
install.packages("doParallel")
install.packages("iterators")
install.packages("parallel")
install.packages("gridExtra")
install.packages("Matrix")
install.packages('rmarkdown')
install.packages("pbkrtest")
library(readxl)
library(data.table)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(tidyverse)
library(mixedpower)
library(lme4)
library(Matrix)
library(lmerTest)
library(pbkrtest)
library(emmeans)
library(effectsize)
library(xvalglms)
library(Rmisc)
library(plyr)
library(lattice)
library(Rmisc)
library(openxlsx)
library(writexl)
library(foreach)
library(iterators)
library(parallel)
library(doParallel)
library(gridExtra)
library(xvalglms)
library(rmarkdown)
syllogism.2 <- read_excel("C:/Users/GL/Desktop/syllogism.2/Data.xlsx")

syllogism.2$Group <- factor(syllogism.2$Group,levels = c('1','2'))
syllogism.2$LC <- factor(syllogism.2$LC,levels = c('1','2'))
syllogism.2$CC <- factor(syllogism.2$CC,levels = c('1','2'))
syllogism.2$Gender <- factor(syllogism.2$Gender,levels = c('1','2'))


contrasts(syllogism.2$Group) <- c(0.5, -0.5)
contrasts(syllogism.2$Group)
contrasts(syllogism.2$LC) <- c(0.5, -0.5)
contrasts(syllogism.2$LC)
contrasts(syllogism.2$CC) <- c(0.5, -0.5)
contrasts(syllogism.2$CC)
contrasts(syllogism.2$Gender) <- c(0.5, -0.5)
contrasts(syllogism.2$Gender)

syllogism.2$Value.z = scale(syllogism.2$Value)[,1]
syllogism.2$Pleasure.z = scale(syllogism.2$Pleasure)[,1]
syllogism.2$Familiarity.z = scale(syllogism.2$Familiarity)[,1]
syllogism.2$Logic.z = scale(syllogism.2$Logic)[,1]
syllogism.2$Discrimination.z = scale(syllogism.2$Discrimination)[,1]



str(syllogism.2)

######################################share###############################################
#经验选择，并通过anova比较逐步剔除变量，最后取最简洁的模型
modelS1 <- lmer(Sharing ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + Gender + 
                 (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                   data = syllogism.2,REML = FALSE, 
               control = lmerControl(optimizer = 'bobyqa'))
summary(modelS1)
#Value,familiarity,Gender效应不显著，依次剔除
modelS2 <- lmer(Sharing ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + 
                  (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                data = syllogism.2,REML = FALSE,
                control = lmerControl(optimizer = 'bobyqa'))
AIC(modelS2)
summary(modelS2)
anova(modelS1,modelS2)

modelS3 <- lmer(Sharing ~ Group * LC * CC + Logic.z +Discrimination.z + Value.z + Pleasure.z + 
                 (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
               data = syllogism.2,REML = FALSE,
               control = lmerControl(optimizer = 'bobyqa'))
summary(modelS3)
anova(modelS1,modelS3)

modelS4 <- lmer(Sharing ~ Group * LC * CC + Logic.z +Discrimination.z + Pleasure.z +
                 (1+ 1|Sub_ID)+(1+ 1|Material_ID), 
               data = syllogism.2,REML = FALSE,
               control = lmerControl(optimizer = 'bobyqa'))
summary(modelS4)
AIC(modelS4)
anova(modelS1,modelS4)
ranova(modelS4)
modelS5 <- lmer(Sharing ~ Group * LC * CC + Logic.z +Discrimination.z + Pleasure.z +  
                   (1+ logicscore|Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
ranova(modelS5)

#交叉验证
models = vector(mode = "list", length = 4)
models[[1]] = Sharing ~ 1
models[[2]] = Sharing ~ Group + LC + CC
models[[3]] = Sharing ~ Group + LC * CC
models[[4]] = Sharing ~ Group * LC * CC
output = xval.glm(data = syllogism.2, models)
#model4wins,纳入其他有关变量
models = vector(mode = "list", length = 4)
models[[1]] = Sharing ~ Group * LC * CC
models[[2]] = Sharing ~ Group * LC * CC + Logic.z +Discrimination.z + Pleasure.z
models[[3]] = Sharing ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z
models[[4]] = Sharing ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + Gender
output = xval.glm(data = syllogism.2, models)
#model3wins，权衡之下保留Familiarity.z和Value.z，即选择modelS2
#检验随机因子显著性
Smodel_1 <- lmer(Sharing ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + 
                   (1+ 1 |Sub_ID), 
                 data = syllogism.2,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
Smodel_2 <- lmer(Sharing ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + 
                   (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
Smodel_3 <- lmer(Sharing ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + 
                   (1+ logicscore |Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
ranova(Smodel_1)
ranova(Smodel_2)
ranova(Smodel_3)
#结果同上，选择 (1 + 1 | Sub_ID) + (1 + 1 | Material_ID) 的随机因子设置

eta_squared(modelS2)


emmeans(modelS2, pairwise ~ Group | LC, adjust = 'sidak')
emmeans(modelS2, pairwise ~ LC | Group, adjust = 'sidak')
emmip(modelS2, Group ~ LC, CIs = TRUE)

emmeans(modelS2, pairwise ~ Group | CC, adjust = 'sidak')
emmeans(modelS2, pairwise ~ CC | Group, adjust = 'sidak')
emmip(modelS2, Group ~ CC, CIs = TRUE)

######################################Accuracy###############################################
modelA1 <- lmer(Accuracy ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + Gender + 
                  (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                data = syllogism.2,REML = FALSE, 
                control = lmerControl(optimizer = 'bobyqa'))
summary(modelA1)
#Discrimination,Pleasure，Gender效应不显著，依次剔除
modelA2 <- lmer(Accuracy ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z +
                  (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                data = syllogism.2,REML = FALSE,
                control = lmerControl(optimizer = 'bobyqa'))
summary(modelA2)
anova(modelA1,modelA2)
modelA3 <- lmer(Accuracy ~ Group * LC * CC + Logic.z + Familiarity.z + Value.z + Pleasure.z +
                  (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                data = syllogism.2,REML = FALSE,
                control = lmerControl(optimizer = 'bobyqa'))
summary(modelA3)
anova(modelA1,modelA3)

modelA4 <- lmer(Accuracy ~ Group * LC * CC + Logic.z + Familiarity.z + Value.z + 
                  (1+ 1|Sub_ID)+(1+ 1|Material_ID), 
                data = syllogism.2,REML = FALSE,
                control = lmerControl(optimizer = 'bobyqa'))
summary(modelA4)
AIC(modelA4)
anova(modelA1,modelA4)

modelA5 <- lmer(Accuracy ~ Group * LC * CC + Logic.z + Familiarity.z + Value.z +   
                  (1+ logicscore|Sub_ID)+(1+ 1|Material_ID), 
                data = syllogism.2,REML = FALSE,
                control = lmerControl(optimizer = 'bobyqa'))


#交叉验证
models = vector(mode = "list", length = 4)
models[[1]] = Accuracy ~ 1
models[[2]] = Accuracy ~ Group + LC + CC
models[[3]] = Accuracy ~ Group + LC * CC
models[[4]] = Accuracy ~ Group * LC * CC
output = xval.glm(data = syllogism.2, models)
#model4wins,纳入其他有关变量
models = vector(mode = "list", length = 4)
models[[1]] = Accuracy ~ Group * LC * CC
models[[2]] = Accuracy ~ Group * LC * CC + Logic.z + Familiarity.z + Value.z
models[[3]] = Accuracy ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z
models[[4]] = Accuracy ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + Gender
output = xval.glm(data = syllogism.2, models)
#model4wins，权衡之下保留Familiarity.z和Value.z，Gender即选择modelS1
#检验随机因子显著性
Smodel_1 <- lmer(Accuracy ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + 
                   (1+ 1 |Sub_ID), 
                 data = syllogism.2,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
Smodel_2 <- lmer(Accuracy ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + 
                   (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
Smodel_3 <- lmer(Accuracy ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + 
                   (1+ logicscore |Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
ranova(Smodel_1)
ranova(Smodel_2)
ranova(Smodel_3)
#结果同上，选择 (1 + 1 | Sub_ID) + (1 + 1 | Material_ID) 的随机因子设置
emmip(modelA2, Group ~ LC, CIs = TRUE)
emmip(modelA2, Group ~ CC, CIs = TRUE)

syllogism.2$Accuracy <- as.numeric(syllogism.2$Accuracy)
syllogism.2$CC <- as.factor(syllogism.2$CC)
conclusion_labels <- c('1' = "conclusion_true", '2' = "conclusion_false") # 标签设置
emmip(modelA2, Group ~ LC | CC, CIs = TRUE) + 
  facet_grid(cols = vars(CC), labeller = labeller(CC = conclusion_labels)) +
  scale_color_discrete(label = c("Low", "High")) +
  scale_x_discrete(labels = c("true", "false")) +
  scale_y_continuous(name = "Accuracy") +
  theme_classic() +
  theme(strip.text = element_text(size = 12), strip.background = element_rect(colour = "white"), 
        legend.title = element_blank(), 
        axis.text.x = element_text(size = 9))
##################################Eyemovement############################
emerged_dir <- 'C:/Users/GL/Desktop/syllogism.2/organize/eye'
emerged_filelist <- list.files(path = emerged_dir, pattern = "*.xlsx$", recursive = TRUE, full.names = TRUE)
emerged_data <- data.frame()
for (i in 1:length(emerged_filelist)) {
  emerged_filename <- emerged_filelist[i]
  
  # 使用 read_excel 函数并设置 col_names 和 range 参数
  emerged_Readfile <- read_excel(emerged_filename, col_names = TRUE, range = "A1:DQ49")
  # 选择所需的列
  emerged_Readfile <- select(emerged_Readfile,RECORDING_SESSION_LABEL,IA_DWELL_TIME,IA_FIXATION_COUNT,IA_REGRESSION_OUT_COUNT,IA_LABEL,lxhealth)
  # 将数据添加到 emerged_data
  emerged_data <- rbind(emerged_data, emerged_Readfile)
}
# 保存数据
write.xlsx(emerged_data, 'C:/Users/GL/Desktop/syllogism.2/organize/eye/E.xlsx')

syllogism.2E <- read_excel("C:/Users/GL/Desktop/syllogism.2/Data.xlsx", sheet = "Sheet2")
syllogism.2E$Group <- factor(syllogism.2E$Group,levels = c('1','2'))
syllogism.2E$LC <- factor(syllogism.2E$LC,levels = c('1','2'))
syllogism.2E$CC <- factor(syllogism.2E$CC,levels = c('1','2'))
syllogism.2E$Gender <- factor(syllogism.2E$Gender,levels = c('1','2'))


contrasts(syllogism.2E$Group) <- c(0.5, -0.5)
contrasts(syllogism.2E$Group)
contrasts(syllogism.2E$LC) <- c(0.5, -0.5)
contrasts(syllogism.2E$LC)
contrasts(syllogism.2E$CC) <- c(0.5, -0.5)
contrasts(syllogism.2E$CC)
contrasts(syllogism.2E$Gender) <- c(0.5, -0.5)
contrasts(syllogism.2E$Gender)

syllogism.2E$Value.z = scale(syllogism.2E$Value)[,1]
syllogism.2E$Pleasure.z = scale(syllogism.2E$Pleasure)[,1]
syllogism.2E$Familiarity.z = scale(syllogism.2E$Familiarity)[,1]
syllogism.2E$Logic.z = scale(syllogism.2E$Logic)[,1]
syllogism.2E$Discrimination.z = scale(syllogism.2E$Discrimination)[,1]


##################################Pdwelltime############################

syllogism.2E$Pdwelltime_log <- log(syllogism.2E$Pdwelltime)
modelPd1 <- lmer(Pdwelltime_log ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + Gender + 
                     (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                   data = syllogism.2E,REML = FALSE, 
                   control = lmerControl(optimizer = 'bobyqa'))
AIC(modelPd1)
summary(modelPd1) 
#结果显示 Logic、Value、Pleasure、Gender均无效应，后续一一剔除，使用anova()检查剔除前后的模型，均不存在显著差异
modelPd2 <- lmer(Pdwelltime_log ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Gender +
                  (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                data = syllogism.2E,REML = FALSE, 
                control = lmerControl(optimizer = 'bobyqa'))
summary(modelPd2)
anova(modelPd1,modelPd2)

modelPd3 <- lmer(Pdwelltime_log ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Gender +
                  (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                data = syllogism.2E,REML = FALSE,
                control = lmerControl(optimizer = 'bobyqa'))
summary(modelPd3)
anova(modelPd1,modelPd3)

modelPd4 <- lmer(Pdwelltime_log ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z +  
                  (1+ 1|Sub_ID)+(1+ 1|Material_ID), 
                data = syllogism.2E,REML = FALSE,
                control = lmerControl(optimizer = 'bobyqa'))
AIC(modelPd4)
summary(modelPd4)
anova(modelPd1,modelPd4)

modelPd5 <- lmer(Pdwelltime_log ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z +  
                   (1+ logicscore|Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
AIC(modelPd5)
summary(modelPd5)
ranova(modelPd5)


eta_squared(modelPd1)

#交叉验证
models = vector(mode = "list", length = 4)
models[[1]] = Pdwelltime_log ~ 1
models[[2]] = Pdwelltime_log ~ Group + LC + CC
models[[3]] = Pdwelltime_log ~ Group + LC * CC
models[[4]] = Pdwelltime_log ~ Group * LC * CC
output = xval.glm(data = syllogism.2E, models)
#model4wins,纳入其他有关变量
models = vector(mode = "list", length = 4)
models[[1]] = Pdwelltime_log ~ Group * LC * CC
models[[2]] = Pdwelltime_log ~ Group * LC * CC + Logic.z +Discrimination.z + Pleasure.z
models[[3]] = Pdwelltime_log ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z
models[[4]] = Pdwelltime_log ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + Gender
output = xval.glm(data = syllogism.2E, models)
#model3wins，权衡之下保留Familiarity.z和Value.z，即选择modelS2
#检验随机因子显著性
Pdmodel_1 <- lmer(Pdwelltime_log ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + 
                   (1+ 1 |Sub_ID), 
                 data = syllogism.2E,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
Pdmodel_2 <- lmer(Pdwelltime_log ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + 
                   (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
Pdmodel_3 <- lmer(Pdwelltime_log ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + 
                   (1+ logicscore |Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
ranova(Pdmodel_1)
ranova(Pdmodel_2)
ranova(Pdmodel_3)
#结果同上，选择 (1 + 1 | Sub_ID) + (1 + 1 | Material_ID) 的随机因子设置

#画出三项交互图
syllogism.2E$Pdwelltime_log <- as.numeric(syllogism.2E$Pdwelltime_log)
syllogism.2E$CC <- as.factor(syllogism.2E$CC)
conclusion_labels <- c('1' = "conclusion_true", '2' = "conclusion_false") # 标签设置
emmip(modelPd3, Group ~ LC | CC, CIs = TRUE) + 
  facet_grid(cols = vars(CC), labeller = labeller(CC = conclusion_labels)) +
  scale_color_discrete(label = c("Low", "High")) +
  scale_x_discrete(labels = c("true", "false")) +
  scale_y_continuous(name = "Pdwelltime_log") +
  theme_classic() +
  theme(strip.text = element_text(size = 12), strip.background = element_rect(colour = "white"), 
        legend.title = element_blank(), 
        axis.text.x = element_text(size = 9))


emmeans(modelPd3, pairwise ~ Group | LC | CC, adjust = 'sidak')
emmeans(modelPd3, pairwise ~ LC | CC| Group, adjust = 'sidak')
emmeans(modelPd3, pairwise ~ CC | LC| Group, adjust = 'sidak')




#################################Pfixation###########################
modelPf1 <- lmer(Pfixation ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + Gender +
                   (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE, 
                 control = lmerControl(optimizer = 'bobyqa'))
summary(modelPf1)
#结果显示Value、Pleasure均无效应，后续一一剔除，使用anova()检查剔除前后的模型，均不存在显著差异
modelPf2 <- lmer(Pfixation ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Gender + 
                   (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE, 
                 control = lmerControl(optimizer = 'bobyqa'))
summary(modelPf2)
AIC(modelPf1)
anova(modelPf1,modelPf2)

modelPf3 <- lmer(Pfixation ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Gender + 
                   (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
summary(modelPf3)
anova(modelPf1,modelPf3)

modelPf4 <- lmer(Pfixation ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z +  
                   (1+ 1|Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
AIC(modelPf4)
summary(modelPf4)
ranova(modelPf4)
anova(modelPf1,modelPf4)

modelPf5 <- lmer(Pfixation ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z +  
                   (1+ logicscore|Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
AIC(modelPf5)
summary(modelPf5)
ranova(modelPf5)


#交叉验证
models = vector(mode = "list", length = 4)
models[[1]] = Pfixation ~ 1
models[[2]] = Pfixation ~ Group + LC + CC
models[[3]] = Pfixation ~ Group + LC * CC
models[[4]] = Pfixation ~ Group * LC * CC
output = xval.glm(data = syllogism.2E, models)
#model4wins,纳入其他有关变量
models = vector(mode = "list", length = 4)
models[[1]] = Pfixation ~ Group * LC * CC
models[[2]] = Pfixation ~ Group * LC * CC + Logic.z +Discrimination.z + Pleasure.z

models[[3]] = Pfixation ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z
models[[4]] = Pfixation ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + Gender
output = xval.glm(data = syllogism.2E, models)
#model4wins，权衡之下保留Familiarity.z和Value.z和Gender，即选择modelPf1
#检验随机因子显著性
Pfmodel_1 <- lmer(Pfixation ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + 
                    (1+ 1 |Sub_ID), 
                  data = syllogism.2E,REML = FALSE,
                  control = lmerControl(optimizer = 'bobyqa'))
Pfmodel_2 <- lmer(Pfixation ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + 
                    (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                  data = syllogism.2E,REML = FALSE,
                  control = lmerControl(optimizer = 'bobyqa'))
Pfmodel_3 <- lmer(Pfixation ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + 
                    (1+ logicscore |Sub_ID)+(1+ 1|Material_ID), 
                  data = syllogism.2E,REML = FALSE,
                  control = lmerControl(optimizer = 'bobyqa'))
ranova(Pfmodel_1)
ranova(Pfmodel_2)
ranova(Pfmodel_3)
eta_squared(modelPf3)


# 绘制三元交互图
syllogism.2E$Pfixation <- as.numeric(syllogism.2E$Pfixation)
syllogism.2E$CC <- as.factor(syllogism.2E$CC)
conclusion_labels <- c('1' = "conclusion_true", '2' = "conclusion_false") # 标签设置
emmip(modelPf3, Group ~ LC | CC, CIs = TRUE) + 
  facet_grid(cols = vars(CC), labeller = labeller(CC = conclusion_labels)) +
  scale_color_discrete(label = c("Low", "High")) +
  scale_x_discrete(labels = c("true", "false")) +
  scale_y_continuous(name = "Pfixation") +
  theme_classic() +
  theme(strip.text = element_text(size = 12), strip.background = element_rect(colour = "white"), 
        legend.title = element_blank(), 
        axis.text.x = element_text(size = 9))
  

emmeans(modelPf3, pairwise ~ Group | LC | CC, adjust = 'sidak')
emmeans(modelPf3, pairwise ~ LC | CC| Group, adjust = 'sidak')
emmeans(modelPf3, pairwise ~ CC | LC| Group, adjust = 'sidak')


#############################################Cdwelltime#################
syllogism.2E$Cdwelltime_log <- log(syllogism.2E$Cdwelltime)
modelCd1 <- lmer(Cdwelltime_log ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + Gender +
                   (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE, 
                 control = lmerControl(optimizer = 'bobyqa'))
summary(modelCd1)
AIC(modelCd1)
#结果显示 除discrimination均无效应，后续一一剔除，使用anova()检查剔除前后的模型，均不存在显著差异
modelCd2 <- lmer(Cdwelltime_log ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Pleasure.z + Gender +
                   (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE, 
                 control = lmerControl(optimizer = 'bobyqa'))
summary(modelCd2)
anova(modelCd1,modelCd2)


modelCd3 <- lmer(Cdwelltime_log ~ Group * LC * CC + Logic.z + Discrimination.z +  Pleasure.z + Gender +
                   (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
summary(modelCd3)
anova(modelCd1,modelCd3)
modelCd4 <- lmer(Cdwelltime_log ~ Group * LC * CC +  Logic.z  + Discrimination.z + Gender + 
                   (1+ 1|Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
summary(modelCd4)
anova(modelCd1,modelCd4)
modelCd5 <- lmer(Cdwelltime_log ~ Group * LC * CC +  Logic.z  + Discrimination.z +   
                   (1+ 1|Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
summary(modelCd5)
anova(modelCd1,modelCd5)
modelCd6 <- lmer(Cdwelltime_log ~ Group * LC * CC  + Discrimination.z +   
                   (1+ 1|Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
summary(modelCd6)
anova(modelCd1,modelCd6)

modelCd7 <- lmer(Cdwelltime_log ~ Group * LC * CC + Discrimination.z +  
                   (1+ logicscore|Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
AIC(modelCd7)
summary(modelCd7)
ranova(modelCd7)


eta_squared(modelCd5)

#交叉验证
models = vector(mode = "list", length = 4)
models[[1]] = Cdwelltime_log ~ 1
models[[2]] = Cdwelltime_log ~ Group + LC + CC
models[[3]] = Cdwelltime_log ~ Group + LC * CC
models[[4]] = Cdwelltime_log ~ Group * LC * CC
output = xval.glm(data = syllogism.2E, models)
#model4wins,纳入其他有关变量
models = vector(mode = "list", length = 7)
models[[1]] = Cdwelltime_log ~ Group * LC * CC
models[[2]] = Cdwelltime_log ~ Group * LC * CC + Discrimination.z
models[[3]] = Cdwelltime_log ~ Group * LC * CC + Logic.z + Discrimination.z
models[[4]] = Cdwelltime_log ~ Group * LC * CC + Logic.z + Discrimination.z + Gender
models[[5]] = Cdwelltime_log ~ Group * LC * CC + Logic.z + Discrimination.z +  Pleasure.z + Gender
models[[6]] = Cdwelltime_log ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Pleasure.z + Gender
models[[7]] = Cdwelltime_log ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + Gender
output = xval.glm(data = syllogism.2E, models)
#model7wins，权衡之下保留Familiarity.z和Value.z和Gender，即选择modelCd1
#检验随机因子显著性
Cdmodel_1 <- lmer(Cdwelltime_log ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + 
                    (1+ 1 |Sub_ID), 
                  data = syllogism.2E,REML = FALSE,
                  control = lmerControl(optimizer = 'bobyqa'))
Cdmodel_2 <- lmer(Cdwelltime_log ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + 
                    (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                  data = syllogism.2E,REML = FALSE,
                  control = lmerControl(optimizer = 'bobyqa'))
Cdmodel_3 <- lmer(Cdwelltime_log ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + 
                    (1+ logicscore |Sub_ID)+(1+ 1|Material_ID), 
                  data = syllogism.2E,REML = FALSE,
                  control = lmerControl(optimizer = 'bobyqa'))
ranova(Cdmodel_1)
ranova(Cdmodel_2)
ranova(Cdmodel_3)
eta_squared(modelCd1)

#绘制三项图
syllogism.2E$Cdwelltime_log <- as.numeric(syllogism.2E$Cdwelltime_log)
syllogism.2E$CC <- as.factor(syllogism.2E$CC)
conclusion_labels <- c('1' = "conclusion_true", '2' = "conclusion_false") # 标签设置
emmip(modelCd1, Group ~ LC | CC, CIs = TRUE) + 
  facet_grid(cols = vars(CC), labeller = labeller(CC = conclusion_labels)) +
  scale_color_discrete(label = c("Low", "High")) +
  scale_x_discrete(labels = c("true", "false")) +
  scale_y_continuous(name = "Cdwelltime_log") +
  theme_classic() +
  theme(strip.text = element_text(size = 12), strip.background = element_rect(colour = "white"), 
        legend.title = element_blank(), 
        axis.text.x = element_text(size = 9))


emmeans(modelCd1, pairwise ~ Group | LC | CC, adjust = 'sidak')
emmeans(modelCd1, pairwise ~ LC | CC| Group, adjust = 'sidak')
emmeans(modelCd1, pairwise ~ CC | LC| Group, adjust = 'sidak')
emmip(modelCd5, Group ~ LC ~ CC, CIs = TRUE)

########################################Cfixation########################
modelCf1 <- lmer(Cfixation ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + Gender +
                   (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE, 
                 control = lmerControl(optimizer = 'bobyqa'))
summary(modelCf1)
#结果显示除discrimination均无效应均无效应，后续一一剔除，使用anova()检查剔除前后的模型，均不存在显著差异
modelCf2 <- lmer(Cfixation ~ Group * LC * CC + Logic.z + Discrimination.z + Familiarity.z + Pleasure.z + Gender + 
                   (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
summary(modelCf2)
anova(modelCf1,modelCf2)

modelCf3 <- lmer(Cfixation ~ Group * LC * CC + Logic.z + Discrimination.z + Pleasure.z + Gender +  
                   (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
summary(modelCf3)
anova(modelCf1,modelCf3)

modelCf4 <- lmer(Cfixation ~ Group * LC * CC + Logic.z + Discrimination.z + Pleasure.z +   
                   (1+ 1|Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
summary(modelCf4)
anova(modelCf1,modelCf4)

modelCf5 <- lmer(Cfixation ~ Group * LC * CC + Discrimination.z + Pleasure.z +  
                   (1+ 1|Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
summary(modelCf5)
anova(modelCf1,modelCf5)

modelCf6 <- lmer(Cfixation ~ Group * LC * CC + Discrimination.z +  
                   (1+ 1|Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
summary(modelCf6)
anova(modelCf1,modelCf6)

modelCf7 <- lmer(Cfixation ~ Group * LC * CC + Discrimination.z +  
                   (1+ logicscore|Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
summary(modelCf7)
ranova(modelCf7)


eta_squared(modelCf6)

#交叉验证
models = vector(mode = "list", length = 4)
models[[1]] = Cfixation ~ 1
models[[2]] = Cfixation ~ Group + LC + CC
models[[3]] = Cfixation ~ Group + LC * CC
models[[4]] = Cfixation ~ Group * LC * CC
output = xval.glm(data = syllogism.2E, models)
#model4wins,纳入其他有关变量
models = vector(mode = "list", length = 7)
models[[1]] = Cfixation ~ Group * LC * CC
models[[2]] = Cfixation ~ Group * LC * CC + Discrimination.z
models[[3]] = Cfixation ~ Group * LC * CC + Pleasure.z + Discrimination.z
models[[4]] = Cfixation ~ Group * LC * CC + Logic.z + Pleasure.z + Discrimination.z
models[[5]] = Cfixation ~ Group * LC * CC + Logic.z + Pleasure.z + Discrimination.z + Gender
models[[6]] = Cfixation ~ Group * LC * CC + Logic.z + Pleasure.z + Discrimination.z + Gender + Familiarity.z
models[[7]] = Cfixation ~ Group * LC * CC + Logic.z + Pleasure.z + Discrimination.z + Gender + Familiarity.z + Value.z
output = xval.glm(data = syllogism.2E, models)
#model2wins，与经验选择一致，即选择modelCf6
#检验随机因子显著性
Cfmodel_1 <- lmer(Cfixation ~ Group * LC * CC + Logic.z + Discrimination.z
                    (1+ 1 |Sub_ID), 
                  data = syllogism.2E,REML = FALSE,
                  control = lmerControl(optimizer = 'bobyqa'))
Cfmodel_2 <- lmer(Cfixation ~ Group * LC * CC + Logic.z + Discrimination.z
                    (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                  data = syllogism.2E,REML = FALSE,
                  control = lmerControl(optimizer = 'bobyqa'))
Cfmodel_3 <- lmer(Cfixation ~ Group * LC * CC + Logic.z + Discrimination.z
                    (1+ logicscore |Sub_ID)+(1+ 1|Material_ID), 
                  data = syllogism.2E,REML = FALSE,
                  control = lmerControl(optimizer = 'bobyqa'))
ranova(Cfmodel_1)
ranova(Cfmodel_2)
ranova(Cfmodel_3)
eta_squared(modelCf1)

#绘制三项图
syllogism.2E$Cfixation <- as.numeric(syllogism.2E$Cfixation)
syllogism.2E$CC <- as.factor(syllogism.2E$CC)
conclusion_labels <- c('1' = "conclusion_true", '2' = "conclusion_false") # 标签设置
emmip(modelCf6, Group ~ LC | CC, CIs = TRUE) + 
  facet_grid(cols = vars(CC), labeller = labeller(CC = conclusion_labels)) +
  scale_color_discrete(label = c("Low", "High")) +
  scale_x_discrete(labels = c("true", "false")) +
  scale_y_continuous(name = "Cfixation") +
  theme_classic() +
  theme(strip.text = element_text(size = 12), strip.background = element_rect(colour = "white"), 
        legend.title = element_blank(), 
        axis.text.x = element_text(size = 9))

emmeans(modelCf6, pairwise ~ Group | LC | CC, adjust = 'sidak')
emmeans(modelCf6, pairwise ~ LC | CC| Group, adjust = 'sidak')
emmeans(modelCf6, pairwise ~ CC | LC| Group, adjust = 'sidak')
emmip(modelCf6, Group ~ LC ~ CC, CIs = TRUE)

###########################Regression###Regression######################
modelR1 <- lmer(Regression ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z + Pleasure.z + 
                   (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE, 
                 control = lmerControl(optimizer = 'bobyqa'))
summary(modelR1)
#结果显示除discrimination均无效应均无效应，后续一一剔除，使用anova()检查剔除前后的模型，均不存在显著差异
modelR2 <- lmer(Regression ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z + Value.z +  
                   (1+ 1 |Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
summary(modelR2)

modelR3 <- lmer(Regression ~ Group * LC * CC + Logic.z + Familiarity.z + Discrimination.z +
                   (1+ 1|Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
summary(modelR3)


modelR4 <- lmer(Regression ~ Group * LC * CC + Familiarity.z + Discrimination.z + 
                   (1+ 1|Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
summary(modelR4)

modelR5 <- lmer(Regression ~ Group * LC * CC + Discrimination.z +  
                   (1+ 1|Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
summary(modelR5)
anova(modelR1,modelR5)

modelR6 <- lmer(Regression ~ Group * LC * CC + Discrimination.z + 
                   (1+ logicscore|Sub_ID)+(1+ 1|Material_ID), 
                 data = syllogism.2E,REML = FALSE,
                 control = lmerControl(optimizer = 'bobyqa'))
summary(modelR6)
ranova(modelR6)


eta_squared(modelR5)

lme.dscore(modelR5,data = syllogism.2E,type = "lme4")

#绘制三项图
syllogism.2E$Regression <- as.numeric(syllogism.2E$Regression)
syllogism.2E$CC <- as.factor(syllogism.2E$CC)
conclusion_labels <- c('1' = "conclusion_true", '2' = "conclusion_false") # 标签设置
emmip(modelR5, Group ~ LC | CC, CIs = TRUE) + 
  facet_grid(cols = vars(CC), labeller = labeller(CC = conclusion_labels)) +
  scale_color_discrete(label = c("Low", "High")) +
  scale_x_discrete(labels = c("true", "false")) +
  scale_y_continuous(name = "Regression") +
  theme_classic() +
  theme(strip.text = element_text(size = 12), strip.background = element_rect(colour = "white"), 
        legend.title = element_blank(), 
        axis.text.x = element_text(size = 9))

emmeans(modelR5, pairwise ~ Group | LC | CC, adjust = 'sidak')
emmeans(modelR5, pairwise ~ LC | CC| Group, adjust = 'sidak')
emmeans(modelR5, pairwise ~ CC | LC| Group, adjust = 'sidak')
emmip(modelR5, Group ~ LC ~ CC, CIs = TRUE)
