rm(list=ls()) #install.packages("vioplot")
library(plyr)
library(data.table)
#main_path = '/Users/akm82/Box Sync/SON/Data'
if(dir.exists('/Users/akikomizuno')){
    main_path = '/Users/akikomizuno//Box Sync/SON/Data'
}else{
    main_path = '~/mycloud/repos/SC_task'
}


##OutsideScan Data_______
# add participant's IS
outside <- read.csv(file.path(main_path, 'compiled_conframe_outsidescan.csv'))
outside$Participant_original <- outside$X
outside <- cbind(ID = NA, outside)
outside$ID <- substr(outside$Participant_original, 1,8)
colnames(outside)[11] <- "ConditionResponse"
write.csv(outside, file.path(main_path, "outside.csv"), row.names = FALSE)

#OutsideScan Data______________________________________________________________
# Load the subject list
subjlist <- unique(read.csv(file.path(main_path, 'outside.csv'),
                     stringsAsFactors=FALSE, colClasses = "character")[,2])
DT = data.table::fread(file.path(main_path, 'outside.csv'))
DT = DT[ConditionResponse != '',]
# Split the data by participant
DT_groups = split(DT, as.factor(DT$ID))
# Calculate overall response rate
overall_fun = function(dt){
  npos = nrow(dt[ConditionResponse=='7&',])
  nneg = nrow(dt[ConditionResponse=='1!',])
  return((npos - nneg)/(npos + nneg))
}

#' @description Select on facetype and apply overall_fun
face_fun = function(dt, facetype){
  df <- dt[Condition==facetype,]
  return(overall_fun(df))
}

#' @description Compute reaction time of
mean_RT_by_participant_and_condition = function(DT){
    Z = DT[, c("Condition", "Participant", "ConditionRt")]
    return(
        DT[, .(mrt = mean(ConditionRt)),
           by = c("Condition", "Participant")]
    )
}

X = mean_RT_by_participant_and_condition(DT)



RES = data.table(
  subject = names(DT_groups),
  Overall = unlist(lapply(DT_groups, overall_fun)),
  Fearful = unlist(lapply(DT_groups, face_fun, "negative")),
  Neutral = unlist(lapply(DT_groups, face_fun, "neutral")),
  Happy = unlist(lapply(DT_groups, face_fun, "positive"))
  )
# library(vioplot)
# vioplot(RES$Overall,RES$Neutral,RES$Happy, RES$Fearful,
#         names=c("All Faces", "Neutral", "Happy", "Fearful"),ylim=c(-1,1))

RES.m <- reshape2::melt(RES, id.vars="subject") # Transform wide to long form
group.colors <- c( "limegreen",  "royalblue", "lightblue",  "hotpink")
library(ggplot2)
ggplot(RES.m, aes(x=variable, y=value,fill=variable) ) + geom_violin() +
  scale_fill_manual(values=group.colors) +
  geom_boxplot(width=0.2) +
  theme(legend.position="none") +
  labs(title="Outside Scan", x="Type of Trial", y="Response") +
  ylim(-1,1)

## Analysis
RES.mNOoverall <- dplyr::filter(RES.m, !grepl('Overall', variable)) # Remove Overall
RES.aov <- aov(value ~ variable, data = RES.mNOoverall)
summary(RES.aov)
TukeyHSD(RES.aov)

### Accuracy
overall_fun2 = function(dt, method = 'all'){
  #  browser()
  npos = nrow(dt[ConditionResponse=='7&',])
  nneg = nrow(dt[ConditionResponse=='1!',])
  if(method == 'all'){
    return((npos - nneg)/(npos + nneg))
  }else if(method == 'PosAcc'){
    return(npos/nrow(dt))
  }else if(method == 'NegAcc'){
    return(nneg/nrow(dt))
  }else{
    stop("Invalid method")
  }
}

face_fun = function(dt, facetype, method = 'all'){
  df <- dt[Condition==facetype,]
  return(overall_fun2(df, method))
}

RES2 = data.table(
  subject = names(DT_groups),
  Happy = unlist(lapply(DT_groups, face_fun,'positive', method = 'PosAcc')),
  Fearful = unlist(lapply(DT_groups, face_fun, "negative",method = 'NegAcc'))
)
RES2.m <- reshape2::melt(RES2, id.vars="subject") # Transform wide to long form
group.colors <- c("hotpink",  "royalblue")

ggplot(RES2.m, aes(x=variable, y=value,fill=variable) ) + geom_violin() +
  scale_fill_manual(values=group.colors) +
  geom_boxplot(width=0.2) +
  theme(legend.position="none") +
  labs(title="Outside Scan", x="Type of Trial", y="Response") +
  ylim(0,1)

#t-test
t.test(RES2$Happy,RES2$Fearful)

#Inside_Scan Data________________________________________________________________________________
rm(list=ls())
library(plyr)
library(data.table)
library(ggplot2)

##------make inside-scan cvs file
main_path = '/Users/akm82/Box Sync/SON/Data'
inside <- read.csv(file.path(main_path, 'compiled_conframe.csv'))
inside <- cbind(ID = NA, inside)
inside$ID <- substr(inside$Participant, 1,8)
inside$med_type <- NA
for(i in 1:nrow(inside)) {
  if (grepl("Plac",inside$X[i])) {
    inside$med_type[i] <- "Placebo"
  } else if (grepl("Nal", inside$X[i])) {
    inside$med_type[i] <- "Naltrexone"
  }
}
write.csv(inside, file.path(main_path, "inside.csv"))
##---------------


#main_path = '/Users/akikomizuno/Box Sync/SON/Data'
subjlist <- unique(read.csv(file.path(main_path, 'inside.csv'),
                            stringsAsFactors=FALSE, colClasses = "character")[,2])
DT = data.table::fread(file.path(main_path, 'inside.csv'))
DT = DT[FaceResponseText != 'NaN',]
# DT_Nalt <- dplyr::filter(DT, grepl('Naltrexone', med_type))
# DT_Plac <- dplyr::filter(DT, grepl('Placebo', med_type))
##DT <- dplyr::filter(DT, grepl('Happy|Fear', Emotion)) # Limit Data for Happy|Fearful Faces
DT_groups = split(DT, as.factor(DT$Participant))
# DT_Plac_groups = split(DT_Plac, as.factor(DT_Plac$Participant))
# DT_Nalt_groups = split(DT_Nalt, as.factor(DT_Nalt$Participant))

# Calculate overall response rate
overall_fun = function(dt, method = 'all'){
  #  browser()
  npos = nrow(dt[FaceResponseText=="Positive",])
  nneg = nrow(dt[FaceResponseText=="Negative",])
  if(method == 'all'){
    return((npos - nneg)/(npos + nneg))
  }else if(method == 'PosAcc'){
    return(npos/nrow(dt))
  }else if(method == 'NegAcc'){
    return(nneg/nrow(dt))
  }else{
    stop("Invalid method")
  }
}

# face_fun = function(dt, cont_type, facetype, method = 'all'){
#   dt <- dt[(Context==cont_type)&(is.element(Emotion, facetype)),]
#   return(overall_fun(dt, method))
# }
face_fun = function(dt,  drug, cont_type, facetype, method = 'all'){
  dt <- dt[(med_type==drug)&(Context==cont_type)&(is.element(Emotion, facetype)),]
  return(overall_fun(dt, method))
}

RES = data.table(
  subject = names(DT_groups),
  #Overall = unlist(lapply(DT_groups, overall_fun)),
  Nalt_Overall = unlist(lapply(DT_groups, face_fun,"Naltrexone",c("Pleasant","Unpleasant"),c("Happy", "Neutral","Fearful"), method = 'all')),
  Nalt_Unplea_Overall = unlist(lapply(DT_groups, face_fun,"Naltrexone","Unpleasant",c("Happy", "Neutral","Fearful"), method = 'all')),
  Nalt_Plea_Overall = unlist(lapply(DT_groups, face_fun,"Naltrexone","Pleasant",c("Happy", "Neutral","Fearful"), method = 'all')),
  Nalt_Unplea_Fearful = unlist(lapply(DT_groups, face_fun,"Naltrexone","Unpleasant","Fearful",method = 'all')),#method = 'NegAcc'
  Nalt_Plea_Fearful = unlist(lapply(DT_groups, face_fun,"Naltrexone","Pleasant","Fearful",method = 'all')), #method = 'NegAcc'
  Nalt_Unplea_Neutral = unlist(lapply(DT_groups, face_fun,"Naltrexone","Unpleasant","Neutral",method = 'all')),
  Nalt_Plea_Neutral = unlist(lapply(DT_groups, face_fun,"Naltrexone","Pleasant","Neutral",method = 'all')),
  Nalt_Unplea_Happy = unlist(lapply(DT_groups, face_fun,"Naltrexone","Unpleasant","Happy",method = 'all')),#method = 'PosAcc'
  Nalt_Plea_Happy = unlist(lapply(DT_groups, face_fun,"Naltrexone","Pleasant","Happy",method = 'all')),#method = 'PosAcc'
  Plac_Overall = unlist(lapply(DT_groups, face_fun,"Placebo",c("Pleasant","Unpleasant"),c("Happy", "Neutral","Fearful"), method = 'all')),
  Plac_Unplea_Overall = unlist(lapply(DT_groups, face_fun,"Placebo","Unpleasant",c("Happy", "Neutral","Fearful"), method = 'all')),
  Plac_Plea_Overall = unlist(lapply(DT_groups, face_fun,"Placebo","Pleasant",c("Happy", "Neutral","Fearful"), method = 'all')),
  Plac_Unplea_Fearful = unlist(lapply(DT_groups, face_fun,"Placebo","Unpleasant","Fearful",method = 'all')),#method = 'NegAcc'
  Plac_Plea_Fearful = unlist(lapply(DT_groups, face_fun,"Placebo","Pleasant","Fearful",method = 'all')), #method = 'NegAcc'
  Plac_Unplea_Neutral = unlist(lapply(DT_groups, face_fun,"Placebo","Unpleasant","Neutral",method = 'all')),
  Plac_Plea_Neutral = unlist(lapply(DT_groups, face_fun,"Placebo","Pleasant","Neutral",method = 'all')),
  Plac_Unplea_Happy = unlist(lapply(DT_groups, face_fun,"Placebo","Unpleasant","Happy",method = 'all')),#method = 'PosAcc'
  Plac_Plea_Happy = unlist(lapply(DT_groups, face_fun,"Placebo","Pleasant","Happy",method = 'all'))#method = 'PosAcc'
)

vioplot(na.omit(RES$Nalt_Overall),na.omit(RES$Nalt_Unplea_Overall),na.omit(RES$Nalt_Plea_Overall),
        na.omit(RES$Nalt_Unplea_Fearful),na.omit(RES$Nalt_Plea_Fearful),
        na.omit(RES$Nalt_Unplea_Neutral),na.omit(RES$Nalt_Plea_Neutral),
        na.omit(RES$Nalt_Unplea_Happy), na.omit(RES$Nalt_Plea_Happy),ylim=c(-1,1))
        #         names=c("All Faces", "Neutral", "Happy", "Fearful"),ylim=c(-1,1))


vioplot(na.omit(RES$Plac_Overall),na.omit(RES$Plac_Unplea_Overall),na.omit(RES$Plac_Plea_Overall),
        na.omit(RES$Plac_Unplea_Fearful),na.omit(RES$Plac_Plea_Fearful),
        na.omit(RES$Plac_Unplea_Neutral),na.omit(RES$Plac_Plea_Neutral),
        na.omit(RES$Plac_Unplea_Happy), na.omit(RES$Plac_Plea_Happy),ylim=c(-1,1))

RES_Nalt <- RES[, 1:10]
RES_Nalt <- RES_Nalt[complete.cases(RES_Nalt),]
RES_Plac <- RES[, c(1, 11,12,13,14,15,16,17,18,19)]
RES_Plac <- RES_Plac[complete.cases(RES_Plac),]

RES_Plac.m <- reshape2::melt(RES_Plac, id.vars="subject")
RES_Nalt.m <- reshape2::melt(RES_Nalt, id.vars="subject")
RES_Plac.m$Exp <- "Plac"
RES_Nalt.m$Exp <- "Nalt"

RES_all <- rbind(RES_Plac.m, RES_Nalt.m)
## Graph______________
group.colors <- c("hotpink", "pink", "lightblue","royalblue")
ggplot(RES_Plac.m)

, aes(x=variable, y=value,fill=variable) ) + geom_violin() +
  scale_fill_manual(values=group.colors) + geom_boxplot(width=0.2) +
  facet_wrap(~ Exp)+theme(legend.position="none") +
  labs(x="Type of Trial", y="Accuracy") +ylim(0,1)





## Graph______________
group.colors <- c("hotpink", "pink", "lightblue","royalblue")
ggplot(RES_Nalt, aes(x=variable, y=value,fill=variable) ) + geom_violin() +
  scale_fill_manual(values=group.colors) + geom_boxplot(width=0.2) +
  facet_wrap(~ Exp)+theme(legend.position="none") +
  labs(x="Type of Trial", y="Accuracy") +ylim(0,1)

ggplot(RES_Nalt)









compu_fun = function(drug){
  main_path = '/Users/akm82/Box Sync/SON'
  subjlist <- function(sub_path){
    x = read.csv(file.path(main_path, sub_path),
                 stringsAsFactors=FALSE, colClasses = "character")[,1]
    return(x)
  }
  listDrug <- paste('InScanBeha/misc/SubjList_', drug, '.txt', sep="")
  subjlist<- subjlist(listDrug)
  # Load the data for all subjects
  subjDataList = lapply(subjlist, function(x){
    subj_dir = paste(main_path, '/InScanBeha/SON2_0', x, "SC_", drug,sep="")
    csv_files = dir(path = subj_dir, pattern='.csv')
    stopifnot(length(csv_files) == 2)
    dt_list = lapply(csv_files, function(x) fread(file.path(subj_dir, x)))
    return(data.table::rbindlist(dt_list))
  })
  DT = data.table::rbindlist(subjDataList)
  DT = DT[FaceResponseText != 'NaN',]
  #DT <- dplyr::filter(DT, grepl('Happy|Fear', Emotion)) # Limit Data for Happy|Fearful Faces
  DT_groups = split(DT, as.factor(DT$Participant))
  # Calculate overall response rate
  overall_fun = function(dt, method = 'all'){
    #  browser()
    npos = nrow(dt[FaceResponseText=="Positive",])
    nneg = nrow(dt[FaceResponseText=="Negative",])
    if(method == 'all'){
      return((npos - nneg)/(npos + nneg))
    }else if(method == 'PosAcc'){
      return(npos/nrow(dt))
    }else if(method == 'NegAcc'){
      return(nneg/nrow(dt))
    }else{
      stop("Invalid method")
    }
  }

  face_fun = function(dt, cont_type, facetype, method = 'all'){
    dt <- dt[(Context==cont_type)&(is.element(Emotion, facetype)),]
    return(overall_fun(dt, method))
  }

  RES = data.table(
    subject = names(DT_groups),
    Plea_Happy = unlist(lapply(DT_groups, face_fun,"Pleasant","Happy",method = 'PosAcc')),
    Unplea_Happy = unlist(lapply(DT_groups, face_fun,"Unpleasant","Happy",method = 'PosAcc')),
    Plea_Fearful = unlist(lapply(DT_groups, face_fun,"Pleasant","Fearful",method = 'NegAcc')),
    Unplea_Fearful = unlist(lapply(DT_groups, face_fun,"Unpleasant","Fearful",method = 'NegAcc'))
  )
  return(RES)
}

RES_Plac <- compu_fun("Plac")
RES_Nalt <- compu_fun("Nalt")

RES_Plac.m <- reshape2::melt(RES_Plac, id.vars="subject")
RES_Nalt.m <- reshape2::melt(RES_Nalt, id.vars="subject")
RES_Plac.m$Exp <- "Plac"
RES_Nalt.m$Exp <- "Nalt"

RES_all <- rbind(RES_Plac.m, RES_Nalt.m)
## Graph______________
group.colors <- c("hotpink", "pink", "lightblue","royalblue")
ggplot(RES_all, aes(x=variable, y=value,fill=variable) ) + geom_violin() +
  scale_fill_manual(values=group.colors) + geom_boxplot(width=0.2) +
  facet_wrap(~ Exp)+theme(legend.position="none") +
  labs(x="Type of Trial", y="Accuracy") +ylim(0,1)







#Inside_Scan Data________________________________________________________________________________
rm(list=ls())
library( plyr)
library(data.table)
main_path = '/Users/akm82/Box Sync/SON'

# Load the subject list
subjlist <- function(sub_path){
  x = read.csv(file.path(main_path, sub_path),
           stringsAsFactors=FALSE, colClasses = "character")[,1]
  return(x)
}
drug <- as.character("Plac")
listDrug <- paste('InScanBeha/misc/SubjList_', drug, '.txt', sep="")

subjlist<- subjlist(listDrug)

# Load the data for all subjects
subjDataList = lapply(subjlist, function(x){
  subj_dir = paste(main_path, '/InScanBeha/SON2_0', x, "SC_", drug,sep="")
  csv_files = dir(path = subj_dir, pattern='.csv')
  stopifnot(length(csv_files) == 2)
  dt_list = lapply(csv_files, function(x) fread(file.path(subj_dir, x)))
  return(data.table::rbindlist(dt_list))
})
DT = data.table::rbindlist(subjDataList)
DT = DT[FaceResponseText != 'NaN',]
          ### Check missing data
          DT_missing <- DT[FaceResponseText == 'NaN'] #-- 38
          DT_missing_groups = split(DT_missing, as.factor(DT_missing$Participant))
          unlist(lapply(DT_missing_groups, function(x) nrow(x)))
          #unique(DT_missing$Run) # --3 4 5 2

          # Check inconsistency
          nrow(DT[DT$FaceResponseNum==2 & DT$FaceResponseText=="Negative"])#correct -- 450
          nrow(DT[DT$FaceResponseNum==2 & DT$FaceResponseText=="Positive"])         -- 342
          nrow(DT[DT$FaceResponseNum==7 & DT$FaceResponseText=="Negative"])         -- 327
          nrow(DT[DT$FaceResponseNum==7 & DT$FaceResponseText=="Positive"])#correct -- 215

DT_groups = split(DT, as.factor(DT$Participant))
# Calculate overall response rate
overall_fun = function(dt){
  npos = nrow(dt[FaceResponseText=='Positive',])
  nneg = nrow(dt[FaceResponseText=='Negative',])
  return((npos - nneg)/(npos + nneg))
}

face_fun = function(dt, cont_type, facetype){
  df <- dt[(Context==cont_type)&(is.element(Emotion, facetype)),]
  return(overall_fun(df))
}

RES_Plac = data.table(
  subject = names(DT_groups),
  Overall = unlist(lapply(DT_groups, overall_fun)),
  Plea_Overall = unlist(lapply(DT_groups, face_fun,"Pleasant",c("Happy", "Neutral","Fearful"))),
  Unplea_Overall = unlist(lapply(DT_groups, face_fun,"Unpleasant",c("Happy", "Neutral","Fearful"))),
  Plea_Neutral = unlist(lapply(DT_groups, face_fun,"Pleasant", "Neutral")),
  Unplea_Neutral = unlist(lapply(DT_groups, face_fun,"Unpleasant","Neutral")),
  Plea_Happy = unlist(lapply(DT_groups, face_fun,"Pleasant","Happy")),
  Unplea_Happy = unlist(lapply(DT_groups, face_fun,"Unpleasant","Happy")),
  Plea_Fear = unlist(lapply(DT_groups, face_fun,"Pleasant","Fearful")),
  Unplea_Fear = unlist(lapply(DT_groups, face_fun,"Unpleasant","Fearful"))
  # Happy = unlist(lapply(DT_groups, face_fun, "positive")),
  # Fearful = unlist(lapply(DT_groups, face_fun, "negative"))
)
RES_Plac.m <- reshape2::melt(RES_Plac, id.vars="subject")
##_____________
#rename -- run Nalt first, then Plac
RES_Nalt.m <- RES_Plac.m
group.colors <- c(Overall = "limegreen", Plea_Overall = "pink", Unplea_Overall ="pink",
                  Plea_Neutral = "lightblue", Unplea_Neutral = "lightblue", Plea_Happy = "hotpink",
                  Unplea_Happy = "hotpink", Plea_Fear ="royalblue", Unplea_Fear = "royalblue")
library(ggplot2)
ggplot(RES_Plac.m, aes(x=variable, y=value,fill=variable) ) + geom_violin() +
  scale_fill_manual(values=group.colors) + geom_boxplot(width=0.2) +
  theme(legend.position="none") +
  labs(title=drug, x="Type of Trial", y="Response") +
  ylim(-1,1)
##_________________________
#analysis
RES_Plac.m$Exp <- "Plac"
RES_Nalt.m$Exp <- "Nalt"

RES_all <- rbind(RES_Plac.m, RES_Nalt.m)
RES_all <- dplyr::filter(RES_all, !grepl('Overall', variable))
RES_all$Context[grep("Unplea", RES_all$variable)] <- "Unplea"
RES_all$Context[grep("Plea", RES_all$variable)] <- "Plea"
RES_all$Face[grep("Fear", RES_all$variable)] <- "Fear"
RES_all$Face[grep("Neutral", RES_all$variable)] <- "Neutral"
RES_all$Face[grep("Happy", RES_all$variable)] <- "Happy"
#analysis for all faces
ver1 <- RES_all$Face
ver2 <- RES_all$Exp
fit <- lm(value ~ ver1+ver2+ver1*ver2,data=RES_all)
anova(fit)
summary(fit)
#analysis for only Happy & Fearful faces
data_HappyFear <- dplyr::filter(RES_all, grepl('Happy|Fear', variable))
data_HappyFear$Cong[grep("Plea_Happy|Unplea_Fear", data_HappyFear$variable)] <- "Congru"
data_HappyFear$Cong[grep("Plea_Fear|Unplea_Happy", data_HappyFear$variable)] <- "Incong"
data_HappyFear$ConFaExp <- paste(data_HappyFear$variable,data_HappyFear$Exp,sep = "")
data_HappyFear$FaceCong <- paste(data_HappyFear$Face,data_HappyFear$Cong,sep = "")
  #Context x Face x Drug
group.colors2 <- c(Plea_Happy = "hotpink", Unplea_Happy = "hotpink",
                   Plea_Fear ="royalblue", Unplea_Fear = "royalblue")
ggplot(data_HappyFear, aes(x=variable, y=value, fill=variable) ) + geom_violin() +
  facet_wrap(~ Exp) + scale_fill_manual(values=group.colors2) + geom_boxplot(width=0.2) +
  theme(legend.position="none") +
  labs(x="Type of Trial", y="Response") +
  ylim(-1,1)
  #Congruency x Face x Drug
ggplot(data_HappyFear, aes(x=FaceCong, y=value, fill=variable) ) + geom_violin() +
  facet_wrap(~ Exp) + scale_fill_manual(values=group.colors2) + geom_boxplot(width=0.2) +
  theme(legend.position="none") +
  labs(x="Type of Trial", y="Response") +
  ylim(-1,1)

ver1 <- data_HappyFear$Contxt
ver2 <- data_HappyFear$Face
#ver3 <- data_HappyFear$Exp
#fit <- lm(value ~ ver1+ver2+ver3+ver1*ver2*ver3,data=data_HappyFear)
anova(fit)
fit2 <- aov(value ~ ver1+ver2+ver1*ver2,data=data_HappyFear)
TukeyHSD(fit2)

fit <- lm(value ~ ver1+ver2+ver1*ver2,data=data_HappyFear)
anova(fit)
summary(fit)

##----- Only Happy Faces
data_Happy <- dplyr::filter(data_HappyFear, grepl('Happy', variable))
fit <- lm(value ~ Context+Exp+Context*Exp,data=data_Happy)
fit <- lm(value ~ Context,data=data_Happy)
anova(fit)
summary(fit)
group.colors3 <- c(Plea_HappyPlac = "hotpink", Unplea_HappyPlac = "hotpink",
                   Plea_HappyNalt = "pink", Unplea_HappyNalt = "pink")
ggplot(data_Happy, aes(x=variable, y=value, fill=ConFaExp) ) + geom_violin() +
  facet_wrap(~ Exp) + scale_fill_manual(values=group.colors3) + geom_boxplot(width=0.2) +
  theme(legend.position="none") +
  labs(x="Type of Trial", y="Response") +
  ylim(-1,1)

##----- Only Fearful Faces
data_Fear <- dplyr::filter(data_HappyFear, grepl('Fear', variable))
fit <- lm(value ~ Context+Exp+Context*Exp,data=data_Fear)
fit <- lm(value ~ Exp,data=data_Fear)
anova(fit)
summary(fit)
group.colors4 <- c(Plea_FearPlac = "royalblue", Unplea_FearPlac = "royalblue",
                   Plea_FearNalt = "lightblue", Unplea_FearNalt = "lightblue")
ggplot(data_Fear, aes(x=variable, y=value, fill=ConFaExp) ) + geom_violin() +
  facet_wrap(~ Exp) + scale_fill_manual(values=group.colors4) + geom_boxplot(width=0.2) +
  theme(legend.position="none") +
  labs(x="Type of Trial", y="Response") +
  ylim(-1,1)

#________________
data_HappyFear$Accu[grep("Fear", data_HappyFear$Face)] <- (-1)*(data_HappyFear$value)
data_HappyFear









#ANOVA
placeboInput <- setNames(data.frame(matrix(ncol = 4, nrow = 66)),
                          c("ID","Context", "Face", "Score"))
sub <- as.numeric(subjlist_place[,1])
placeboInput$ID <- rep(sub, each=6)
placeboInput$Context <- rep(c("Pleasant","Unpleasant"), each=3, length=66)
placeboInput$Face <- rep(c("Neutral","Happy","Fear"), length=66)
placeboAllTrim <- placeboAll[,4:9]
placeboAllOrder <- placeboAllTrim[,c(1,3,6,2,4,5)]
#install.packages("dplyr")
library("dplyr")
placeboAllReorder <- reshape(placeboAllOrder, direction = "long",
        varying = list(names(placeboAllOrder)[1:6]), idvar="ID")
placeboAllReorder <- arrange(placeboAllReorder,placeboAllReorder$ID)
placeboInput$Score <- placeboAllReorder$Plea_NeutFace
head(placeboInput)
placeboInput$Context <- as.factor(placeboInput$Context)
placeboInput$Face <- as.factor(placeboInput$Face)
placeboInput$Prim <- 0

for(h in 1:nrow(placeboInput)){
  if ((placeboInput$Context[h] =="Pleasant") && (placeboInput$Face[h] =="Happy")) {
    placeboInput$Prim[h] <- "Congru"
  } else if ((placeboInput$Context[h] =="Pleasant") && (placeboInput$Face[h] =="Fear")) {
    placeboInput$Prim[h] <- "Incongru"
  }  else if ((placeboInput$Context[h] =="Unpleasant") && (placeboInput$Face[h] =="Happy")) {
    placeboInput$Prim[h] <- "Incongru"
  }  else if ((placeboInput$Context[h] =="Unpleasant") && (placeboInput$Face[h] =="Fear")) {
    placeboInput$Prim[h] <- "Congru"
  }  else {
    placeboInput$Prim[h] <- NA
  }
}

mod <- aov(placeboInput$Score~placeboInput$Context)
summary(mod)
TukeyHSD(mod)


placeboInput_NoNeut <- filter(placeboInput, Face == "Happy" | Face  == "Fear")
mod_NoNeut <- lm(Score ~ Context + Face + Context*Face,data=placeboInput_NoNeut)
anova(mod_NoNeut)
summary(mod_NoNeut)

mod_NoNeut <- lm(Score ~ Prim + Face + Prim*Face,data=placeboInput_NoNeut)
anova(mod_NoNeut)
summary(mod_NoNeut)
vioplot(placeboAll[,6],placeboAll[,7],placeboAll[,8],placeboAll[,9],
        names=c("Congru_PleaHappy", "Incongru_UnpleaHappy",
                "Congru_UnpleaFear", "Incongru_PleaFear"),ylim=c(-1,1))

mod_NoNeut2 <- aov(Score ~ Face + Prim + Face:Prim ,data=placeboInput_NoNeut)
summary(mod_NoNeut2)
TukeyHSD(mod_NoNeut2)

