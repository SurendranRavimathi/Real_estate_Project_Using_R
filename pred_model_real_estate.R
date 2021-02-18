
## target ---> Price

getwd()
setwd("C:/project/real_estate")
rs_train=read.csv("housing_train .csv",stringsAsFactors = F)
rs_test=read.csv("housing_test.csv",stringsAsFactors = F)

rs_test$Price=NA

rs_train$data="train"
rs_test$data="test"

rs_all=rbind(rs_train,rs_test)

library(dplyr)

glimpse(rs_all)

#--- analaysing the data-------------

# 1.suburb

sort(table(rs_all$Suburb),decreasing = T) # keep and create dumies

length(unique(rs_all$Suburb))

sum(is.na(rs_all$Suburb))
#2.Address

length(unique(rs_all$Address)) # drop the address col.

#3.Rooms

table(rs_all$Rooms) #   keep


#4.Type

table(rs_all$Type) # keep create dumies

#5.Price ----> # keep # since its is target value

#6.Method

table(rs_all$Method) # drop

#7.SellerG

table(rs_all$SellerG) #drop #since this not required in modelling
length(unique(rs_all$SellerG))

#8.Distance
sort(table(rs_all$Distance),decreasing = T) #  keep

#9.Postcode # create dummies
table(rs_all$Postcode)
length(unique(rs_all$Postcode))
sum(is.na(rs_all$Postcode))

sort(table(rs_train$Price),decreasing = T) 

#10.Bedroom2
table(rs_all$Bedroom2) # keep 
sum(is.na(rs_all$Bedroom2)) 
round(mean(rs_all$Bedroom2,na.rm = T)) # replace NA with mean i.e, 3 bedroom2

#11.Bathroom
table(rs_all$Bathroom) # keep
sum(is.na(rs_all$Bathroom))
round(mean(rs_all$Bathroom,na.rm=T)) #replace NA with mean i.e, 2 Bathroom

glimpse(rs_all)

#12.Car

table(rs_all$Car)#keep
sum(is.na(rs_all$Cat)) # no NA 's

#13.Landsize #drop

table(rs_all$Price,rs_all$Landsize) #drop
sum(is.na(rs_all$Landsize))

sort(table(rs_all$Landsize),decreasing = T)

rs_all %>% filter(Landsize== 454 ) %>% select(Price) 

round(mean(rs_all$Landsize,na.rm = T)) #replace NA 's with 454 Landsize

sum(is.na(rs_all$Price))

#14.BuildingArea #drop

table(rs_all$BuildingArea) 
sum(is.na(rs_all$BuildingArea))
round(mean(rs_all$BuildingArea,na.rm = T))# replace NA's with 145 BuildingArea

#15.YearBuilt #drop
table(rs_all$YearBuilt)#keep
sum(is.na(rs_all$YearBuilt))
round(mean(rs_all$YearBuilt,na.rm=T)) # replace NA's with 1961 YearBuilt

#16.CouncilArea

table(rs_all$CouncilArea)#keep #create dummies # keep the empty space an coln
sum(is.na(rs_all$CouncilArea))
#--------creating dummies---------

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}




# sort(table(rs_all$Suburb),decreasing = T)
# 
# rs_all=CreateDummies(rs_all,"Suburb",100)

table(rs_all$Type)

rs_all=CreateDummies(rs_all,"Type")

sort(table(rs_all$Postcode),decreasing = T)
rs_all=CreateDummies(rs_all,"Postcode",100)

# sort(table(rs_all$CouncilArea),decreasing=T)
# rs_all=CreateDummies(rs_all,"CouncilArea",100)
# glimpse(rs_all)
# # for(col in cat){
# #   rs_all=CreateDummies(rs_all,col,100)
# # }

# sort(table(rs_all$Landsize),decreasing=T)
# rs_all=CreateDummies(rs_all,"Landsize",10)

table(rs_all$BuildingArea)
sum(is.na(rs_all$BuildingArea))
glimpse(rs_all)

#----------------dropping col--------

rs_all=rs_all %>% select(-Suburb,-Address,-SellerG,-YearBuilt,-Method,-CouncilArea)

#------------replacing NA's -------------


# Bedroom2,Bathroom,Landsize,BuildingArea,YearBuilt

lapply(rs_all, function(x) sum(is.na(x)))

for(col in names(rs_all)){
  
  if(sum(is.na(rs_all[,col]))>0 & !(col %in% c("data","Price"))){
    
    rs_all[is.na(rs_all[,col]),col]=0
  }
  
    
  
}

glimpse(rs_all)



lapply(rs_all,function(x) sum(is.na(x)))

rs_all[is.na(rs_all[,"Bedroom2"]),"Bedroom2"]=0

sum(is.na(rs_all$Bedroom2))
#-----linear model---------


#separateing into train and test
library(dplyr)
library(tidyr)
rs_train=rs_all %>% filter(data=="train") %>% select(-data)
rs_test=rs_all %>% filter(data=="test") %>% select(-data)


set.seed(2)
s=sample(1:nrow(rs_train),0.7*nrow(rs_train))
rs_train1=rs_train[s,]
rs_train2=rs_train[-s,]

fit=lm(Price~.,data=rs_train1)

library(car)

summary(fit)

#we will take vif cutoff 5

sort(vif(fit),decreasing = T)[1:3]

# p-value take the cutoff .05


summary(fit)
fit=step(fit)## AIC score 

summary(fit)

formula(fit)

fit=lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + Car + Landsize + 
         Type_u + Type_h + Postcode_3068 + Postcode_3039 + Postcode_3071 + 
         Postcode_3147 + Postcode_3013 + Postcode_3103 + Postcode_3124 + 
         Postcode_3145 + Postcode_3127 + Postcode_3081 + Postcode_3044 + 
         Postcode_3187 + Postcode_3122 + Postcode_3031 + Postcode_3104 + 
         Postcode_3015 + Postcode_3042 + Postcode_3011 + Postcode_3188 + 
         Postcode_3101 + Postcode_3186 + Postcode_3146 + Postcode_3056 + 
         Postcode_3012 + Postcode_3072 + Postcode_3204 + Postcode_3058 + 
         Postcode_3163 + Postcode_3040 + Postcode_3032 + Postcode_3121 + 
         Postcode_3165 + Postcode_3046 + Postcode_3020 + Postcode_3073
       ,
       data=rs_train1)

summary(fit)


###

val.pred=predict(fit,newdata=rs_train2)

errors=rs_train2$Price-val.pred

errors**2 %>% mean() %>% sqrt() # above 0.51
####


### model for predcition on the entire data

fit.final=fit=lm(Price~ .-Postcode_3073-Postcode_3165-Postcode_3121-CouncilArea_ ,
                 data=rs_train)
summary(fit.final)
sort(vif(fit.final),decreasing = T)[1:3]

fit.final=step(fit.final)

summary(fit.final)

formula(fit.final)
fit.final=fit=lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + Car + Suburb_Doncaster + 
                   Suburb_MooneePonds + Suburb_Thornbury + Suburb_Hampton + 
                   Suburb_Balwyn + Suburb_MalvernEast + Suburb_Camberwell + 
                    Suburb_Bentleigh + 
                   Suburb_PascoeVale + Suburb_BrightonEast + Suburb_Hawthorn + 
                   Suburb_Coburg + Suburb_Kew + Suburb_Brighton + Suburb_GlenIris + 
                   Suburb_SouthYarra + Suburb_Preston + Suburb_Richmond + 
                   Suburb_Reservoir + Type_u + Type_h + Postcode_3040 + Postcode_3032 + 
                   Postcode_3046 + Postcode_3020 + CouncilArea_Whitehorse + 
                   CouncilArea_HobsonsBay + CouncilArea_Bayside + CouncilArea_Banyule + 
                   CouncilArea_PortPhillip + CouncilArea_Maribyrnong + CouncilArea_Stonnington + 
                   CouncilArea_GlenEira + CouncilArea_Darebin + CouncilArea_MooneeValley + 
                   CouncilArea_Moreland + CouncilArea_Boroondara + Landsize_122 + 
                    Landsize_558 + 
                    Landsize_141 + 
                   Landsize_213 + Landsize_274 +  Landsize_687 + 
                    Landsize_190 + Landsize_220 + Landsize_640 + 
                    Landsize_657 +  
                    Landsize_697 + Landsize_0,
                 data=rs_train)

summary(fit.final)

test.pred=predict(fit.final,newdata=rs_test)

write.csv(test.pred,"Surendran_R_P1_part2.csv ",row.names = F)

### 

plot(fit.final,1) # residual vs fitted values => non-linearity in the data exists or not

plot(fit.final,2) # errors are normal or not

plot(fit.final,3) # variance is constant or not

plot(fit.final,4) # outliers in the data if cook's distance >1

getwd()

#-----submission format----

r=read.csv("Surendran_R_P1_part2.csv")
glimpse(r)

colnames(r)="Price"

write.csv(r,"Surendran_R_P1_part2.csv ",row.names = F)



# linear result is not so good
#---------------ntree=20-------------

library(randomForest)
rs.fit.trial=randomForest(Price~.,data=rs_train1,ntree=20)

val.pred=predict(rs.fit.trial,newdata=rs_train2)

errors=rs_train2$Price-val.pred

errors**2 %>% mean() %>% sqrt()

212467/ 362221.9
####

rs.fit.trial=randomForest(Price~.,data=rs_train,ntree=20)

test.pred=predict(rs.fit.trial,newdata = rs_test)
write.csv(test.pred,"Surendran_R_P1_part2.csv",row.names = F)

getwd()
r=read.csv("Surendran_R_P1_part2.csv")
dplyr::glimpse(r)

colnames(r)="Price"
r=as.integer(r$Price)
write.csv(r,"Surendran_R_P1_part2.csv ",row.names = F)


glimpse(r)

getwd()

library(dplyr)
glimpse(rs_train)


# -----------random forest---------

library(randomForest)
library(cvTools)
params=list(mtry=c(5,10),ntree=c(100,500),
            maxnodes=c(15,20),nodesize=(c(2,5)))
expand.grid(params)

## paramter values that we want to try out

param=list(mtry=c(5,10,15,20,25),
           ntree=c(50,70,100,150,200,500,700),
           maxnodes=c(5,10,15,20,30,50),
           nodesize=c(1,2,5,10))


## Function for selecting random subset of params

subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

## 

num_trials=10
my_params=subset_paras(param,num_trials)
# Note: A good value for num_trials is around 10-20% of total possible 
# combination. It doesnt have to be always 50

## cvtuning for regression
## this code might take too long to run
## no need to execute completely in class
myerror=9999999

for(i in 1:num_trials){
  # print(paste0('starting iteration:',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,Price~.,
             data =rs_train,
             tuning =params,
             folds = cvFolds(nrow(rs_train), K=10, type = "random"),
             seed =2
  )
  score.this=k$cv[,2]
  
  if(score.this<myerror){
    print(params)
    # uncomment the line above to keep track of progress
    myerror=score.this
    print(myerror)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  print('DONE')
  # uncomment the line above to keep track of progress
}

## from another run following values were obtained

# myerror=1.870957
# best_params=data.frame(mtry=20,
#                       ntree=200,
#                       maxnodes=50,
#                       nodesize=10)

## Final model with obtained best parameters

ld.rf.final=randomForest(Interest.Rate~.-ID,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=ld_train)

test.pred=predict(ld.rf.model,newdata = ld_test)
write.csv(test.pred,"mysubmission.csv",row.names = F)

ld.rf.final



r=read.csv("Surendran_R_P1_part2.csv")
dplyr::glimpse(r)

colnames(r)="Price"
r=as.integer(r$Price)
write.csv(r,"Surendran_R_P1_part2.csv ",row.names = F)

library(dplyr)
glimpse(rs_train)

getwd()


441212.3**2 %>% mean() %>% sqrt()



library(randomForest)

trial=randomForest(Price~ .-Postcode_3073-Postcode_3165-Postcode_3121-CouncilArea_ ,
                                    data=rs_train1,n.tree=20)
