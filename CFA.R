install.packages("haven")
install.packages("foreign")
install.packages("dplyr")
install.packages("magrittr")
install.packages("data.table")
install.packages("reshape2")
install.packages("stringi")
install.packages("stringr")
install.packages("xlsx")
install.packages("psych")
install.packages("nFactors")
install.packages("lavaan")
install.packages("semPlot")
install.packages("tibble")
install.packages("psych")
library(haven)
library(foreign)
library(dplyr)
library(magrittr)
library(data.table)
library(reshape2)
library(stringi)
library(stringr)
#library(xlsx)
library(psych)
library(nFactors)
library(lavaan)
library(semPlot)
library(tibble)
library(psych)



###############--------------- CFA1 Total --------------################



m1a <- 'Factor1A =~ This.brand.is.enjoyable.to.use+
            Makes.my.life.easier+
            Helps.me.achieve.the.look.I.want+
            This.brand.helps.me.feel.more.confident+
            Keeps.my.hair.cleaner.for.longer+
            Leaves.a.pleasant.smell.on.my.hair+
            Cleans.my.hair.without.stripping

        Factor1B=~Moisturizes.my.hair.better.than.other.brands+
             Has.the.best.conditioners.I.can.get+
             Detangles.my.hair.better.than.other.brands

        Factor2=~Provides.superior.anti.dandruff.performance+
            Gets.rid.of.dry.itchy.scalp+
            Provides.superior.scalp.care


        Factor3=~Has.the.best.natural.ingredients+
            Free.from.harmful.chemicals+
            Is.full.of.good.ingredients+
            Nourishes.my.hair.naturally

        Factor4A=~Strengthens.my.hair.bonds+
            Improves.my.hair.health+
            Repairs.hair.damage+
            Provides.vitamins.to.your.hair


        Factor4B=~Prevents.hair.loss+
            Helps.my.hair.grow+
            Prevents.hair.color.fade


        Factor5=~Is.good.for.textured.or.multicultural.hair+
             Is.good.for.people.with.hair.like.mine

       '


onefac3items_a <- cfa(m1a, data=dataset)

summary(onefac3items_a,standardized=T)

factors<-as.data.frame(predict(onefac3items_a))



write.csv(factors,'Factors.csv',row.names = F)

param_happy <- parameterEstimates(onefac3items_a)

fit_happy <- fitMeasures(onefac3items_a)



write.csv(param_happy,'CFA.csv',row.names = F)



#############------------- Construct Reliability final ---------------###############



modindices(onefac3items_a,sort. = T)



#semPaths(onefac3items_a,'std')



ins<-inspect(onefac3items_a,what = 'std')

out<-as.data.frame(ins$lambda)

out<-rownames_to_column(out,'Factors')

out1<-melt(out,'Factors')



out1<-out1[out1$value>0,]

names(out1)[names(out1)=='value']<-'Loadings'

out1$Squared_Loadings<-out1$Loadings^2



m<-out1 %>% group_by(variable) %>% summarise('Mean'=mean(Squared_Loadings))



out1<-merge(out1,m,by.x = 'variable',by.y = 'variable')



theta<-as.data.frame(ins$theta)

theta<-rownames_to_column(theta,'Factors')

theta1<-melt(theta,'Factors')



theta1<-theta1[theta1$value!=0,]



Out2<-merge(out1,theta1[,c("Factors","value")],by.x ='Factors',by.y = 'Factors',all.x = T)

names(Out2)[names(Out2)=='value']<-'Errors'

Out2$Errors[is.na(Out2$Errors)]<-0



l<-Out2 %>% group_by(variable)%>% summarise('sum of Squared loadings'=sum(Loadings)^2,'sum of Squared errors'=sum(Errors)^2)



Out2<-merge(Out2,l,by.x = 'variable',by.y = 'variable')

Out2$CR<-Out2$`sum of Squared loadings`/(Out2$`sum of Squared loadings` + Out2$`sum of Squared errors`)



fitmeasures(onefac3items_a,c('gfi','agfi','nfi','cfi','rmsea','srmr','tli'))



Out2$Key<-str_split_fixed(Out2$Factors,'_',4)[,3]



#P10<-read.csv("P10_Mapping.csv",stringsAsFactors = F)



#Out2<-merge(Out2,P10,by.x = 'Key',by.y = 'Sl.No')



#Out2<-Out2[,c(2,3,11,4:10)]



Out2<-Out2[order(Out2$variable),]

write.csv(Out2,'Construct_Reliability_Output.csv',row.names = F)
