
#a. semesterdata

#bind data 
sumdata = rbind(semester_data_1, semester_data_2)

#一行目を列名にする
colnames(sumdata) <- sumdata[1,]
sumdata <- sumdata[-1,]

#Yを消す
sumdata =sumdata[,-6]

#semester制が導入された年の列?
#とりあえず変化したところのdummy変数
sumdata <- sumdata %>%
  group_by(unitid)  %>%
  mutate(
    dummy_switch= ifelse(semester != lag(semester), 1, 0) , 
    dummy_switch = ifelse(is.na(dummy_switch), 0, dummy_switch)
  ) %>%
  ungroup()


#b. Gradrate

#データ読み込み

X1991<- read_xlsx("~/Downloads/warmup training package/01_data/raw/outcome/1991.xlsx")
X1992<- read_xlsx("~/Downloads/warmup training package/01_data/raw/outcome/1992.xlsx")
X1993<- read_xlsx("~/Downloads/warmup training package/01_data/raw/outcome/1993.xlsx")
X1995<- read_xlsx("~/Downloads/warmup training package/01_data/raw/outcome/1995.xlsx")
X1996<- read_xlsx("~/Downloads/warmup training package/01_data/raw/outcome/1996.xlsx")
X1997<- read_xlsx("~/Downloads/warmup training package/01_data/raw/outcome/1997.xlsx")
X1998<- read_xlsx("~/Downloads/warmup training package/01_data/raw/outcome/1998.xlsx")
X1999<- read_xlsx("~/Downloads/warmup training package/01_data/raw/outcome/1999.xlsx")
X2000<- read_xlsx("~/Downloads/warmup training package/01_data/raw/outcome/2000.xlsx")
X2001<- read_xlsx("~/Downloads/warmup training package/01_data/raw/outcome/2001.xlsx")
X2002<- read_xlsx("~/Downloads/warmup training package/01_data/raw/outcome/2002.xlsx")
X2003<- read_xlsx("~/Downloads/warmup training package/01_data/raw/outcome/2003.xlsx")
X2004<- read_xlsx("~/Downloads/warmup training package/01_data/raw/outcome/2004.xlsx")
X2005<- read_xlsx("~/Downloads/warmup training package/01_data/raw/outcome/2005.xlsx")
X2006<- read_xlsx("~/Downloads/warmup training package/01_data/raw/outcome/2006.xlsx")
X2007<- read_xlsx("~/Downloads/warmup training package/01_data/raw/outcome/2007.xlsx")
X2008<- read_xlsx("~/Downloads/warmup training package/01_data/raw/outcome/2008.xlsx")
X2009<- read_xlsx("~/Downloads/warmup training package/01_data/raw/outcome/2009.xlsx")
X2010<- read_xlsx("~/Downloads/warmup training package/01_data/raw/outcome/2010.xlsx")

#1992-2010
gradrate = rbind(X1991, X1992, X1993, X1995, X1996,X1997, X1998,  X1999,  X2000,  X2001,  X2002,  X2003,  X2004,  X2005,  X2006,  X2007,  X2008,  X2009,  X2010)
#0.01~1"
gradrate[,9]=gradrate[,9]*.01
#total
gradrate[,3] = as.numeric(unlist(gradrate[,3]))
gradrate$total_gradrate_4yr = gradrate[,6]/gradrate[,3]
gradrate[,7] = as.numeric(unlist(gradrate[,7]))
gradrate$men_gradrate_4yr =gradrate[,7]/gradrate[,5]


#round values
gradrate[,10]= as.numeric(unlist(gradrate[,10]))
gradrate[,11]= as.numeric(unlist(gradrate[,11]))
gradrate[, 9:11] = round(gradrate[,9:11], digits = 2)


#c. covariates

#unitid
colnames(covariates)[1] <- "unitid"

covariates$unitid<-gsub("aaaa","",as.character(covariates$unitid))

library(tidyr)
covariates = covariates %>%
  pivot_wider(names_from = "category",
              values_from = "value")

#keep the rows in 1991-2010
covariates = covariates [covariates$year %in% c("1991", "1992", "1993","1995", "1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010"),]


#unitidの特定
gradrate_unitid = gradrate$unitid
covariates = covariates [covariates$unitid %in% c(gradrate_unitid) ,]

#gradrateの並び替え。yearが変だから
library(dplyr)
gradrate=gradrate %>% arrange(unitid, year)

#統合

sumdata[5]=as.numeric(unlist(sumdata[,5]))
sumdata[,1]=as.numeric(unlist(sumdata[,1]))

left = left_join(x = sumdata, y = gradrate, by = c('unitid', 'year'))

#型を揃える。列だけではなくて一気にやる方法を知りたい
covariates[,1] = as.numeric(unlist(covariates[,1]))
covariates[,2] = as.numeric(unlist(covariates[,2]))
covariates[,3] = as.numeric(unlist(covariates[,3]))
covariates[,4] = as.numeric(unlist(covariates[,4]))
covariates[,5] = as.numeric(unlist(covariates[,5]))
covariates[,6] = as.numeric(unlist(covariates[,6]))


# 完成！(a-5. "semester制が導入された年の列を作成しなさい。"はできていない)

masterdata= left_join(x = left, y = covariates, by = c('unitid', 'year'))


masterdata[,3]= as.numeric(unlist(masterdata[,3]))
masterdata[,4]= as.numeric(unlist(masterdata[,4]))

#複数一気に適合はうまくいかず
#left = print(list(sumdata, gradrate, covariates) %>% reduce (left_join, by = "unitid", year" ))

#csvに保存
write.csv(masterdata, "~/Desktop/ra-bootcamp-warmup/cleaning/masterdata.csv")

