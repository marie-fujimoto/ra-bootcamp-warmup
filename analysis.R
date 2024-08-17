
#NAの数
na_count=masterdata %>% summarise_all(~ sum(is.na(.)))

------
#Table1をつくろう
#never switcher, switcherを作りたい
  
#switchしたところの大学名
sunidata =masterdata[masterdata$dummy_switch == '1', ]
suni=sunidata$instnm
----
#never switchersの特定
nsunidata = masterdata [ !(masterdata$instnm %in% c(suni)) ,]
#switcherの特定
sunidata = masterdata [ masterdata$instnm %in% c(suni) ,]

library(dplyr)

#switchers の平均
sunitable= colMeans(sunidata[c(4,8, 14, 15, 16, 17, 18, 19,20)], na.rm = TRUE)

#never switchersの平均, なぜか単位が変。
nsunitable= colMeans(nsunidata[c(4,8, 14:20)],  na.rm = TRUE)

#all universitiesの平均. 同じく変
masterunitable = colMeans(masterdata[c(4,8, 14, 15, 16, 17, 18, 19,20)], na.rm = TRUE)

#なぜかrbindしたら治った. cbindだと変のまま
table1=rbind(masterunitable, sunitable, nsunitable)
table1=data.frame(table1)
#rownames を加える
rownames(table1) <- c("All","Switchers","Never Switchers") 
#round
table1=round(table1, digits = 2)

#flip the columns and rows
table1=t(table1)
help("nice_table")

#nice_table
install.packages("rempsyc")
library(rempsyc)
nice_table(table1, title = c("Table 1", "Institution-Level Summary Statistics" ))

#rownamesが消えてる

#Figure1 よくわからない
library(ggplot2)
p_0 <- ggplot(data = masterdata, mapping = aes(x = year, y = semester))
              p_1 <- p_0 + 
                layer(geom = "point", stat = "identity", position = "identity")
            