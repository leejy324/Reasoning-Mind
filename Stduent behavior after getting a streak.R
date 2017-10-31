#论坛回答
dat <- read_csv(file="MiniData.csv")
dat %>% 
  group_by(`Student ID`) %>% 
  mutate(correct = Result == "correct", 
         cum_cor = roll_sum(x = correct, n = 5, fill = NA, align = "right"), 
         judge = lag(cum_cor, n = 1) == 5) %>% 
  filter(judge) %>% 
  ungroup() %>% 
  count(`Event type`)

#自己尝试,连续对5题的反应和时间
library(RcppRoll)
RMData1$Consecutive_correct = roll_sum(RMData1$Results, n=5, fill = NA, na.rm = TRUE, align = "right")
RMData1$If_streak <- ifelse(RMData1$Consecutive_correct == 5, 1, 0)
RMData1$Mode_after_streak = ifelse(RMData1$`Student ID`[i] != RMData1$`Student ID`[i+1], NA, 
                                   ifelse(RMData1$If_streak[i] == 1, RMData1$Mode[i+1], NA)) #如果连续对了5道题，就返回下一次回应的Mode
RMData1$If_switch <- !(RMData1$Mode == RMData1$Mode_after_streak) #连续对5道题以后，学生是否会切换到其他模式

RMData1$Time_after_streak = ifelse(RMData1$`Student ID`[i] != RMData1$`Student ID`[i+1], NA, 
                                   ifelse(RMData1$If_streak[i] == 1, RMData1$TimeUsage[i+1], NA))


#IRT 重铸数据
#itemset 是只有studentID,contentitem 和reuslt的数据集，并且没有重复的行

#计算每个item的correctness
library(dplyr)
library(tidyr)
itemset12 <- select(RMData12, student, item, Results)
itemset12 <- filter(itemset12, Results == 1 | Results == 0)
#itemset2$If_duplicated <- duplicated(itemset$item)
#itemset2 <- filter(itemset2, If_duplicated == FALSE)
#itemset2 <- select(itemset2, -If_duplicated)
#itemset2 <- spread(data = itemset, item, Results) #一共有17863道题 + 1列StudentID
#itemset2 <- select(itemset, -StudentID)
itemset_group <- group_by(itemset12, item)
itemset12 <- summarise(itemset_group, Total_actions = n(), item_correctness = sum(Results)/Total_actions)

#把item的correctness绑定到原始数据中
RMData12 <- left_join(RMData12, itemset12, by = "item")








#empty变量是判断x当中某一行的NA值的和，为的之后删除只有1个response的行
itemset$empty <- NA
for (i in 1:1336) {
 if(sum(is.na(itemset[i,])) >= 17847){
   itemset$empty[i] = TRUE}
 else {
  itemset$empty[i] = FALSE
 }
   }

itemset2 <- filter(itemset, empty == FALSE) #删除那些只有1个response的行

col_index <- 1:17848
#找出那些全部为NA的列
for (i in 1:17848) {
  if(sum(is.na(itemset2[,i])) == 994){
    col_index[i] = TRUE}
  else {
    col_index[i] = FALSE
  }
}
col_index <- as.logical(col_index)
itemset2 <- select(itemset2, -empty) #删除多余的empty这一列
itemset2 <- itemset2[,!col_index] #x2就是已经删除了全部为NA的列的数据集




RMData1 <- mutate(RMData1,Consecutive_correct1 = roll_sum(Results, n=5, fill = NA, na.rm = TRUE, align = "right"))
mutate(RMData1, if_streak = ifelse(Consecutive_correct = 5, 1, 0))


RMData1$Mode_after_streak = ifelse(RMData1$`Student ID`[i] != RMData1$`Student ID`[i+1], NA, 
                                   ifelse(RMData1$if_streak[i] == 1, RMData1$Mode[i+1], NA)) #如果连续对了5道题，就返回下一次回应的Mode




