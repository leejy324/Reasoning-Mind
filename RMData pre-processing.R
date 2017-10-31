#第一批数据处理
library(readr)
RMData12 <- read_csv("EXTERNAL-Foundations_STAAR_SmarterSolving_events_20170701-20170731.csv",na = "NA")
RMData12$Results <- ifelse(RMData12$Result == "incorrect", 0, ifelse(RMData12$Result == "correct", 1, NA)) #把incorrect和correct替换成0和1
RMData12$TimeUsage = NA #创建一个空变量
i = c(1:nrow(RMData12)) #创建索引
names(RMData12)[1] <- "student"
RMData12$TimeUsage <- ifelse(RMData12$student[i] != RMData12$student[i+1], 0, 
                            RMData12$Timestamp[i+1] - RMData12$Timestamp[i]) #像Excel里一样计算TimeUsage
RMData12$TimeUsage[is.na(RMData12$TimeUsage)] <- 0 #把TimeUsage中的NA替换为0
RMData12 <- select(RMData12, -Result)
RMData12 <- filter(RMData12, RMData12$TimeUsage <= 600) #删除所有超过10分钟的行

#Survey数据框之前必须加载好
#RMData12 <- semi_join(RMData12, Survey, by = "student") #只保留那些做了Survey的学生的日志记录

#RMData12$row_num <- as.integer(rownames(RMData12))
#names(RMData12)[10] <- "item"

i <- 1:nrow(RMData12)

#Feature 1
#是否做speedgame,及其正确率
RMData12$If_speedgame <- ifelse(RMData12$`Content type` == "Speedgame", 1, 0)
RMData12$Results_of_speedgame = ifelse(RMData12$If_speedgame == 1 & RMData12$Results == 1,
                                      1, ifelse(RMData12$If_speedgame == 1 & RMData12$Results == 0, 0, NA))
#是否做theory,及其正确率
RMData12$If_theory <- ifelse(RMData12$`Content type` == "Theory", 1, 0)
RMData12$Results_of_theory = ifelse(RMData12$If_theory == 1 & RMData12$Results == 1,
                                      1, ifelse(RMData12$If_theory == 1 & RMData12$Results == 0, 0, NA))
#是否做notestest,及其正确率
RMData12$If_notestest <- ifelse(RMData12$`Content type` == "Notestest", 1, 0)
RMData12$Results_of_notestest = ifelse(RMData12$If_notestest == 1 & RMData12$Results == 1,
                                   1, ifelse(RMData12$If_notestest == 1 & RMData12$Results == 0, 0, NA))
                            
#是否做ProblemA，及其正确率 
RMData12$If_problemA = ifelse(RMData12$`Content type`=="Problem A", 1, 0)
RMData12$Results_of_problemA = ifelse(RMData12$If_problemA == 1 & RMData12$Results == 1, 
                                      1, ifelse(RMData12$If_problemA == 1 & RMData12$Results == 0, 0, NA))
#是否做ProblemB，及其正确率 
RMData12$If_problemB = ifelse(RMData12$`Content type`=="Problem B", 1, 0)
RMData12$Results_of_problemB = ifelse(RMData12$If_problemB == 1 & RMData12$Results == 1, 
                                      1, ifelse(RMData12$If_problemB == 1 & RMData12$Results == 0, 0, NA))
#是否做ProblemC，及其正确率 
RMData12$If_problemC = ifelse(RMData12$`Content type`=="Problem C", 1, 0)
RMData12$Results_of_problemC = ifelse(RMData12$If_problemC == 1 & RMData12$Results == 1, 
                                      1, ifelse(RMData12$If_problemC == 1 & RMData12$Results == 0, 0, NA))


#Feature 2(连续正确5次以后的反应及其时间)
library(RcppRoll)
RMData12$Consecutive_correct = roll_sum(RMData12$Results, n=5, fill = NA, na.rm = TRUE, align = "right")
RMData12$If_streak <- ifelse(RMData12$Consecutive_correct == 5, 1, 0)
RMData12$Mode_after_streak = ifelse(RMData12$student[i] != RMData12$student[i+1], NA, 
                                   ifelse(RMData12$If_streak[i] == 1, RMData12$Mode[i+1], NA)) #如果连续对了5道题，就返回下一次回应的Mode
RMData12$If_switch <- !(RMData12$Mode == RMData12$Mode_after_streak) #连续对5道题以后，学生是否会切换到其他模式

RMData12$Time_after_streak = ifelse(RMData12$student[i] != RMData12$student[i+1], NA, 
                                   ifelse(RMData12$If_streak[i] == 1, RMData12$TimeUsage[i+1], NA))
RMData12$Off_task_after_streak <- ifelse(RMData12$Time_after_streak >= 80, 1,0)

#RMData12 <- select(RMData12, -Consecutive_correct) #删除多余变量


#Feature 3(学生是否主动承担困难题目)
RMData12$Voluntarily_challengeproblem <- ifelse(RMData12$Mode =="WALL_OF_MASTERY" & RMData12$`Content type`=="Problem C",1,0)
RMData12 <- mutate(RMData12, VCP_time = ifelse(Voluntarily_challengeproblem==1, TimeUsage,0))
#Prop_vcp_time = sum(VCP_time) / sum(ProblemA_time, ProblemB_time, ProblemC_time, Speedgame_time, Theory_time, Notestest_time)#在整合阶段创建
#Prop_vcp_actions = sum(Voluntarily_ChallengeProblem) / sum(If_ProblemA, If_ProblemB, If_ProblemC, If_Speedgame, If_Theory, If_Notetests)#在整合阶段创建

#Feature 4(math games的速度)
library(stringr)
RMData12$Speedgame_time <- ifelse(RMData12$If_speedgame==1,RMData12$TimeUsage,0)
RMData12$Speedgame_items <- ifelse(RMData12$If_speedgame==1 & RMData12$`Event type`=="Submit",
                                  str_count(RMData12$`Student answer`, "="),0)

#Speedgame_speed = sum(Speedgame_time) / sum(Speedgame_items) 在整合阶段的时候创建


#Feature 5 (学生是否会做RMG)
RMData12$If_RMG <- ifelse(RMData12$Mode == "RMG", 1, 0)
#Total_count_RMG = sum(If_RMG) 在整合阶段创建
#Averageuse_RMG = Total_count_RMG / tally(group_by(Combined_Data, Student ID)，在Combined_data的时候创建，即9个文件整合以后再创建

#Feature 6 (基础练习时间，是否写邮件，是否装饰小屋)
RMData12$Basic_practice_time <- ifelse(RMData12$If_theory==1, RMData12$TimeUsage, 0)
# Prop_basicpractice_time = sum(Basic_practice_time) / sum(TimeUsage)在整合阶段创建
# Prop_theory_actions = sum(If_basic_practice) / count=n() 在整合阶段创建
# 邮件暂时忽略
#RMData12$If_mail <- ifelse(RMData12$Mode == "MAILBOX", 1, 0)
#RMData12$Mail_time <- ifelse(RMData12$If_mail == "1", RMData12$TimeUsage, 0)
#RMData12$Mail_30s <- ifelse(RMData12$If_mail == "1" & RMData12$Mail_time >= 30, 1, 0) #是否超过30s
#RMData12$Mail_realtime <- ifelse(RMData12$Mail_30s == 1, RMData12$TimeUsage, 0) #真正的Mail时间
RMData12$If_myplace = ifelse(RMData12$Mode=="MY_PLACE", 1, 0) #是否进入myplace,myplace和library是分开的
RMData12$Myplace_time = ifelse(RMData12$If_myplace == 1, RMData12$TimeUsage, 0) #myplace的时间
RMData12$Myplace_20s <- ifelse(RMData12$If_myplace == 1 & RMData12$Myplace_time >= 20, 1, 0) #是否超过20s
RMData12$Myplace_20s_time <- ifelse(RMData12$Myplace_20s ==1, RMData12$TimeUsage, 0) #使用myplace超过20s的行为所花费的时间


#Feature 7 (学生在家使用RM的时间)
library(lubridate) #处理时间
Sys.setlocale("LC_TIME", "de_DE") #设定OS系统的字符格式
#备用：把Datatime中的“MAR”之类识别不了的月份简写替换成小写或数字
#substr(RMData12$Datetime, 4, 6) <- "Mar"
RMData12$Datetime <- gsub("JUL","07",RMData12$Datetime) #如果有必要，把Datetime里面的英文月份改成数字月份
RMData12$Datetime1 = parse_date_time2(RMData12$Datetime,"d!/m!/Y! I!:M!:S! p!", tz = "America/Chicago") #把Datetime转换成时间格式
RMData12$Whichday = wday(RMData12$Datetime1, label = FALSE) #识别Datetime是属于星期几
RMData12$Date = as.Date(RMData12$Datetime1) #把Datetime里面的年月日提取出来，以便下一步做比较
Force_tz = force_tz(RMData12$Date+hm("15:20"), tzone = "America/Chicago") #创建放学时间的基线，调整好时区
RMData12$If_after_schooltime = ifelse(RMData12$Datetime1 > Force_tz, 1, 0) #Datetime是否大于放学时间15:20
RMData12$If_weekends = ifelse(RMData12$Whichday=="1"|RMData12$Whichday == "7", 1, 0) #是否是周末
RMData12$If_learning_at_home = ifelse(RMData12$If_weekends=="1" | RMData12$If_after_schooltime == "1", 1, 0) #学生是否在放学时间使用RM
RMData12$Time_at_home = ifelse(RMData12$If_learning_at_home == "1", RMData12$TimeUsage, 0) #学生在家使用RM的时间
#Totaltime_at_home = sum(Time_at_home) #在整合阶段创建


#Feature 8(Usage of hints)把BKT在R中的实现搞清楚以后再弄，BKT参数和基线已经弄好

#Not requesting help on poorly known skill(cut-off = 0.97)
RMData12$Nohelp_poor <- ifelse(RMData12$Ln.1 <=0.7 & RMData12$Hints != 1, 1, 0)

#Not requesting help on well known skill(cut-off = 0.97)
RMData12$Nohelp_well <- ifelse(RMData12$Ln.1 >=0.75 & RMData12$Hints != 1, 1, 0)

#Long pauses after reading hints
RMData12$Longpause_afterhints <- ifelse(RMData12$student[i] != RMData12$student[i+1], 0, 
                         ifelse(RMData12$Hints[i]==1 & RMData12$TimeUsage[i+1] >= 18, 1, 0))
#Short pauses after reading hints
RMData12$Shortpause_afterhints <- ifelse(RMData12$student[i] != RMData12$student[i+1], 0, 
                                       ifelse(RMData12$Hints[i]==1 & RMData12$TimeUsage[i+1] <= 9, 1, 0))

#Long pauses after reading hits and getting correct
RMData12$Longpause_afterhints_correct  <- ifelse(RMData12$student[i] != RMData12$student[i+1], 0, 
                                ifelse(RMData12$Hints[i]==1 & RMData12$TimeUsage[i+1] >= 18 & RMData12$Results == 1, 1, 0))

#Short pauses after reading hints
RMData12$Shortpause_afterhints_correct  <- ifelse(RMData12$student[i] != RMData12$student[i+1], 0, 
                                                ifelse(RMData12$Hints[i]==1 & RMData12$TimeUsage[i+1] <= 9 & RMData12$Results == 1, 1, 0))

#Feature 9(题目难度)
RMData12$If_diffcultitem <- ifelse(RMData12$item_correctness < mean(RMData12$item_correctness,na.rm = T)-sd(RMData12$item_correctness,na.rm = T)
                                  , 1, 0)



#Feature 10
#10a 学生被告知错误以后--到--做出下一次回应之间的平均时间
i <- c(1:nrow(RMData12))
a = vector(mode = "character", length = nrow(RMData12)) #创建一个空向量a
a[i+1] = RMData12$Results[i] #a是中间变量，为创建Time_after_wrong做准备
a = a[-(nrow(RMData12)+1)] #删除最后多余的一行
RMData12 = cbind(RMData12, a) #把数据集和中间变量绑定起来
#RMData12$Time_after_wrong1 <- ifelse(RMData12$student[i] != RMData12$student[i+1], NA, ifelse(RMData12$a == 0, RMData12$TimeUsage, NA)) #计算被告知错误以后的那一次行为的时间
RMData12$Time_after_wrong1 <- ifelse(RMData12$student[i] == RMData12$student[i+1] & RMData12$a == 0, RMData12$TimeUsage, NA)
RMData12$Time_after_wrong[i] <- RMData12$Time_after_wrong1[i+1]
RMData12 <- select(RMData12, -c(a,Time_after_wrong1))
#RMData12$If_wrong <- ifelse(RMData12$Results == 0, 1, 0) 
#RMData12$Wrongtime <- ifelse(RMData12$If_wrong ==1, RMData12$TimeUsage, 0)


#10b Proportion of wrong actions where student takes more than N seconds，cut-off是6秒
RMData12$Wrong_over6s <- ifelse(RMData12$Results==0 & RMData12$TimeUsage >= 6, 1, 0)
#Prop_of_wrongover6s <- sum(Wrong_over6s)/count=n() #在整合阶段创建


#Feature11 how much cost on books
RMData12$Book <- ifelse(RMData12$`Purchased Item`=="A Bedroom for Mustafa, Part 1 (200)"
                        |RMData12$`Purchased Item`=="A Bedroom for Mustafa, Part 2 (200)"
                        |RMData12$`Purchased Item`=="A Bedroom for Mustafa, Part 3 (200)"
                        |RMData12$`Purchased Item`=="A Bedroom for Mustafa, Part 4 (200)"
                        |RMData12$`Purchased Item`=="A Bedroom for Mustafa, Part 5 (200)"
                        |RMData12$`Purchased Item`=="Island Dreams (1000)"
                        |RMData12$`Purchased Item`=="The Ant and the Grasshopper (300)"
                        |RMData12$`Purchased Item`=="The Dog and the Reflection (300)"
                        |RMData12$`Purchased Item`=="The Fox and the Crow (300)"
                        |RMData12$`Purchased Item`=="The Fox and the Grapes (200)"
                        |RMData12$`Purchased Item`=="The Tortoise and the Hare (300)"
                        |RMData12$`Purchased Item`=="The Wind of Change, Part 1 (400)"
                        |RMData12$`Purchased Item`=="The Wind of Change, Part 2 (400)"
                        |RMData12$`Purchased Item`=="The Wind of Change, Part 3 (400)", 1, 0)

#提取purchased item里面的书本价格，即大于等于3个的数字
RMData12$Book_cost <- ifelse(RMData12$Book == 1, str_extract(pattern = "\\d{3,}", string = RMData12$`Purchased Item`), 0)
RMData12$Book_cost <- as.integer(RMData12$Book_cost)
sum(RMData12$Book_cost,na.rm = T)


#整合数据
library(dplyr)
RMData12 = select(RMData12, -Datetime1) #删除Datatime1,因为它的格式是时间格式，会导致无法创建分组索引(不知道原因)

GroupIndex <- group_by(RMData12, student) #先按照学生ID分好组
RMData_aggregate12 <- summarise(GroupIndex, SchoolID = max(`School ID`, na.rm = TRUE), TeacherID = max(`Teacher ID`, na.rm = TRUE), 
                            Gradelevel = max(`Grade level`, na.rm = TRUE), SessionUID = max(`Session UID`, na.rm = TRUE), Date = min(Date),
                            Total_actions = n(),
                            F1_Correctness_problemA = mean(Results_of_problemA, na.rm = TRUE), 
                            F1_Correctness_problemB = mean(Results_of_problemB, na.rm = TRUE),
                            F1_Correctness_problemC = mean(Results_of_problemC, na.rm = TRUE),
                            F1_Correctness_speedgame = mean(Results_of_speedgame,na.rm = TRUE),
                            F1_Correctness_notestest = mean(Results_of_notestest,na.rm = TRUE),
                            F1_Correctness_theory = mean(Results_of_theory,na.rm = TRUE),
                            F2_Switch_mode = sum(If_switch,na.rm = TRUE)/Total_actions,
                            F2_Off_task_after_streak = max(Off_task_after_streak, na.rm = TRUE),
                            F2_Average_Careless = mean(Slip_TRIO, na.rm = TRUE),
                            F2_Average_Guess = mean(Guess_TRIO, na.rm = TRUE),
                            F3_Prop_vcp_time = sum(VCP_time)/sum(TimeUsage),
                            F3_Prop_vcp_actions = sum(Voluntarily_challengeproblem)/sum(If_problemA, If_problemB, If_problemC, If_speedgame, If_theory, If_notestest),
                            F4_Speedgame_speed = sum(Speedgame_time)/sum(Speedgame_items),
                            F5_Total_count_RMG = sum(If_RMG),
                            F6_Prop_bp_time = sum(Basic_practice_time)/sum(TimeUsage),
                            F6_Prop_bp_actions = sum(If_theory)/Total_actions, #要除以总actions数量
                            F6_Decorating_room = max(Myplace_20s),
                            F6_Prop_Decorating_room = sum(Myplace_20s_time)/sum(TimeUsage),
                            F7_Time_at_home = sum(Time_at_home),
                            F7_Enter_GenieMail = max(Enter_mail, na.rm = TRUE),
                            F8_Nohelp_poor = sum(Nohelp_poor, na.rm = TRUE)/Total_actions, #要除以总actions数量
                            F8_Nohelp_well = sum(Nohelp_well, na.rm = TRUE)/Total_actions, #要除以总actions数量
                            F8_Longpause_afterhints = sum(Longpause_afterhints, na.rm = TRUE)/Total_actions, #要除以总actions数量
                            F8_Shortpause_afterhints = sum(Shortpause_afterhints, na.rm = TRUE)/Total_actions, #要除以总actions数量
                            F8_Longpause_afterhints_correct = sum(Longpause_afterhints_correct, na.rm = TRUE)/Total_actions, #要除以总actions数量
                            F8_Shortpause_afterhints_correct = sum(Shortpause_afterhints_correct, na.rm = TRUE)/Total_actions, #要除以总actions数量
                            F9_Prop_of_doing_difficultitems = sum(If_diffcultitem, na.rm = TRUE)/Total_actions, #要除以总actions数量
                            F10_Ave_time_after_wrong = mean(Time_after_wrong,na.rm = TRUE), 
                            F10_Prop_of_wrongover6s = sum(Wrong_over6s,na.rm = TRUE)/Total_actions,
                            F11_Bookcost = sum(Book_cost, na.rm = TRUE)) #要除以总actions数量

#哪些学生写了GenieMail超过50个单词？
RMData_aggregate1 <- left_join(RMData_aggregate1, Write_Mail1, by = "student")
#把Write_GenieMail_50words中的NA换成0
RMData_aggregate1$Write_GenieMail_50words <- ifelse(is.na(RMData_aggregate1$Write_GenieMail_50words) == TRUE, 0, 1)

write.csv(Longitudinal_RMData, "Longitudinal_RMData.csv",na = "NA", row.names = FALSE)


Longitudinal_RMData <- bind_rows(RMData_aggregate1, RMData_aggregate2,RMData_aggregate3,RMData_aggregate4,RMData_aggregate5,RMData_aggregate6,
                                 RMData_aggregate7,RMData_aggregate8,RMData_aggregate9,RMData_aggregate10,RMData_aggregate11,RMData_aggregate12)
Longitudinal_RMData <- arrange(Longitudinal_RMData, student, Date)

write.csv(Longitudinal_RMData, "Longitudinal_RMData0818.csv", row.names = FALSE, na = "NA")




#F2_Switch_mode里有些值是-Inf,把它们全部替换成0                 
RMData_aggregate1$F2_Switch_mode <- gsub(pattern = "-Inf", replacement = "0", x = RMData_aggregate1$F2_Switch_mode)            
 
#备用代码
#Aggregate1 <- read.csv("Aggregate1.csv", header = T)
#Aggregate1 <- left_join(Aggregate1, Survey1, by = "student") 
#write.csv(Aggregate9, "Aggregate9.csv") 
#TEST <- duplicated(Survey_1$student) #删除数据集中重复的ID
#Survey_1 <- Survey_1[!TEST,]
                            
#合并问卷数据
QuestionarieData <- read_csv("Questionarie.csv",na = "NA") #导入问卷调查数据
names(QuestionarieData)[1] = "student" #让两个数据的uniqueID保持名称一致，此处为Student ID
QuestionarieData <- mutate(QuestionarieData, Value = Q1+Q2+Q3+Q4+Q5, Self_efficacy = Q6+Q7+Q8+Q9+Q10, Interest = Q15+Q16+Q17, Math_identity = Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+Q9+Q10)
RMData_aggregate1 = left_join(RMData_aggregate1, Survey, by = "student") #合并两个数据集，以Student ID为唯一索引
RMData_Studentlevel <- RMData_Studentlevel[!is.na(RMData_Studentlevel$Timestamp),] #删除RMData_Studentlevel中的缺失值
write.csv(RMData_Studentlevel, "TrtItOnMan.csv")
