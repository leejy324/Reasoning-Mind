BKT1 <- select(RMData1, row_num, student, `Current objective`, item, `Try number`, Results, Hints)
BKT1 <- filter(BKT1, `Try number` == "first" & (Results == 0 | Results == 1))
BKT1 <- filter(BKT1, !`Current objective`=="")
names(BKT1)[3] <- "skill"
BKT1$s?ill <- gsub(pattern = "[[:punct:]]", "", BKT1$skill) 
BKT1$skill <- gsub(pattern = "[[:blank:]]", "", BKT1$skill)
BKT1 <- arrange(BKT1, skill,student)
#BKT1$num <- BKT_skill9$num?
#RMData1 <- left_join(RMData1, BKT1, by = "num")


# Build Dataset for Brute?orce-BKT
BKT_fitdata <- BKT1
BKT_fitdata$num <- as.integer(rownames(BKT_fitdata))
BKT_fitdata$lesson <- "first"
BKT_fitdata$cell <- "cell"
BKT_fitdata$eol <- "eol"
BKT_fitdata$student <- paste("s",BKT_fitdata$student, sep="") 
names(BKT_fitdata)[6] <- "rig?t"
BKT_fitdata <- select(BKT_fitdata, num, lesson, student, skill, cell, right, eol)

write_delim(BKT_fitdata, "BKTData_1.txt", delim="\t")


# Aggregate Skill parameters
skill_parameters <- bind_rows(skill1,skill2,skill3,skill4,skill5,skill6,skill7,skill8?skill9,skill10,skill11,skill12)

skill_parameters <- arrange(skill_parameters, skill)

Aggregate_index <- group_by(skill_parameters, skill)
Skill_Aggregate <- summarise(Aggregate_index, L0 = mean(L0, na.rm = TRUE),G = mean(G, na.rm = TRUE), S = mean(S, na.?m = TRUE),T = mean(T, na.rm = TRUE))


# Build Dataset for applying BKT
BKT_test1 <- left_join(BKT1,Skill_Aggregate, by = "skill")






