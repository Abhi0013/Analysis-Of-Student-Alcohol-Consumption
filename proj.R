install.packages("hms")
attach(student_mat)
lm1<-lm(student_mat$G3~student_mat$school+student_mat$sex+student_mat$age+student_mat$address+student_mat$famsize+student_mat$Pstatus+student_mat$Medu+student_mat$Fedu+student_mat$Mjob+student_mat$Fjob+student_mat$reason+student_mat$guardian+student_mat$traveltime+student_mat$studytime+student_mat$failures+student_mat$schoolsup+student_mat$famsup+student_mat$paid+student_mat$activities+student_mat$nursery+student_mat$higher+student_mat$internet+student_mat$romantic+student_mat$famrel+student_mat$freetime+student_mat$goout+student_mat$Dalc+student_mat$Walc+student_mat$health+student_mat$absences+student_mat$G1+student_mat$G2,data=student_mat)
summary(lm1)
lm2<-lm(formula = student_mat_avg$Avg ~ student_mat_avg$school + student_mat_avg$sex + 
     student_mat_avg$age + student_mat_avg$address + student_mat_avg$famsize + 
     student_mat_avg$Pstatus + student_mat_avg$Medu + student_mat_avg$Fedu + 
     student_mat_avg$Mjob + student_mat_avg$Fjob + student_mat_avg$reason + 
     student_mat_avg$guardian + student_mat_avg$traveltime + student_mat_avg$studytime + 
     student_mat_avg$failures + student_mat_avg$schoolsup + student_mat_avg$famsup + 
     student_mat_avg$paid + student_mat_avg$activities + student_mat_avg$nursery + 
     student_mat_avg$higher + student_mat_avg$internet + student_mat_avg$romantic + 
     student_mat_avg$famrel + student_mat_avg$freetime + student_mat_avg$goout + 
     student_mat_avg$Dalc + student_mat_avg$Walc + student_mat_avg$health + 
     student_mat_avg$absences, data = student_mat_avg)
summary(lm2)
lm3<-lm(formula = student_mat_avg$Avg ~ student_mat_avg$studytime + student_mat_avg$failures + student_mat_avg$schoolsup + student_mat_avg$famsup + student_mat_avg$goout,data = student_mat_avg)
summary(lm3)
plot(lm3)
plot(predict(lm2),student_mat_avg$Avg)
plot(lm2)
tds<-data.frame(model.matrix(~.- 1,data=student_mat_avg))

res<- cor(tds)
lm4<-lm(student_mat_avg$Avg~student_mat_avg$Medu + student_mat_avg$failures + student_mat_avg$schoolsup + student_mat_avg$higher + student_mat_avg$goout,data = student_mat_avg)
summary(lm4)
lm5<-lm(student_por_avg$Avg~student_por_avg$failures + student_por_avg$higher+student_por_avg$school+student_por_avg$Medu+student_por_avg$studytime+student_por_avg$Fedu+student_por_avg$Dalc+student_por_avg$Walc,data= student_por_avg) 
summary(lm5)
tds2<-data.frame(model.matrix(~.- 1,data=student_mat))
tds3<-data.frame(model.matrix(~.- 1,data=student_por))
res3<-cor(tds3)
res3
res2<- cor(tds2)
res2
lm6<-lm(student_por$G3~student_por$G2+student_por$G1+student_por$higher+student_por$school+student_por$Medu+student_por$Fedu+student_por$studytime+student_por$failures+student_por$Dalc,data=student_por)
summary(lm6)






