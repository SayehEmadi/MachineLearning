TrainFile=read.csv("Training-breastCancer.csv",na.strings="?")
TestFile=read.csv("Testing-breastCancer.csv",na.strings="?")
classValue1=2
classValue2=4
TrainClass2 <- subset(TrainFile, class == classValue1)
TrainClass4 <- subset(TrainFile, class == classValue2)

data <- data.frame(Id = 0,               
                   Real_Class = 0,Predicted_Class=0,Maximum_Probability=0) 

for (r in 1:nrow(TestFile)){
  tmp_p2=1
  tmp_p4=1
  for (c in 2:(ncol(TestFile)-1)){
    tmp_p2=tmp_p2*nrow(subset(TrainClass2, TrainClass2[,c] == TestFile[r,c]))/nrow(TrainClass2)
    tmp_p4=tmp_p4*nrow(subset(TrainClass4, TrainClass4[,c] == TestFile[r,c]))/nrow(TrainClass4)
 
  }
  p2=tmp_p2*(nrow(TrainClass2)/nrow(TrainFile))
  p4=tmp_p4*(nrow(TrainClass4)/nrow(TrainFile))

 
  if(p2>p4){
    data[r,] =c(TestFile[r,1],TestFile[r,ncol(TestFile)],classValue1,p2)
  }
   else {
    data[r,] =c(TestFile[r,1],TestFile[r,ncol(TestFile)],classValue2,p4)
   }
  if(p2==0 & p4==0){
    data[r,] =c(TestFile[r,1],TestFile[r,ncol(TestFile)],'-',p2)
  }
  
}

#Calculate Accuracy
accuracy=0
for(i in 1:nrow(data)){
  if(data[i,2]==data[i,3]){
    accuracy=accuracy+1
  }
}

accuracy=accuracy/nrow(data)
write.csv(data,"OutputNB-MojtabaAsadollahpour.csv",row.names = FALSE)
print("Result")
print(data)
print("Accuracy")
print(accuracy)







  
  

