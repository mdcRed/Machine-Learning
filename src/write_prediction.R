write_prediction = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("../output/problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}