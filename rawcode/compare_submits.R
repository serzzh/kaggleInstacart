head(submission)
submit <- fread(file.path(path, "submit.csv"))
colnames(submit)<-c('order_id', 'or2')
head(submit)
t<-merge(submit, submission, on='order_id')
head(t)
head(submission)

t[t$or2 != t$products,]
