z <- data.frame(a = rep(1:50000,100), b = sample(LETTERS, 5000000, replace = TRUE))

library(data.table)
library(dplyr)

#dplyr
system.time({
        y <- z %>% 
                group_by(a) %>% 
                summarise(c = names(which(table(b) == max(table(b)))[1]))  
})


#data.table
system.time(
        setDT(z)[, .N, by=b][order(N),][.N,]
)


#@zx8754 's way - base R
system.time(
        names(sort(table(z$b),decreasing = TRUE)[1])
)
user  system elapsed 
0.73    0.06    0.81 