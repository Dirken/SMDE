## SMDE; 
## Ricard Meyerhofer Parra

#We set a path where we have our outputs
path <- "C:/Users/Dirken/Desktop/MIRI/SMDE/Lab1/RNG-Algorithms/outputs/"
setwd(path)
#We load the RNG values generated from one algorithm such as:
fileRead <- read.csv("samples.csv", sep="\t", header = F)
#generation of a uniform distribution.
v1=runif(200, min = 0, max = 1)
summary(v1)


#Work with the data as a dataframe.
taula_v1=data.frame(x1=v1)

#Definition of the intervals, categories to be used.
taula_v1_cat=transform(taula_v1, cat = ifelse(x1 < 0,"0",
                                              ifelse(x1 < 0.2,"0.2",
                                                     ifelse(x1 < 0.4,"0.4",
                                                            ifelse(x1 < 0.6,"0.6",
                                                                   
                                                                   ifelse(x1 < 0.8,"0.8","1"))))))
taula_v2_cat=transform(fileRead, cat2 = ifelse(V1 < 0,"0",
                                              ifelse(V1 < 0.2,"0.2",
                                                     ifelse(V1 < 0.4,"0.4",
                                                            ifelse(V1 < 0.6,"0.6",
                                                                   ifelse(V1 < 0.8,"0.8","1"))))))

#Counting the amount of elements in each category “table” function.
taula_freq_v1=as.data.frame(with(taula_v1_cat, table(cat)))
taula_freq_v2=as.data.frame(with(taula_v2_cat, table(cat2)))

f1=taula_freq_v1$Freq
f2=taula_freq_v2$Freq


Test=chisq.test(f1,f2, correct=FALSE)

