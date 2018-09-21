## SMDE; 
## Ricard Meyerhofer Parra

#We set a path where we have our outputs
path <- "/c/Users/Dirken/Desktop/SMDE/Lab1/RNG-Algorithms/outputs"
setwd(path)
#We load the RNG values generated from one algorithm such as:
read.csv("file")

#generation of a uniform distribution.
v1=punif(200, min = 0, max = 1)
summary(v1)

#Work with the data as a dataframe.
taula_v1=data.frame(x1=v1)

#Definition of the intervals, categories to be used.
taula_v1_cat=transform(taula_v1, cat = ifelse(x1 < -1,"-1",
                                              ifelse(x1 < -0.5,"-0.5",
                                                     ifelse(x1 < 0,"0",
                                                            ifelse(x1 < 0.5,"0.5",
                                                                   ifelse(x1 <1,"1","Inf"))))))

#Counting the amount of elements in each category “table” function.
taula_freq_v1=as.data.frame(with(taula_v1_cat, table(cat)))


#read from a spreadsheet doing excel stuff xd




Test=chisq.test(taula_freq, correct=FALSE)

