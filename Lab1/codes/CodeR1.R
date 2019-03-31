# Author: Ricard Meyerhofer Parra
# Subject: SMDE
# Professor: Pau Fonseca

# We generate a sample with one of the generators that we could've used. In this case, we've used mersenne twister (with the first parametrization) since
# is one of the best generators. Anyway we will try multiple generators. 

setwd("C:/Users/Meyerhofer/Desktop/SMDELabs")

mersenne_twister <- read.csv("samples.csv", sep = "\t", header = F)

mersenne_twister_class = transform(mersenne_twister, cat2 = ifelse(V1 < 0,"0",
                                                                   ifelse(V1 < 0.2,"0.2",
                                                                   ifelse(V1 < 0.4,"0.4",
                                                                   ifelse(V1 < 0.6,"0.6",
                                                                   ifelse(V1 < 0.8,"0.8","1"))))))
hist(mersenne_twister_class$V1)
mersenne_freqs = as.data.frame(with(mersenne_twister_class, table(cat2)))


#Now that we have this generator created and we already have the data as we want. We are going to see if it follows
# a normal distribution by applying a chi square test.

#generation of a uniform distribution with a certain seed to make it reproducible.
set.seed(1234)
v1=runif(200, min = 0, max = 1)
hist(v1)
#Work with the data as a dataframe.
taula_v1=data.frame(x1=v1)

#Definition of the intervals, categories to be used.
taula_v1_cat=transform(taula_v1, cat1 = ifelse(x1 < 0,"0",
                                               ifelse(x1 < 0.2,"0.2",
                                               ifelse(x1 < 0.4,"0.4",
                                               ifelse(x1 < 0.6,"0.6",
                                               ifelse(x1 < 0.8,"0.8","1"))))))

#Counting the amount of elements in each category “table” function.
taula_freq_v1=as.data.frame(with(taula_v1_cat, table(cat1)))
hist(taula_v1_cat$x1)
taula_freq = rbind(taula_freq_v1$Freq, mersenne_freqs$Freq)

Test=chisq.test(taula_freq, correct = FALSE)



#Now let's proceed to do the same for a bigger subset of 3000 elements
setwd("C:/Users/Meyerhofer/Desktop/SMDELabs")

mersenne_twister <- read.csv("samples20000.csv", sep = "\t", header = F)

mersenne_twister_class = transform(mersenne_twister, cat2 = ifelse(V1 < 0,"0",
                                                                   ifelse(V1 < 0.2,"0.2",
                                                                   ifelse(V1 < 0.4,"0.4",
                                                                   ifelse(V1 < 0.6,"0.6",
                                                                   ifelse(V1 < 0.8,"0.8","1"))))))
mersenne_freqs = as.data.frame(with(mersenne_twister_class, table(cat2)))


set.seed(1234)
v1=runif(3000, min = 0, max = 1)
summary(v1)
#Work with the data as a dataframe.
taula_v1=data.frame(x1=v1)

#Definition of the intervals, categories to be used.
taula_v1_cat=transform(taula_v1, cat1 = ifelse(x1 < 0,"0",
                                               ifelse(x1 < 0.2,"0.2",
                                                      ifelse(x1 < 0.4,"0.4",
                                                             ifelse(x1 < 0.6,"0.6",
                                                                    ifelse(x1 < 0.8,"0.8","1"))))))


taula_freq_v1=as.data.frame(with(taula_v1_cat, table(cat1)))

taula_freq = rbind(taula_freq_v1$Freq, mersenne_freqs$Freq)

aux = cbind(taula_freq_v1$Freq, mersenne_freqs$Freq)

barplot(aux, beside=T, axisnames = T)

barplot(aux,beside=T)

Test=chisq.test(taula_freq, correct = FALSE)
