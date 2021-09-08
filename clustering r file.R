#Cluster analysis
#install.packages("cluster")

library(tidyverse)
library(cluster)

mydata = read_csv("decathlon_rio2016(Edit).csv")




colnames(mydata) <- c("Athelete","Overall Points", "100m", 
                      "Long Jump","Shot Put","High Jump",
                      "400m","110M Hurdles","Discuss Throw",
                      "Pole Vaulting", "Javelin Throw",
                      "1500m")


#Making tebble dataframe

Athelete = c(mydata$`Athelete`)
Overall_Points = c(mydata$`Overall Points`)
One_Hundred_m = c(mydata$`100m`)
Long_Jump = c(mydata$`Long Jump`)
Shot_Put = c(mydata$`Shot Put`)
High_Jump = c(mydata$`High Jump`)
Four_Hundred_m = c(mydata$`400m`)
One_Hundred_ten_Hurdles = c(mydata$`110M Hurdles`)
Discuss = c(mydata$`Discuss Throw`)
Pole_Vaulting = c(mydata$`Pole Vaulting`)
Javelin_Throw = c(mydata$`Javelin Throw`)

Fifteen_Hundred_m = c(mydata$`1500m`)


Decathlon = tibble(Athelete,Overall_Points,
                   One_Hundred_m,
                   Long_Jump, Shot_Put, High_Jump,
                   Four_Hundred_m, 
                   One_Hundred_ten_Hurdles,
                   Discuss,
                   Pole_Vaulting, Javelin_Throw,
                   Fifteen_Hundred_m)



#Subset of data including athletes and the ten events
De_sub = Decathlon %>%
  select(Athelete,One_Hundred_m,
         Long_Jump, Shot_Put, High_Jump,
         Four_Hundred_m, 
         One_Hundred_ten_Hurdles,
         Discuss,
         Pole_Vaulting, Javelin_Throw,
         Fifteen_Hundred_m) 

#Matrix with just the ten events
Decathlon_matrix = select(De_sub,One_Hundred_m,
                          Long_Jump, Shot_Put, High_Jump,
                          Four_Hundred_m, 
                          One_Hundred_ten_Hurdles,
                          Discuss,
                          Pole_Vaulting, Javelin_Throw,
                          Fifteen_Hundred_m)


#Scaling the data 
sDecathlon = scale(Decathlon_matrix)
sDecathlon

#ac of five different methods and four different distance metrics 

#Distance matrix used with euclidean distance as the 
#distance metric 
D = dist(sDecathlon, method="euclidean")

#Cluster results using five different methods
cluster_results = agnes(D, method='ward')
cluster_results$ac
cluster_results = agnes(D, method='single')
cluster_results$ac
cluster_results = agnes(D, method='complete')
cluster_results$ac
cluster_results = agnes(D, method='average')
cluster_results$ac
cluster_results = agnes(D, method='weighted')
cluster_results$ac

#Distance matrix used with manhattan distance as the 
#distance metric 
D1 = dist(sDecathlon, method="manhattan")

#Cluster results using five different methods
cluster_results = agnes(D1, method='ward')
cluster_results$ac
cluster_results = agnes(D1, method='single')
cluster_results$ac
cluster_results = agnes(D1, method='complete')
cluster_results$ac
cluster_results = agnes(D1, method='average')
cluster_results$ac
cluster_results = agnes(D1, method='weighted')
cluster_results$ac

#Distance matrix used with maximum distance as the 
#distance metric 
D2 = dist(sDecathlon, method="maximum")

#Cluster results using five different methods
cluster_results = agnes(D2, method='ward')
cluster_results$ac
cluster_results = agnes(D2, method='single')
cluster_results$ac
cluster_results = agnes(D2, method='complete')
cluster_results$ac
cluster_results = agnes(D2, method='average')
cluster_results$ac
cluster_results = agnes(D2, method='weighted')
cluster_results$ac

D3 = dist(sDecathlon, method="canberra")

#Cluster results using five different methods
cluster_results = agnes(D3, method='ward')
cluster_results$ac
cluster_results = agnes(D3, method='single')
cluster_results$ac
cluster_results = agnes(D3, method='complete')
cluster_results$ac
cluster_results = agnes(D3, method='average')
cluster_results$ac
cluster_results = agnes(D3, method='weighted')
cluster_results$ac


#highest ac 

D = dist(sDecathlon, method="euclidean")
cluster_results = agnes(D, method='ward')
cluster_results
plot(cluster_results, which.plots=2, label = (Athelete))
cutree(cluster_results, k=6)
rect.hclust(cluster_results, k=6, border=3)

#middle ac 
D1 = dist(sDecathlon, method="manhattan")
cluster_results = agnes(D1, method='weighted')
cluster_results
plot(cluster_results, which.plots=2, label = (Athelete))
cutree(cluster_results, k=6)
rect.hclust(cluster_results, k=6, border=3)

#lowest ac 
D1 = dist(sDecathlon, method="manhattan")
cluster_results = agnes(D1, method='single')
cluster_results
plot(cluster_results, which.plots=2, label = Athelete)
cutree(cluster_results, k=6)
rect.hclust(cluster_results, k=6, border=3)




# ------------------------------------------------------------------------------


#events 


TDM = t(sDecathlon)

TDM

#ac of five different methods and four different distance metrics 

#Distance matrix used with euclidean distance as the 
#distance metric 
D = dist(TDM, method="euclidean")

#Cluster results using five different methods
cluster_results = agnes(D, method='ward')
cluster_results$ac
cluster_results = agnes(D, method='single')
cluster_results$ac
cluster_results = agnes(D, method='complete')
cluster_results$ac
cluster_results = agnes(D, method='average')
cluster_results$ac
cluster_results = agnes(D, method='weighted')
cluster_results$ac

#Distance matrix used with manhattan distance as the 
#distance metric 
D1 = dist(TDM, method="manhattan")

#Cluster results using five different methods
cluster_results = agnes(D1, method='ward')
cluster_results$ac
cluster_results = agnes(D1, method='single')
cluster_results$ac
cluster_results = agnes(D1, method='complete')
cluster_results$ac
cluster_results = agnes(D1, method='average')
cluster_results$ac
cluster_results = agnes(D1, method='weighted')
cluster_results$ac

#Distance matrix used with maximum distance as the 
#distance metric 
D2 = dist(TDM, method="maximum")

#Cluster results using five different methods
cluster_results = agnes(D2, method='ward')
cluster_results$ac
cluster_results = agnes(D2, method='single')
cluster_results$ac
cluster_results = agnes(D2, method='complete')
cluster_results$ac
cluster_results = agnes(D2, method='average')
cluster_results$ac
cluster_results = agnes(D2, method='weighted')
cluster_results$ac

D3 = dist(TDM, method="canberra")

#Cluster results using five different methods
cluster_results = agnes(D3, method='ward')
cluster_results$ac
cluster_results = agnes(D3, method='single')
cluster_results$ac
cluster_results = agnes(D3, method='complete')
cluster_results$ac
cluster_results = agnes(D3, method='average')
cluster_results$ac
cluster_results = agnes(D3, method='weighted')
cluster_results$ac


#highest ac 

D = dist(TDM, method="maximum")
cluster_results = agnes(D, method='ward')
cluster_results
plot(cluster_results, which.plots=2)
cutree(cluster_results, k=3)
rect.hclust(cluster_results, k=3, border=3)

#middle ac 
D1 = dist(TDM, method="manhattan")
cluster_results = agnes(D1, method='average')
cluster_results
plot(cluster_results, which.plots=2)
cutree(cluster_results, k=3)
rect.hclust(cluster_results, k=3, border=3)

#lowest ac 
D1 = dist(TDM, method="manhattan")
cluster_results = agnes(D1, method='single')
cluster_results
plot(cluster_results, which.plots=2)
cutree(cluster_results, k=3)
rect.hclust(cluster_results, k=3, border=3)
