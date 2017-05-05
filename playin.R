test2 <- read.csv('./590_final/test2.csv')
geo_mean_append(test2, "Sample", "E", "Ct", "GeoMeanE", "GeoSD")

geo_sd(test2, "Sample", "E", "Ct", "GeoMeanE", "GeoSD")

df_geomean <- geo_mean_append(test2, "Sample", "E", "Ct", "GeoMeanE")

geo_sd(df_geomean, "Sample", "E", "Ct", "GeoMeanE", "GeoSD" )

test_geomean <- c(1.9, 2.25, 2.22)

geo_mean(test_geomean)

sd(test_geomean)

geo_mean_append(test2)

test3 <- read.csv("./590_final/test3.csv")
library(dplyr)
data1 <- test3 %>% 
  group_by(gene, sample) %>%
 summarise(geo.mean = geo_mean(E), 
            geo.sd = geo_sd(E),
            Mean.Ct = mean(ct), 
            SD.Ct = sd(ct)
 )
head(data1)

data2 <- 
  ungroup(data1) %>%
  group_by(gene) %>%
  summarise(d.Ct = Mean.Ct[sample == "Y4I"] - Mean.Ct[sample == "pgaR"])
head(data2)

data2.5 <- data1 %>% 
  filter(sample == "Y4I") %>%
  left_join(data2, by = "gene") %>%
  mutate(rq.geomean = geo.mean[sample == "Y4I"]^d.Ct[sample == "Y4I"], #doesn't return number
             rq.sd = rq.geomean*sqrt((d.Ct*geo.sd/geo.mean)^2 + (log(geo.mean)*SD.Ct)^2)
             )

head(data2.5)


data3 <- 
  ungroup(data2.5) %>%
  summarise(nf = ((rq.geomean[gene == "alaS"]*rq.geomean[gene == "map"]*rq.geomean[gene == "rpoC"])^(1/3)),
            nf.sd = nf*sqrt((rq.sd[gene == "rpoC"]/(3*rq.geomean[gene == "rpoC"]))^2 + 
              ((rq.sd[gene == "map"])/(3*rq.geomean[gene == "map"]))^2 + 
              ((rq.sd[gene == "alaS"])/(3*rq.geomean[gene == "alaS"]))^2),
            nrq = rq.geomean[gene == "igiD"]/nf,
            nrq.sd = nrq*sqrt((nf.sd/nf)^2 + (rq.sd[gene == "igiD"]/rq.geomean[gene == "igiD"])^2)
              )
head(data3)

#test6 <- 
  #ungroup(test4) %>%
  #group_by(sample)%>%
#mutate(rq.geomean = test4$geo.mean[sample == "Y4I"]^test5$d.Ct)
#head(test6)
  
  #mutate(d.Ct = Mean.Ct[sample == "Y4I"] - Mean.Ct[sample == "pgaR"]) #%>%
  #mutate(RQ = geo.mean[sample == "Y4I"]^d.Ct[sample == "pgaR"]) %>%
  
  #mutate (SD.RQ = geo.mean*sqrt[])
  #mutate(SD.RQ = sd(geo.mean[sample == "Y4I"]^d.Ct[sample == "pgaR"]))

