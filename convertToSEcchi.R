#calculate the relationship between secchi depth and turbidity

library(discretewq)
library(tidyverse)

WQ = wq(Sources = c("20mm", "Baystudy", "DJFMP", "DOP", "EDSM", "EMP", "FMWT", 
                    "NCRO", "SDO", "SKT", "SLS", "STN", "Suisun",
                    "USBR", "USGS_CAWSC", "USGS_SFBS", "YBFMP"),
        Start_year = 2000, End_year = 2022)
WQ2 = filter(WQ, !is.na(Secchi)&!is.na(TurbidityNTU) |!is.na(Secchi)&!is.na(TurbidityFNU)) %>%
  mutate(Turbidity = case_when(!is.na(TurbidityNTU) ~ TurbidityNTU,
                               TRUE ~ TurbidityFNU)) %>%
  filter(Latitude > 37.8, Latitude <38.1, Longitude > -121.75, Longitude < -121.3, Secchi != 200)

#ggplot(WQ2, aes(x = Secchi, y = Turbidity)) + geom_point()

ggplot(WQ2, aes(x = log(Secchi), y = log(TurbidityNTU))) + geom_point()+
  ylab("Log of Turbidity")+ xlab("Log of Secchi Depth") + geom_smooth(method = "lm")


convert = lm(log(Secchi) ~ log(Turbidity), data = WQ2)
summary(convert)

TurbToSecchi = function(Turbidity) {
  df = data.frame(Turbidity = Turbidity)
  secchi = exp(predict(convert, newdata = df))
  return(secchi)
}

TurbToSecchi(5)

save(convert, TurbToSecchi, WQ2,  file = "TurbToSecchi.RData")
