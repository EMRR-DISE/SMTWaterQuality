#calculate the relationship between secchi depth and turbidity

library(discretewq)
library(tidyverse)
library(deltamapr)
library(sf)
library(readxl)
library(mgcv)


stations = read_excel("continuous stations.xlsx") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

WQ = wq(Sources = c("20mm", "Baystudy", "DJFMP", "DOP", "EDSM", "EMP", "FMWT", 
                    "NCRO", "SDO", "SKT", "SLS", "STN", "Suisun",
                    "USBR", "USGS_CAWSC", "USGS_SFBS", "YBFMP"),
        Start_year = 2000, End_year = 2024)
WQ2 = filter(WQ, !is.na(Secchi)&!is.na(TurbidityNTU) |!is.na(Secchi)&!is.na(TurbidityFNU)) %>%
  mutate(Turbidity = case_when(!is.na(TurbidityNTU) ~ TurbidityNTU,
                               TRUE ~ TurbidityFNU)) %>%
  filter(Latitude > 37.8, Latitude <38.1, Longitude > -121.75, Longitude < -121.3, Secchi != 200, Secchi !=0,
         Turbidity !=0)

#ggplot(WQ2, aes(x = Secchi, y = Turbidity)) + geom_point()

ggplot(WQ2, aes(x = log(Secchi), y = log(TurbidityNTU))) + geom_point()+
  ylab("Log of Turbidity")+ xlab("Log of Secchi Depth") + geom_smooth(method = "lm")


convert = lm(log(Secchi) ~ log(Turbidity), data = WQ2)
summary(convert)


###################################
#origional version (whole estuary)


WQx = wq(Sources = c("20mm", "Baystudy", "DJFMP", "DOP", "EDSM", "EMP", "FMWT", 
                    "NCRO", "SDO", "SKT", "SLS", "STN", "Suisun",
                    "USBR", "USGS_CAWSC", "USGS_SFBS", "YBFMP"),
        Start_year = 2000, End_year = 2024)
WQ2x = filter(WQx, !is.na(Secchi)&!is.na(TurbidityNTU) |!is.na(Secchi)&!is.na(TurbidityFNU)) %>%
  mutate(Turbidity = case_when(!is.na(TurbidityNTU) ~ TurbidityNTU,
                               TRUE ~ TurbidityFNU)) %>%
  filter(Turbidity !=0)

#ggplot(WQ2, aes(x = Secchi, y = Turbidity)) + geom_point()

ggplot(WQ2x, aes(x = log(Secchi), y = log(TurbidityNTU))) + geom_point()+
  ylab("Log of Turbidity")+ xlab("Log of Secchi Depth") + geom_smooth(method = "lm")


convertx = lm(log(Secchi) ~ log(Turbidity), data = WQ2x)
summary(convertx)

#########################################################
#maybe it's better to pull the sonde turbidity data and align it
#with the discrete secchi

# stations2 = st_drop_geometry(P_Stations) %>%
#   filter(Source %in% c("20mm", "DJFMP",
#                                              "EMP", "FMWT", "SKT",
#                                              "SLS", "STN")) %>%
#   group_by(Station, Source) %>%
#   summarize(Latitude = mean(Latitude), Longitude = mean(Longitude)) %>%
#   filter(Latitude > 37.8, Latitude <38.1, Longitude > -121.75, Longitude < -121.3) %>%
#   st_as_sf(coords = c("Longitude", "Latitude"), 
#            crs = 4326, remove=FALSE)

#write.csv(stations2, "Stations2.csv")

# ggplot()+
#   geom_sf(data = WW_Delta, fill = "lightblue", color = "grey")+
#   geom_sf(data = stations)+
#   geom_sf(data = stations2, aes(color = Source))+
#   geom_sf_text(data = filter(stations2,Source == "EMP"), aes(label = Station))+
#   geom_sf_text(data = stations, aes(label = StationCode), nudge_x = .01, nudge_y = .01, size = 2)+
#   coord_sf(ylim = c(37.8, 38.1), xlim = c(-121.75, -121.3))+
#   theme_bw()

stations2 = read_csv("stations2.csv")

WQx2 = full_join(WQ2x, select(stations2, -Latitude, -Longitude))

library(cder)
contwaterquality = cdec_query(unique(stations2$ContStation), c(27,221), duration = "E",
                                     start.date = as.Date("2010-01-01"), end.date = today())

contwq = mutate(contwaterquality, Date = date(ObsDate)) %>%
  filter(Value >0) %>%
  group_by(StationID, Date) %>%
  summarize(TurbiditySonde = mean(Value, na.rm =T)) 

WQx2a = left_join(WQx2, contwq, by = c("ContStation"= "StationID", "Date")) %>%
  filter(!is.na(Secchi), !is.na(TurbiditySonde), TurbiditySonde !=0, Secchi != 200,
         abs(Turbidity-TurbiditySonde) <50) %>%
  select(StationID, ContStation, Date, Secchi, TurbiditySonde, Turbidity)


ggplot(WQx2a, aes(x = log(Secchi), y = log(TurbiditySonde)))+ geom_point(aes(color = ContStation))+ 
  geom_smooth(method = "lm")
#OK, OBI and FRK might be the problem children

ggplot(WQx2a, aes(x = Turbidity, y = TurbiditySonde))+ geom_point()+ geom_smooth(method = "lm")
ggplot(WQx2a, aes(x = Turbidity, y = TurbiditySonde-Turbidity))+ geom_point()+ geom_smooth(method = "lm")
#OK, so some of the values there are clearly different things going on with the sonde than the boat, can we remove those?

convert3 = lm(log(Secchi) ~ log(TurbiditySonde), data = filter(WQx2a, ContStation != "OBI", ContStation != "FRK"))
summary(convert3)

#what is the actual versus predicted secchi?

TurbToSecchi = function(Turbidity) {
  df = data.frame(TurbiditySonde = Turbidity)
  secchi = exp(predict(convert3, newdata = df))
  return(secchi)
}

TurbToSecchi(5)
mypal = c("red", "blue", "springgreen3", "darkorange", "purple2", "darkred", "tan", "skyblue", "yellow","salmon","black")
save(convert3, TurbToSecchi, mypal, WQx2a,  file = "docs/TurbToSecchi.RData")

#what is the actual versus predicted secchi?
WQx2a = mutate(WQx2a, SecchiPredicted = TurbToSecchi(TurbiditySonde)) %>%
  filter(SecchiPredicted <600)

ggplot(WQx2a, aes(x = Secchi, y = SecchiPredicted))+ geom_point(aes(color = ContStation))+ 
  scale_color_manual(values = mypal, name = "Continuous\nStation")+
  geom_abline(slope =1, intercept =0, size =1, linetype = 2)+
  theme_bw()+
  annotate("text", x = 300, y = 310, angle = 30, label = "1:1 line")
  
#what if I used a GAM to predict secchi dpeth instead?
  
tsgam = gam(Secchi~s(TurbiditySonde), data = WQx2a, family = nb)
summary(tsgam)
gam.check(tsgam)
plot(tsgam)

#how about the average turbidiyt and average secchi by date?

WQave = group_by(WQx2a, Date) %>%
  summarize(Secchi = mean(Secchi), TurbiditySonde = mean(TurbiditySonde), Turbidity = mean(Turbidity))

ggplot(WQave, aes(x = log(Secchi), y = log(TurbiditySonde)))+geom_point()+ geom_smooth(method = "lm")

convert4 = lm(log(Secchi) ~ log(TurbiditySonde), data = WQave)
summary(convert4)
#well that's worse. Much worse