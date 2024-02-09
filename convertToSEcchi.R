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


###################################
#origional version (whole estuary)


WQx = wq(Sources = c("20mm", "Baystudy", "DJFMP", "DOP", "EDSM", "EMP", "FMWT", 
                    "NCRO", "SDO", "SKT", "SLS", "STN", "Suisun",
                    "USBR", "USGS_CAWSC", "USGS_SFBS", "YBFMP"),
        Start_year = 2000, End_year = 2022)
WQ2x = filter(WQx, !is.na(Secchi)&!is.na(TurbidityNTU) |!is.na(Secchi)&!is.na(TurbidityFNU)) %>%
  mutate(Turbidity = case_when(!is.na(TurbidityNTU) ~ TurbidityNTU,
                               TRUE ~ TurbidityFNU))

#ggplot(WQ2, aes(x = Secchi, y = Turbidity)) + geom_point()

ggplot(WQ2x, aes(x = log(Secchi), y = log(TurbidityNTU))) + geom_point()+
  ylab("Log of Turbidity")+ xlab("Log of Secchi Depth") + geom_smooth(method = "lm")


convertx = lm(log(Secchi) ~ log(Turbidity), data = WQ2x)
summary(convertx)

#########################################################
#maybe it's better to pull the sonde turbidity data and align it
#with the discrete secchi
library(deltamapr)
library(sf)
stations = read_excel("../continuous stations.xlsx") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
stations2 = st_drop_geometry(P_Stations) %>%
  filter(Source %in% c("20mm", "DJFMP",
                                             "EMP", "FMWT", "SKT",
                                             "SLS", "STN")) %>%
  group_by(Station, Source) %>%
  summarize(Latitude = mean(Latitude), Longitude = mean(Longitude)) %>%
  filter(Latitude > 37.8, Latitude <38.1, Longitude > -121.75, Longitude < -121.3) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), 
           crs = 4326, remove=FALSE)

#write.csv(stations2, "Stations2.csv")

ggplot()+
  geom_sf(data = WW_Delta, fill = "lightblue", color = "grey")+
  geom_sf(data = stations)+
  geom_sf(data = stations2, aes(color = Source))+
  geom_sf_text(data = filter(stations2,Source == "EMP"), aes(label = Station))+
  geom_sf_text(data = stations, aes(label = StationCode), nudge_x = .01, nudge_y = .01, size = 2)+
  coord_sf(ylim = c(37.8, 38.1), xlim = c(-121.75, -121.3))+
  theme_bw()

stations2 = read_csv("stations2.csv")

WQx2 = full_join(WQx, select(stations2, -Latitude, -Longitude))

library(cder)
contwaterquality = cdec_query(stations$StationCode, c(27,221), duration = "E",
                                     start.date = as.Date("2010-01-01"), end.date = today())

contwq = mutate(contwaterquality, Date = date(ObsDate)) %>%
  group_by(StationID, Date) %>%
  summarize(Turbidity = mean(Value, na.rm =T))

WQx2a = left_join(WQx2, contwq, by = c("ContStation"= "StationID", "Date")) %>%
  filter(!is.na(Secchi), !is.na(Turbidity.y), Turbidity.y !=0, Secchi != 200) %>%
  rename(Turbidity = Turbidity.y)

ggplot(WQx2a, aes(x = log(Secchi), y = log(Turbidity)))+ geom_point()+ geom_smooth(method = "lm")

convert3 = lm(log(Secchi) ~ log(Turbidity), data = WQx2a)
summary(convert3)


TurbToSecchi = function(Turbidity) {
  df = data.frame(Turbidity = Turbidity)
  secchi = exp(predict(convert3, newdata = df))
  return(secchi)
}

TurbToSecchi(5)

save(convert3, TurbToSecchi, WQx2a,  file = "TurbToSecchi.RData")
