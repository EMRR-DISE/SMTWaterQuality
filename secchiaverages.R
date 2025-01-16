#pull 20mm secchi depth for 2009-2023 and average by survey
#just the 12 stations for OMR management

library(tidyverse)
library(discretewq)

twentymil = wq(Sources = "20mm", Start_year = 2009)
#hmm... this doesn't have survey numbers. Lets try the edi dataset

twmil = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.535.4&entityid=671a005532337c078b2bbf95c1df6e2d")
twmil2 = select(twmil, Year, SampleDate, "Station" = "qry_AMC_EDI_01.Station.Station", Secchi, , "Survey" = "qry_AMC_EDI_01.Survey.Survey")
twmil3 = filter(twmil2, Year >2008, Station %in% c(809, 812, 815, 901, 902, 906, 910, 912, 914, 915, 918, 919)) %>%
  mutate(Month = month(dmy(SampleDate)), Date = dmy(SampleDate), DOY = yday(Date))

ggplot(twmil3, aes(x = DOY, y = Secchi, color = as.factor(Station)))+ 
  geom_point()+geom_smooth()+
  facet_wrap(~Year)


ggplot(twmil3, aes(x = DOY, y = Secchi))+ 
  geom_point()+geom_smooth()+
  facet_wrap(~Year)


ggplot(twmil3, aes(x = DOY, y = Secchi))+ 
  geom_point()+geom_smooth()+
  facet_wrap(~Station)


TWMsum = twmil3 %>%
  mutate(Above100 = case_when(Secchi >100 ~ 1,
                              TRUE ~ 0)) %>% 
  group_by(Survey) %>%
  summarize(meansecchi = mean(Secchi, na.rm =T), sdsecchi = sd(Secchi, na.rm =T), 
            mediansecchi = median(Secchi, na.rm =T),N = n(), Percentabove100 = sum(Above100)/N, )
write.csv(TWMsum, "Secchisummary2.csv")

##############################################################
#are the turbidity data also that variable?

turblt = cdec_query(stations$StationCode, c(27,221), duration = "E",
                  start.date = as.Date("2015-01-01"), end.date = today())

turbltspring = filter(turblt, month(DateTime) %in% c(3,4,5,6,7)) %>%
  mutate(DOY = yday(DateTime), Date = date(DateTime), Month = month(DateTime), Year = year(DateTime))

turbdaily = filter(turbltspring, Value < 600) %>%
  group_by(Date, DOY, Month, Year, StationID) %>%
  summarize(meanturb = mean(Value), Year = as.factor(Year))

ggplot(turbdaily, aes(x = DOY, y = log(meanturb))) + geom_point(aes(color = StationID))+ geom_smooth() +
  facet_wrap(~Year)#+coord_cartesian(ylim = c(0,50))

ggplot(turbdaily, aes(x = DOY, y = meanturb)) + geom_point(aes(color = Year))+ geom_smooth() +
  facet_wrap(~StationID)+coord_cartesian(ylim = c(0,50))

#######################
#convert it to secchi

ltsecchi = mutate(turbdaily, Secchi = TurbToSecchi(meanturb))

ggplot(ltsecchi, aes(x = DOY, y = Secchi)) + geom_point(aes(color = StationID))+ geom_smooth() +
  facet_wrap(~Year)

ggplot(ltsecchi, aes(x = DOY, y = Secchi)) + geom_point(aes(color = StationID))+ geom_smooth() +
  facet_wrap(~Year) + coord_cartesian(ylim = c(0,200))
