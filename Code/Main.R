library(tidyverse)
library(lubridate)
library(ggthemes)
library(broom)
library(ggrepel)
library(xtable)

# GET LOAD DATA
load1 <- readr::read_csv("../Data/load_1.csv") %>%
  mutate(DATEHOUR=ymd_hm(DATEHOUR))
load2 <- readr::read_csv("../Data/load_2.csv") %>%
  mutate(DATEHOUR=ymd_hm(DATEHOUR))
load2 <- readr::read_csv("../Data/load_3.csv") %>%
  mutate(DATEHOUR=ymd_hm(DATEHOUR))
load <- bind_rows(load1,load2,load3)
rm(load1,load2,load3)

# GET WEATHER DATA
weather_coast <- readr::read_csv("../Data/weather_coast.csv") 
weather_east <- readr::read_csv("../Data/weather_east.csv") 
weather_fwest <- readr::read_csv("../Data/weather_fwest.csv") 
weather_ncent <- readr::read_csv("../Data/weather_ncent.csv") %>% mutate(DATEHOUR=ymd_hm(DATEHOUR))
weather_north <- readr::read_csv("../Data/weather_north.csv") 
weather_scent <- readr::read_csv("../Data/weather_scent.csv") 
weather_south <- readr::read_csv("../Data/weather_south.csv") 
weather_west <- readr::read_csv("../Data/weather_west.csv") 
weather <- weather_coast %>%
  bind_rows(weather_east,weather_fwest,weather_ncent,weather_north,weather_scent,weather_south,weather_west) %>%
  mutate(`HourlyDryBulbTemperature`=(`HourlyDryBulbTemperature`-32)/1.8)  #convert F to C
rm(weather_coast,weather_east,weather_fwest,weather_ncent,weather_north,weather_scent,weather_south,weather_west)

#define previous large load shed events to exclude
outages <- readxl::read_xlsx("../Data/outages.xlsx")

# COMBINE TO CREATE LOAD+WEATHER PANEL
data <- full_join(load,weather,by=c("DATEHOUR","LOCATION")) %>%
  drop_na() %>%
  left_join(outages,by="DATEHOUR") %>%
  mutate(Outage=replace_na(Outage,0))

min(data$HourlyDryBulbTemperature)
max(data$HourlyDryBulbTemperature)

range <- data %>%
  filter(year(DATEHOUR)<2021,Outage==0)
min(range$HourlyDryBulbTemperature)
max(range$HourlyDryBulbTemperature)

# CREATE WORKING DATAFRAME WITH FIXED EFFECTS AND TEMPBIN, EXCLUDE 2021
df <- data %>%
  mutate(year=year(DATEHOUR),
         month=month(DATEHOUR),
         wday=wday(DATEHOUR),
         HE=hour(DATEHOUR),
         HD=ifelse(HourlyDryBulbTemperature<65,65-HourlyDryBulbTemperature,0),
         CD=ifelse(HourlyDryBulbTemperature>65,HourlyDryBulbTemperature-65,0),
         tempbin=cut(HourlyDryBulbTemperature,breaks=seq(-16,47,3)),
         log.load=log(LOAD)) %>%
  filter(year<2021,Outage==0)
levels(df$tempbin)
df <- within(df, tempbin <- relevel(tempbin, ref = 12))

counts <- df %>%
  group_by(LOCATION,tempbin) %>%
  summarise(n=n()) %>%
  mutate(outlier = ifelse(n<10,1,0))

df <- df %>%
  left_join(counts, by=c("LOCATION","tempbin")) %>%
  filter(outlier==0)

TRF <- function(loc,tmin=2002,tmax=2020){
  m1 <- lm(log.load ~ factor(year) + factor(month) + factor(HE)*factor(month) + factor(wday)*factor(HE) + factor(tempbin),df %>% filter(LOCATION==loc,year>=tmin,year<=tmax))
  results <- tidy(m1) %>%
    filter(grepl("tempbin",term))
  results$temp<-gsub(".*,|]*", "", results$term)
  results$temp<-as.integer(results$temp)-1.5
  results <- results %>%
    mutate(#norm = estimate-estimate[temp==65],
           #norm.upper=norm+1.96*std.error,
           #norm.lower=norm-1.96*std.error,
           estimate.upper=estimate+1.96*std.error,
           estimate.lower=estimate-1.96*std.error,
           period=paste(tmin,tmax,sep="-"),
           location=loc)
  return(results)
}

# Get TRF coefficients for each location
plist <- unique(df$LOCATION)
datalist = list()
for(p in plist){
  dat <- TRF(p)
  datalist[[p]] <- dat
}
TRF.df <- do.call(rbind,datalist)

set_zero_18 <- tibble(
  location=plist,
  temp=18.5,
  estimate=0,
  estimate.upper=0,
  estimate.lower=0
)

TRF.df <- bind_rows(TRF.df,set_zero_18)
TRF.df$pdiff <- exp(TRF.df$estimate)-1

ggplot(TRF.df,
       aes(temp,pdiff,ymin=estimate.lower,ymax=estimate.upper,color=fct_reorder2(location,temp,estimate),fill=fct_reorder2(location,temp,estimate)))+
  #geom_ribbon(alpha=.2,color=NA)+
  geom_line(size=1.5)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 18.5,linetype="dashed")+
  scale_x_continuous(breaks=scales::pretty_breaks(n=10))+
  scale_y_continuous(labels=scales::percent)+
  #scale_color_colorblind(name="ERCOT Weather Zone")+
  scale_color_viridis_d(name="ERCOT Weather Zone")+
  theme_light(14)+
  labs(x="Temperature (°C)",
       y="% difference in demand relative to 18.5°C")
ggsave("../Figures/TRF_all_years.png",width=10,height=5,dpi=600)

ggplot(TRF.df,
       aes(temp,pdiff,ymin=estimate.lower,ymax=estimate.upper,))+
  #geom_ribbon(alpha=.2,color=NA)+
  facet_wrap(~location,nrow=2)+
  geom_line(size=1.5)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 18.5,linetype="dashed")+
  scale_x_continuous(breaks=scales::pretty_breaks(n=5))+
  scale_y_continuous(labels=scales::percent)+
  #scale_color_colorblind(name="ERCOT Weather Zone")+
  #scale_color_viridis_d(name="ERCOT Weather Zone")+
  theme_light(14)+
  theme(strip.text = element_text(face="bold"))+
  labs(x="Temperature (°C)",
       y="% difference in demand relative to 18.5°C")
ggsave("../Figures/TRF_all_years_facet.png",width=10,height=6,dpi=600)



# 4 year blocks
plist <- unique(df$LOCATION)
ylist <- c(2002,2007,2012,2017)
datalist = list()
i=0
for(p in plist){
  for(y in ylist){
    i=i+1
    dat <- TRF(p,y,y+4)
    datalist[[i]] <- dat
  }
}
TRF.master <- do.call(rbind,datalist)
TRF.master$pdiff <- exp(TRF.master$estimate)-1
TRF.master$period[TRF.master$period=="2017-2021"] <- "2017-2020"

ggplot(TRF.master,
       aes(temp,pdiff,ymin=estimate.lower,ymax=estimate.upper,color=period,fill=period))+
  facet_wrap(~location,ncol = 2)+
  geom_line(size=1)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 18.5,linetype="dashed")+
  scale_x_continuous(breaks=scales::pretty_breaks(n=5))+
  scale_y_continuous(labels=scales::percent)+
  scale_color_viridis_d(name="Period of estimation")+
  theme_light()+
  theme(strip.text = element_text(size=rel(1.2)),
        legend.position = "top")+
  labs(x="Temperature (°C)",
       y="% difference in demand relative to 18.5°C")
ggsave("../Figures/TRF_Master.png",width=7,height=10,dpi=600)
#ggsave("../Figures/TRF_Master.png",width=10,height=5,dpi=600)

# tempbin plot below 10C
ggplot(TRF.master %>% filter(temp<=10,period %in% c("2002-2006","2017-2020"),
                             location %in% c("North Central (DFW)","Coast (Houston)","South Central (Austin)")),
       aes(temp,estimate,ymin=estimate.lower,ymax=estimate.upper,color=period,fill=period))+
  facet_wrap(~location,nrow = 1)+
  geom_line(size=1.5)+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks=scales::pretty_breaks(n=5),limits=c(-15,10))+
  scale_y_continuous(breaks=seq(0,.8,.2))+
  scale_color_viridis_d(name="Period of estimation")+
  theme_light()+
  theme(legend.position = "top",
        strip.text = element_text(size=rel(1.2),face="bold"))+
  labs(x="Temperature (°C)",
       y="Difference in demand relative to 18.5°C (log points)")
ggsave("../Figures/TRF_Master_cold.png",width=10,height=4)


####################################################
#PREDICTION off linear fit of cold bin estimates

trend <- TRF.master %>%
  filter(temp<=10,period %in% c("2002-2006","2017-2020"))
m.trend <- lm(estimate ~ location*temp*period,trend)
df.trend <- tidy(m.trend)

B3 <- df.trend %>%
  filter(str_detect(term,"location"),
         str_detect(term,"period"),
         !(str_detect(term,"temp"))) %>%
  mutate(location=substr(term,9,str_locate(term,":")-1)) %>%
  select(location,B3=estimate,term)

B4 <- df.trend %>%
  filter(str_detect(term,"location"),
         str_detect(term,"period"),
         str_detect(term,"temp")) %>%
  mutate(location=substr(term,9,str_locate(term,":")-1)) %>%
  select(location,B4=estimate,term)

B3.coast <- tibble(location="Coast (Houston)",B3=0)
B4.coast <- tibble(location="Coast (Houston)",B4=0)

B3 <- bind_rows(B3,B3.coast)
B4 <- bind_rows(B4,B4.coast)

# CREATE PREDICTION DATAFRAME WITH FIXED EFFECTS for OUTAGE EVENT FEB 15-19 2021
forecast <- readxl::read_xlsx("../Data/forecast_load.xlsx") %>%
  pivot_longer(-DATEHOUR,names_to="location",values_to="ForecastLoad") %>%
  left_join(weather,by=c("DATEHOUR","location"="LOCATION")) %>%
  rename(temp="HourlyDryBulbTemperature") %>%
  mutate(B1 = coef(summary(m.trend))["period2017-2020","Estimate"],
         B2 = coef(summary(m.trend))["temp:period2017-2020","Estimate"]) %>%
  left_join(B3,by="location") %>%
  left_join(B4,by="location") %>%
  mutate(log.diff = B1 + B2*temp + B3 + B4*temp,
         load.diff=ForecastLoad*(exp(log.diff)-1))

forecast.summary <- forecast %>%
  group_by(location) %>%
  summarise(load.diff.mean=mean(load.diff,na.rm=T),
            ForecastLoad.mean=mean(ForecastLoad,na.rm=T),
            load.diff.sum=sum(load.diff,na.rm=T),
            ForecastLoad.sum=sum(ForecastLoad,na.rm=T),
            pdiff=load.diff.sum/ForecastLoad.sum)

forecast.total <- forecast.summary %>%
  summarise(load.diff.sum=sum(load.diff.sum,na.rm=T),
            ForecastLoad.sum=sum(ForecastLoad.sum,na.rm=T),
            pdiff=load.diff.sum/ForecastLoad.sum)
forecast.total[1,3] #total percent change was 5.5%

forecast.hourly <- forecast %>%
  group_by(DATEHOUR) %>%
  summarise(load.diff.sum=sum(load.diff,na.rm=T),
            `ERCOT Forecast Load`=sum(ForecastLoad,na.rm=T),
            `Counterfactual load with 2002-2006 temperature sensitivity`=`ERCOT Forecast Load`-load.diff.sum) %>%
  select(1,3,4) %>%
  pivot_longer(-1) %>%
  mutate(name=factor(name,levels=c("ERCOT Forecast Load","Counterfactual load with 2002-2006 temperature sensitivity")))

forecast.hourly.diff <- forecast.hourly %>%
  pivot_wider(names_from = name, values_from = value) %>%
  mutate(diff=`Counterfactual load with 2002-2006 temperature sensitivity`-`ERCOT Forecast Load`) %>%
  arrange(diff)
forecast.hourly.diff[1,4]
forecast.hourly.diff[1,3]
forecast.hourly.diff[1,2]
# sort on diff, largest diff was 5300MW on Feb 16, 8am, against a based of 70GW

ggplot(forecast.hourly, aes(DATEHOUR,value,linetype=name))+
  geom_line(size=1)+
  scale_linetype(name="")+
  scale_y_continuous(labels=scales::comma)+
  scale_x_datetime(date_breaks = "1 day",date_labels="%b-%d")+
  theme_light()+
  theme(legend.position = c(.75,.85),
        legend.background = element_blank())+
  labs(y="Hourly load (MW)",x="")
ggsave("../Figures/load_forecast.png",width=8,height=4,dpi=600)


forecast.hourly.zonal <- forecast %>%
  group_by(DATEHOUR,location) %>%
  summarise(load.diff.sum=sum(load.diff,na.rm=T),
            ForecastLoad=sum(ForecastLoad,na.rm=T),
            OldForecastLoad=ForecastLoad-load.diff.sum) %>%
  select(DATEHOUR,location,ForecastLoad,OldForecastLoad) %>%
  pivot_longer(c("ForecastLoad","OldForecastLoad"))

ggplot(forecast.hourly.zonal, aes(DATEHOUR,value,color=name))+
  facet_wrap(~location)+
  geom_line()+
  scale_color_viridis_d()+
  theme_light()



# TRF cold temps with forecast load
forecast.plot <- forecast %>%
  filter(location %in% c("Coast (Houston)","North Central (DFW)","South Central (Austin)"))

ggplot(TRF.master %>% filter(temp<=10,period %in% c("2002-2006","2017-2020"),
                             location %in% c("Coast (Houston)")),
       aes(temp,estimate))+
  facet_wrap(~location,nrow = 1)+
  geom_line(aes(color=period),size=1.5)+
  geom_abline(slope=df.trend$estimate[df.trend$term=="temp"],
              intercept=df.trend$estimate[df.trend$term=="(Intercept)"],
              linetype="dotted",color=scales::viridis_pal()(1))+
  geom_abline(slope=df.trend$estimate[df.trend$term=="temp"]+df.trend$estimate[df.trend$term=="temp:period2017-2020"],
              intercept=df.trend$estimate[df.trend$term=="(Intercept)"]+df.trend$estimate[df.trend$term=="period2017-2020"],
              linetype="dotted",color="gold")+
  annotate("segment",x=-10,xend=-10,y=.45,yend=.6,arrow = arrow(),size=1)+
  annotate("text",x=-9.5,y=.52,hjust=0,label="Increase in load\ndue to heightened temperature\nsensitivity")+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks=scales::pretty_breaks(n=5),limits=c(-15,10))+
  scale_y_continuous(breaks=seq(0,.8,.2),limits=c(0,.8))+
  scale_color_viridis_d(name="Period of estimation")+
  theme_light(14)+
  theme(legend.position = "top",
        strip.text = element_text(size=rel(1.2),face="bold"))+
  labs(x="Temperature (°C)",
    y="Difference in demand relative to 18.5°C (log points)")
ggsave("../Figures/TRF_coast_cold.png",width=10,height=6,dpi=600)






# B1 <- coef(summary(m.trend))["period2017-2021","Estimate"]
# B2 <- coef(summary(m.trend))["temp:period2017-2021","Estimate"]
# B3 <- ifelse(loc=="Coast (Houston)",0,coef(summary(m.trend))[paste0("location",loc,":period2017-2021"),"Estimate"])
# B4 <- ifelse(loc=="Coast (Houston)",0,coef(summary(m.trend))[paste0("location",loc,":temp:period2017-2021"),"Estimate"])
# df.predict.loc <- df.predict %>%
#   filter(LOCATION==loc) %>%
#   mutate(log.diff = B1 + B2*HourlyDryBulbTemperature + B3 + B4*HourlyDryBulbTemperature)
#   
# 
# 
# df.predict <- data %>%
#   mutate(year=2020,  #force year to be 2020 to be in factors estimated in main regression, differenced out with old vs new
#          month=month(DATEHOUR),
#          wday=wday(DATEHOUR),
#          HE=hour(DATEHOUR),
#          #HD=ifelse(HourlyDryBulbTemperature<65,65-HourlyDryBulbTemperature,0),
#          #CD=ifelse(HourlyDryBulbTemperature>65,HourlyDryBulbTemperature-65,0),
#          #tempbin=cut(HourlyDryBulbTemperature,breaks=seq(3,113,5)),
#          log.load=log(LOAD)) %>%
#   filter(DATEHOUR>as.POSIXct("2021-02-14 23:00",tz="UTC"),DATEHOUR<=as.POSIXct("2021-02-19 23:00",tz="UTC"))
# 
# 
# 
# trend <- TRF.master %>%
#   filter(temp<=40,period %in% c("2002-2006","2017-2021"))
# 
# m.trend <- lm(estimate ~ location*temp*period,trend)
# df.trend <- tidy(m.trend)
# 
# loc <- "Coast (Houston)"
# B1 <- coef(summary(m.trend))["period2017-2021","Estimate"]
# B2 <- coef(summary(m.trend))["temp:period2017-2021","Estimate"]
# B3 <- ifelse(loc=="Coast (Houston)",0,coef(summary(m.trend))[paste0("location",loc,":period2017-2021"),"Estimate"])
# B4 <- ifelse(loc=="Coast (Houston)",0,coef(summary(m.trend))[paste0("location",loc,":temp:period2017-2021"),"Estimate"])
# df.predict.loc <- df.predict %>%
#   filter(LOCATION==loc) %>%
#   mutate(log.diff = B1 + B2*HourlyDryBulbTemperature + B3 + B4*HourlyDryBulbTemperature) %>%
#   summarise(LOCATION=first(LOCATION),
#             log.diff=mean(log.diff))
# 
# 
# predict.diff <- function(loc){
#   B1 <- coef(summary(m.trend))["period2017-2021","Estimate"]
#   B2 <- coef(summary(m.trend))["temp:period2017-2021","Estimate"]
#   B3 <- ifelse(loc=="Coast (Houston)",0,coef(summary(m.trend))[paste0("location",loc,":period2017-2021"),"Estimate"])
#   B4 <- ifelse(loc=="Coast (Houston)",0,coef(summary(m.trend))[paste0("location",loc,":temp:period2017-2021"),"Estimate"])
#   df.predict.loc <- df.predict %>%
#     filter(LOCATION==loc) %>%
#     mutate(log.diff = B1 + B2*HourlyDryBulbTemperature + B3 + B4*HourlyDryBulbTemperature) %>%
#     summarise(LOCATION=first(LOCATION),
#               log.diff=mean(log.diff))
# }
# 
# plist <- unique(TRF.master$location)
# datalist=list()
# for(p in plist){
#   dat <- predict.diff(p)
#   datalist[[p]] <- dat
# }
# predict.df <- do.call(rbind,datalist)
# 
# 
# # AVERAGE Load size
# df.avg <- df %>%
#   group_by(LOCATION) %>%
#   summarise(LOAD=mean(LOAD))
# 
# ggplot(df.avg, aes(fct_reorder(LOCATION,-LOAD),LOAD))+
#   geom_col(fill="royalblue")+
#   geom_hline(yintercept = 0)+
#   theme_hc()+
#   theme(axis.ticks=element_blank())+
#   labs(x="",y="Average hourly load (MW)",
#        title="Average load by ERCOT Weather Zone")
# 
# # log.load diff x avg load
# df.chg <- left_join(predict.df,df.avg,by="LOCATION") %>%
#   mutate(new.load = LOAD*exp(log.diff),
#          load.chg = new.load-LOAD)
# 
# sum(df.chg$load.chg)
# sum(df.chg$LOAD)
# sum(df.chg$load.chg)/sum(df.chg$LOAD)  #6.4% higher due to heightened sensitivity to cold temps
#        

