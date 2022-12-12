
# LOADING LIBRARIES

library(data.table)
library(magrittr)
library(sf)
library(tmap)
library(sp)
library(ggpubr)
library(ggplot2)
library(lattice)
library(plotly)


# PALETTE & FUNCTION 

myColors <- c('#d9ef8b','#91cf60','#fee08b','#fc8d59','#d73027','#1a9850')

decade <- function(date){
  year <- data.table::year(date)
  y <-year - year %% 10
  return(y)
}


# LOAD DATA AND SHAPES

dataSSN <- fread("SSNMX_catalogo_19000101_20221126.csv", header = T, skip = 4, sep=",", fill=T)

mxMap <- st_read("dest20gw/dest20gw.shp")

statesNames <- fread("nombres_estados.csv", encoding = "UTF-8",header = T)

skipLast <- grep(pattern = "Fecha y hora local",as.character(unlist(dataSSN[,1])))

dataSSN <- dataSSN[-skipLast:-dim(dataSSN)[1],]

names(dataSSN)<- tolower(names(dataSSN))

names(dataSSN)<- gsub("[[:space:]]", ".", names(dataSSN))


# ADJUSTMENTS TO DATA

dataSSN <- dataSSN %>% 
  .[, state := gsub(".*,\\s*", "\\1", referencia.de.localizacion)] %>%  
  .[, state := gsub("[[:space:]]","", state)] %>% 
  .[, state := fifelse(state=="N", "NL", state)] %>% 
  .[, date  := as.Date(fecha)] %>% 
  .[, intensity := suppressWarnings( as.numeric(magnitud))] %>% 
  .[, intensityRanges := fcase(
    intensity>=0 & intensity<= 2 , "0-2", intensity>=2 & intensity<= 4 , "2-4",
    intensity>=4 & intensity<= 6 , "4-6", intensity>=6 & intensity<= 8 , "6-8",
    intensity>=8 & intensity<= 10 , "8-10", intensity>10, "10 +",
    is.na(intensity), "Unmeasured magnitude")] %>% 
  .[, intensityRanges := factor(intensityRanges, levels = c("0-2", "2-4", "4-6", 
                                                            "6-8", "8-10","10+", 
                                                            "Unmeasured magnitude"))] %>% 
  .[, theDecade := decade(date)] %>% 
  .[, monthDate := as.Date(cut(date, "month"))] %>% 
  .[, weekDate := as.Date(cut(date, "week"))] %>% 
  .[, month := data.table::month(date)] %>% 
  .[, monthName := format(date, "%B")] %>% 
  .[, dayName := format(date, "%A")] %>%
  .[, monthDay := format(as.Date(date), "%m-%d")] %>% 
  .[, year := data.table::year(date)] %>% 
  .[, day := data.table::mday(date)] %>% 
  .[date >= "1900-01-01"]
  
dataSSN <- merge(dataSSN, statesNames, by.x="state", by.y="id.estado")


# SEISMIC ACTIVITY SINCE 1900

dataSSN %>% 
  .[, .(seismicCount = .N), by=date] %>% 
  ggplot(aes(x= date, y= seismicCount)) +
  geom_line(color="darkcyan") +
  xlab("Year") +
  ylab("Earthquakes count") +
  ggtitle("Earthquakes per day reported by the SSN", "Since 1900 to 2022") 


# SEISMIC ACTIVITY SINCE 1900 BY INTENSITY

timeSeries1 <- dataSSN %>% 
  .[, .(seismicCount = .N), by=list(date, intensityRanges)] %>% 
  na.omit %>% 
  ggplot(aes(x= date, y= seismicCount, color=intensityRanges)) +
  geom_line(size=1)+
  scale_color_manual(values= myColors, name="Richter Scale") +
  theme(legend.position="top")+
  ggtitle("Earthquakes per day reported by the SSN", "Magnitude ranges since 1900 to 2022")+
  xlab("Year") +
  ylab("Earthquakes count") 

timeSeries2 <- dataSSN %>% 
  .[, .(seismicCount = .N), by=list(date, intensityRanges)] %>% 
  na.omit %>% 
  ggplot(aes(x= date, y= seismicCount, color=intensityRanges)) +
  geom_line(size=1)+
  scale_color_manual(values= myColors) +
  scale_x_date(breaks = "4 year")+
  theme(legend.position="top", axis.text.x = element_text(angle=90))+
  xlab("Year") +
  ylab("Earthquakes count") +
  facet_wrap(~intensityRanges, scales = "free_y") 

ggarrange(timeSeries1, timeSeries2,
          nrow = 2)


# SEISMIC ACTIVITY SINCE 1900 BY INTENSITY (>6)

dataSSN %>% 
  .[intensity>6] %>% 
  .[, .(seismicCount = .N), by=list(date, intensityRanges)] %>%
  ggplot(aes(x= (date), y= seismicCount)) +
  geom_segment( aes(x=(date), xend=date, y=0, yend=seismicCount, color=intensityRanges)) +
  geom_point(size=3, alpha=0.6, aes(color= intensityRanges))+
  scale_color_manual(values= c("darkcyan", "darkred"), name="Range Magnitude Richter Scale") +
  scale_x_date(breaks = "5 years")+
  scale_y_continuous(breaks = seq(0,2,1))+
  theme(legend.position="top", axis.text.x = element_text(angle=45))+
  ggtitle("Earthquakes reported by the SSN", "Intensity > 6 (Since 1900 to 2022)")+
  xlab("Year") +
  ylab("Earthquakes count") 


# SEISMIC ACTIVITY REPORTED SINCE 1900 (PER MONTH)

dataSSN %>% 
  .[, .(seismicCount = .N), by=list(month, monthName)] %>% 
  .[order(month)] %>% 
  ggplot(aes(x= reorder(monthName, month), y= seismicCount)) +
  geom_bar(stat = "identity") +
  geom_col(aes(fill = seismicCount)) +
  theme(axis.text.x = element_text(angle=45)) +
  geom_label(aes(reorder(monthName, month), y= seismicCount,
                 label=seismicCount) ) +
  xlab("Month") +
  ylab("Earthquakes count")+
  ggtitle("Earthquakes reported by the SSN", "Earthquakes per month (Since 1900 to 2022)")


# SEISMIC ACTIVITY REPORTED SINCE 1900 (PER MONTH AND INTENSITY)

rangesMonth <- dataSSN %>% 
  .[, .(seismicCount = .N), by=list(monthName, month, intensityRanges)] %>% 
  ggplot(aes(x= reorder(monthName, month), y= seismicCount, fill= intensityRanges)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Response", values = myColors) +
  theme(axis.text.x = element_text(angle=45), legend.position="top") +
  xlab("Month") +
  ylab("Earthquakes count")+
  ggtitle("Earthquakes reported by the SSN", "Earthquakes per month (Grouped by intensity since 1900 to 2022)")

rangesMonth


# SEISMIC ACTIVITY REPORTED SINCE 1900 (GROUPED BY INTENSITY >6)

dataSSN %>% 
  .[intensity>6] %>% 
  .[, .(seismicCount = .N), by=list(monthName, month, intensityRanges)] %>% 
  ggplot(aes(x= reorder(monthName, month), y= seismicCount, fill= intensityRanges)) +
  geom_bar(stat = "identity")+
  scale_y_continuous(breaks = seq(0,100,1))+
  theme(axis.text.x = element_text(angle=45), legend.position="top")+
  scale_fill_manual(values= c("darkcyan", "darkred"), name="Range Magnitude Richter Scale")+  
  xlab("Month") +
  ylab("Earthquakes count")+
  geom_label(aes(reorder(monthName, month), y= seismicCount, label=seismicCount) )+
  ggtitle("Earthquakes reported by the SSN", "Earthquakes per month (Intensity > 6 since 1900 to 2022)")


# DAYS THAT AN EARTHQUAKE OF INTENSITY >6 HAS BEEN REPEATED AT LEAST 3 OR MORE TIMES

dataSSN %>% 
  .[intensity>=6] %>%
  .[, .(seismicCount = .N), by=list(month, monthDay)] %>% 
  .[order(month)] %>% 
  .[seismicCount>=3] %>%
  ggplot(aes(x= reorder(monthDay, month), y= seismicCount)) +
  geom_bar(stat = "identity", fill="darkcyan")+
  geom_col(aes(fill = seismicCount)) +
  theme(axis.text.x = element_text(angle=45)) +
  geom_label(aes(reorder(monthDay, month), y= seismicCount, label=seismicCount) ) +
  xlab("Date (month-day)") +
  ylab("Earthquakes count")+
  ggtitle("Earthquakes reported by the SSN", "Dates that have occurred three or more earthquakes on the same day 
and month but different year (Intensity >= 6 since 1900 to 2022)")


# WEEKDAYS THAT AN EARTHQUAKE OF INTENSITY >7 HAS BEEN REPEATED MORE TIMES

dataSSN %>% 
  .[intensity>=7] %>%
  .[order(dayName)] %>% 
  .[, .(seismicCount = .N), by=list(dayName)] %>% 
  ggplot(aes(x= dayName, y= seismicCount)) +
  geom_bar(stat = "identity", fill="darkcyan")+
  geom_col(aes(fill = seismicCount)) +
  theme(axis.text.x = element_text(angle=45)) +
    xlab("Mes") +
  ylab("Conteo Sismos")+
  ggtitle("Earthquakes reported by the SSN", "Weekdays with greater occurrences of earthquakes (Intensity >= 7 since 1900 to 2022)")


# MAP SEISMIC ACTIVITY BY STATE 1 

dataSSN %>% 
  .[, .(seismicCount = .N), by=list(nombreEstado)] %>% 
  merge(., mxMap, by.x="nombreEstado", by.y="NOM_ENT") %>% 
  st_as_sf() %>% 
  ggplot()+
  geom_sf(aes(fill = seismicCount)) + 
  ggtitle("Earthquakes reported by the SSN", "Map of seismic activity in Mexico (Since 1900 to 2022)")


# MAP SEISMIC ACTIVITY BY STATE 2

tmap_style("classic")

statesMap <- dataSSN %>% 
  .[, .(seismicCount = .N), by=list(nombreEstado)] %>% 
  merge(., mxMap, by.x="nombreEstado", by.y="NOM_ENT") %>% 
  st_as_sf() %>% 
  tm_shape() +
  tm_polygons("seismicCount", 
              title = "Number of earthquakes range")+
  tm_layout("Number of earthquakes 
              by State (Since 1900 to 2022)", title.position = c('right', 'top'))

statesMap


# SEISMIC ACTIVITY GROUPED BY STATE 

paretoData <- dataSSN %>% 
  .[, .(seismicCount = .N), by=list(nombreEstado)]  %>% 
  .[, total := sum(seismicCount)] %>% 
  .[order(seismicCount, decreasing = T)] %>% 
  .[, accumulatedSum := cumsum(seismicCount)] %>% 
  .[, percentage := seismicCount/total] %>% 
  .[, accumulatedPercentage := accumulatedSum/total]

listPercentage <- 0:100

statesBars <- ggplot(data= data.frame(paretoData), aes(x=nombreEstado)) +
  geom_bar(aes(x=reorder(nombreEstado, -seismicCount), y=seismicCount), fill='darkcyan', stat="identity") + 
  scale_y_continuous(limits = c(0, (max(paretoData$seismicCount))+15000 ))+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())+
  ylab("Earthquakes count")+
  geom_text(aes(reorder(nombreEstado, -seismicCount), y= seismicCount, label=seismicCount), vjust=-1, angle=45, hjust=0)+
  ggtitle("Earthquakes reported by the SSN", "Earthquakes grouped by State (Since 1900 to 2022)")

statesPercentage <- ggplot(data= data.frame(paretoData)) +
  geom_bar(aes(x=reorder(nombreEstado, -seismicCount), y=percentage), fill='darkcyan', stat="identity") + 
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1,0.1), limits=c(0,0.7))+
  xlab("State") +
  ylab("Earthquakes count")+
  geom_text(aes(x= reorder(nombreEstado, -seismicCount), y = percentage,
                label=paste0(round(percentage*100, 3),"%"), vjust=0, angle=45, hjust=0))+
  ggtitle(" ", "Percentage of earthquakes by State (Since 1900 to 2022)")

ggarrange(statesBars, statesPercentage, nrow = 2)


# MAP TOP EARTHQUAKES >= 7.5

topEarthquakes <- dataSSN %>%  
  .[intensity>=7.5]

ggplot(data = mxMap) +
  geom_sf()+
  geom_point(data= topEarthquakes, aes(x= longitud, y= latitud, size= intensity), color="darkcyan")+
  scale_color_manual( name="Magnitud escala de Richter")+
  theme(axis.text = element_blank(), axis.title = element_blank(), legend.position="top")+
  ggrepel::geom_text_repel(data= topEarthquakes, aes(x= longitud, y= latitud, label= intensity), color="darkred")+
  ggtitle("Earthquakes reported by the SSN", "Map of strongest earthquakes locations (Intensity >= 7.5 since 1900 to 2022)")
