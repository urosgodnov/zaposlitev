library(rvest)
library(lubridate)
library(xlsx)
library(XML)
library(dplyr)


dir.create(file.path("podatki"), showWarnings = FALSE)

start<-Sys.time()

url<-"https://si.trenkwalder.com/ponudbe-delovnih-mest"
t<-read_html(url)

#preberem podatke
naziv<-t%>%html_nodes(".search-hit-headline")%>%html_text()
datum<-as.Date(t%>%html_nodes(".search-hit-date-city > span:nth-child(1)")%>%html_text(),"%d.%m.%Y")
kraj<-t%>%html_nodes(".search-hit-date-city > span:nth-child(2)")%>%html_text()
description<-as.character(t%>%html_nodes(".search-hit")%>%html_text()%>%gsub("\n","",.)%>%gsub("\r"," ",.))


#Združitev podatkov
podatki<-data.frame(naziv,datum,kraj,description)
podatki<-na.omit(podatki)

#dodam še konstatne
final<-podatki%>%mutate(stran="si.trenkwalder.com",podjetje="Trenkwalder",
                        naziv=gsub("\n","",naziv),
                        podrobno=trimws(description),
                        nedolocen=ifelse(grepl("nedoločen",podrobno),"Da",
                                         ifelse(grepl("\\bdoločen\\b",podrobno),"Ne","Ni podatka")),
                        opis=trimws(description))%>%select(stran,naziv,podjetje,datum,kraj,opis,podrobno,nedolocen)

save(final, file="./podatki/Trenkwalder.Rda")

write.xlsx(final, file="./podatki/Trenkwalder.xlsx",sheetName="Podatki")

konec<-Sys.time()-start

print(paste("Preteklo je ",as.character(konec),sep=""))
