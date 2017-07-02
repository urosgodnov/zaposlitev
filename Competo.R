library(rvest)
library(lubridate)
library(xlsx)
library(XML)
library(dplyr)


dir.create(file.path("podatki"), showWarnings = FALSE)

start<-Sys.time()

url<-"http://www.competo.si/za-kandidate/prosta-delovna-mesta/"
t<-read_html(url)%>%html_nodes(".delovno_mesto")

stran<-"competo.si"
podjetje<-"Competo"
naziv<-t%>%html_nodes("a")%>%html_attr("title")
datum<-as.Date(gsub("Rok za prijavo:","",t%>%html_nodes(".date")%>%html_text()),"%d.%m.%Y")
kraj<-""
nedolocen<-"Ni podatka"
opis<-t%>%html_nodes(".text")%>%html_text()%>%gsub("[\n|\t]","",.)%>%trimws()
podrobno<-""

final<-data.frame(stran,naziv,podjetje,datum,kraj,opis,podrobno,nedolocen, stringsAsFactors = FALSE)

save(final, file="./podatki/Competo.Rda")

write.xlsx(final, file="./podatki/Competo.xlsx",sheetName="Podatki")

konec<-Sys.time()-start

print(paste("Preteklo je ",as.character(konec),sep=""))