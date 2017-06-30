library(rvest)
library(lubridate)
library(xlsx)
library(xml2)
library(XML)
library(dplyr)

#funkcija za podrobno
details<-function(url) {
  
  podrobno<-url%>% read_html()%>% html_nodes(".vsebina")%>%html_text()
  podrobno<-gsub("\r"," ",podrobno)
  podrobno<-gsub("\t","",podrobno)
  podrobno<-gsub("\n","",podrobno)
  podrobno<-paste(podrobno,collapse="")
  return(podrobno)
}

dir.create(file.path("podatki"), showWarnings = FALSE)

start<-Sys.time()


  url<-"http://www.zaposlitev.net/feed/"

  xmlfile <- xmlTreeParse(url, encoding="UTF-8")
  
  data<-xmlToDataFrame(xpathApply(xmlRoot(xmlfile), "//item"), stringsAsFactors=FALSE)
  
  data <- data.frame(lapply(data, function(x) {
                     x<-gsub("Ĺľ", "ž", x)
                     x<-gsub("Ĺ˝", "Ž", x)
                     x<-gsub("ÄŤ", "č", x)
                     x<-gsub("Ĺˇ", "š", x)
                     x<-gsub("ÄŚ", "Č", x)
                     x<-gsub("\n", " ", x)
                     x<-gsub("Ĺ[[:blank:]]", "Š", x)
                  }))
  
 
  data<-data%>%mutate(kraj=sub(".*Lokacija\\: *(.*?) *<br.*", "\\1", description))
  Sys.setlocale("LC_TIME", "English")
  data<-data%>%mutate(datum=as.Date(pubDate, "%a, %d %b %Y"))
  
  data<-data%>%mutate(podjetje=sub(".*Delodajalec\\: *(.*?) *<br.*", "\\1", description))
  
  data<-data%>%group_by(link)%>%mutate(podrobno=details(as.character(link)))%>%ungroup()
  
  
  data<-data%>%mutate(opis=sub(".*Opis del in nalog\\ *(.*?) ", "\\1", description),
                      stran="zaposlitev.net", 
                      nedolocen=ifelse(grepl("nedoločen",podrobno,ignore.case=TRUE),"Da",
                                        ifelse(grepl("\\bdoločen\\b",podrobno,ignore.case=TRUE),"Ne","Ni podatka")))
  
  
  
final<-data%>%rename(naziv=title)%>%
  select(stran,naziv,podjetje,datum,kraj,opis,podrobno,nedolocen)

save(final, file="./podatki/ZaposlitevNet.Rda")

write.xlsx(final, file="./podatki/ZaposlitevNet.xlsx",sheetName="Podatki")

konec<-Sys.time()-start

print(paste("Preteklo je ",as.character(konec),sep=""))