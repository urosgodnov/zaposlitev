library(rvest)
library(lubridate)
library(xlsx)

dir.create(file.path("podatki"), showWarnings = FALSE)

start<-Sys.time()
#koliko je max podstrani
url<-"https://www.mojazaposlitev.si/prosta-delovna-mesta?keywords=&podrocja=&regije=&izobrazba=&cas_objave=&delodajalec=&_action=I%C5%A1%C4%8Di+med+prostimi+deli"
max<- url %>% read_html()%>% html_nodes(".pagination")%>% html_text()

max<-regmatches(max, gregexpr("[[:digit:]]+", max))

max<-max(as.numeric(unlist(max)))


podatki<-as.list(rep(NA, max*10))

for (i in seq(from=0, to=(max*10), by=10)) {

print(i)
  
url<-paste("https://www.mojazaposlitev.si/prosta-delovna-mesta/?1=1&o=",as.character(i),"&_action=I%C5%A1%C4%8Di+med+prostimi+deli#jobsAds",sep="")


podatkiAll<- url %>% read_html()%>% html_nodes(".desc")

#posamezne pomečem v list
for (j in 1:length(podatkiAll))  {
  
  podatki[i+j]<-podatkiAll[j]
  
}

}


#Obdelava posameznih elementov
tidyy<-lapply(podatki,function(x) {
 
  
  
  if (!is.na(x)) {
  t<-x%>%html_nodes("span")%>%html_text("class")
  
  stran<-"mojazaposlitev.si"
  naziv<-x%>%html_node("a")%>%html_attr("title")
  podjetje<-t[1]
  datum<-as.Date(t[2]%>%gsub(" Objavljeno: ","",.), "%d.%m.%Y")
  kraj<-t[3]%>%gsub(" Kraj dela: ","",.)
  opis<-x%>%html_nodes("div")%>%html_text("class")
  
  #dodatni opis
  urlpodrobno<-x%>%html_node("a")%>%html_attr("href")
  urlpodrobno<-paste("https://www.mojazaposlitev.si/",urlpodrobno,sep="")
  
  podrobno<-urlpodrobno %>% read_html()%>%html_nodes(".desc")%>%html_text()
  podrobno<-paste(podrobno, collapse = " ")
  
  
  
  nedolocen<-ifelse(grepl("nedoločen",podrobno),"Da",
                    ifelse(grepl("\\bdoločen\\b",podrobno),"Ne","Ni podatka"))

  
  return(data.frame(stran,naziv,podjetje,datum,kraj,opis,podrobno,nedolocen, stringsAsFactors = FALSE))
  }
  
  
})

final<-do.call(rbind, tidyy)

save(final, file="./podatki/MojaZaposlitev.Rda")

write.xlsx(final, file="./podatki/MojaZaposlitev.xlsx",sheetName="Podatki")

konec<-Sys.time()-start

print(paste("Preteklo je ",as.character(konec),sep=""))