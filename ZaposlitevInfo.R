library(rvest)
library(lubridate)
library(xlsx)

dir.create(file.path("podatki"), showWarnings = FALSE)

start<-Sys.time()

#koliko je max podstrani
url<-"https://www.zaposlitev.info/prostadelovnamesta/"
max<- url %>% read_html()%>% html_nodes(".wpjb-paginate-links")%>% html_text()

max<-regmatches(max, gregexpr("[[:digit:]]+", max))

max<-as.numeric(max[[1]][[4]])


podatki<-as.list(rep(NA, max*10))

for (i in seq(from=1, to=max, by=1)) {
  
  
  print(i)

  if (i==1)
  {url<-"https://www.zaposlitev.info/prostadelovnamesta/?query=&location=&type%5B%5D=5&type%5B%5D=38&type%5B%5D=6&type%5B%5D=4&type%5B%5D=37&type%5B%5D=36"} else
  {url<-paste("https://www.zaposlitev.info/prostadelovnamesta/page/",as.character(i),"/","?query&location&type%5B0%5D=5&type%5B1%5D=38&type%5B2%5D=6&type%5B3%5D=4&type%5B4%5D=37&type%5B5%5D=36",sep="")}
  
  
  podatkiAll<- url %>% read_html()%>% html_nodes(".wpjb-job-list")%>% 
    html_nodes(".wpjb-grid-row")%>%html_node("a")%>%html_attr("href")
  
  #posamezne pomečem v list
  for (j in 1:length(podatkiAll))  {
    
    try(podatki[(i-1)*10+j]<-podatkiAll[j])
    
  }
  
}
#Sys.setlocale("LC_TIME", "English")

#Obdelava posameznih elementov
tidyy<-lapply(podatki,function(x) {
  
  if (!is.na(x)) {
    
    print(x)
    
    
    t<-x%>%read_html()%>%html_node("#content")
    
    stran<-"zaposlitev.info"
    podjetje<-t%>%html_nodes(".wpjb-top-header-title")%>%html_text()
    
    naziv<-t%>%html_nodes(".fusion-post-title")%>%html_text()
    
    description<-t%>%html_nodes(".wpjb-grid-row")%>%html_text()
    description<-description[1:4]
    datum<-t%>%html_nodes(".wpjb-top-header-subtitle")%>%html_text()
    datum<-gsub("Objavljeno:","",datum)
    datum<-as.Date(datum, "%d/%m/%Y")  

    
    
    kraj<-gsub("Lokacija ","",description[1])
    
    
    opis<-description
    
    opis<-ifelse(length(opis)>0,opis,"")
    
    podrobno<-t%>%html_nodes(".wpjb-text-box")%>%html_text()

    nedolocen<-ifelse(grepl("nedoločen",podrobno),"Da",
                      ifelse(grepl("\\bdoločen\\b",podrobno),"Ne","Ni podatka"))
    
    return(data.frame(stran,naziv,podjetje,datum,kraj,opis,podrobno,nedolocen, stringsAsFactors = FALSE))
  }
  
  
})

final<-do.call(rbind, tidyy)

save(final, file="./podatki/ZaposlitevInfo.Rda")

write.xlsx(final, file="./podatki/ZaposlitevInfo.xlsx",sheetName="Podatki")

konec<-Sys.time()-start

print(paste("Preteklo je ",as.character(konec),sep=""))
