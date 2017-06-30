library(rvest)
library(lubridate)
library(xlsx)

dir.create(file.path("podatki"), showWarnings = FALSE)

start<-Sys.time()

#koliko je max podstrani
url<-"http://www.prostadelovnamesta.si/dela"
max<- url %>% read_html()%>% html_nodes(".wpjb-paginate-links")%>% html_text()

max<-regmatches(max, gregexpr("[[:digit:]]+", max))

max<-as.numeric(max[[1]][[4]])

podatki<-as.list(rep(NA, max*20))

for (i in seq(from=1, to=max, by=1)) {
  
  
  print(i)
  
  if (i==1)
      {url<-"http://www.prostadelovnamesta.si/dela/"} else
      {url<-paste("http://www.prostadelovnamesta.si/dela/page/",as.character(i),"/",sep="")}
  
    
  
  podatkiAll<- url %>% read_html()%>% html_nodes("td")%>%
               html_nodes("a")%>%html_attr("href")
  
  #posamezne pomečem v list
  for (j in 1:length(podatkiAll))  {
    
    try(podatki[(i-1)*20+j]<-podatkiAll[j])
    
  }
  
}
Sys.setlocale("LC_TIME", "English")

#Obdelava posameznih elementov
tidyy<-lapply(podatki,function(x) {
  
  if (!is.na(x)) {
    
    print(x)
    
    t<-x%>%read_html()%>%html_nodes("table")
    
    stran<-"prostadelovnamesta.si"
    podjetje<-t%>%html_node("a")%>%html_text()
    
    naziv<-x%>%read_html()%>%html_nodes(".title")%>%html_text()
    
    description<-t%>%html_text()%>%paste(collapse = " ")
    datum<-sub(".*Datum objave\\n *(.*?) *\\n.*", "\\1", description)
    datum<-as.Date(datum, "%b %d, %Y")  
    nedolocen<-sub(".*Vrsta dela\\n *(.*?) *\\n.*", "\\1", description)
    
    
    kraj<-sub(".*Lokacija\\n *(.*?) *\\n.*", "\\1", description)
    
    
    opis<-description
    
    opis<-ifelse(length(opis)>0,opis,"")
    
    podrobno<-x%>% read_html()%>%html_nodes(".wpjb-job-text")%>%html_text()
    podrobno<-paste(podrobno, collapse = " ")
    
    podrobno<-gsub("\n","",podrobno)
    podrobno<-gsub("\r"," ",podrobno)
    
    return(data.frame(stran,naziv,podjetje,datum,kraj,opis,podrobno,nedolocen, stringsAsFactors = FALSE))
  }
  
  
})

final<-do.call(rbind, tidyy)

save(final, file="./podatki/ProstaDelovnaMesta.Rda")

write.xlsx(final, file="./podatki/ProstaDelovnaMesta.xlsx",sheetName="Podatki")

konec<-Sys.time()-start

print(paste("Preteklo je ",as.character(konec),sep=""))
