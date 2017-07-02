library(rvest)
library(lubridate)
library(xlsx)
library(XML)
library(dplyr)


dir.create(file.path("podatki"), showWarnings = FALSE)

start<-Sys.time()

url<-"http://www.hill-int.si/sl_SI/trg-dela.html"
t<-read_html(url)%>%html_nodes("article")

link<-as.list(paste("http://www.hill-int.si",t%>%html_nodes("h1")%>%html_nodes("a")%>%html_attr("href"),sep=""))


tidyy<-lapply(link,function(x) {
  
  
  if (!is.na(x)) {
    
    print(x)
    
    t<-x%>%read_html()
    
    stran<-"hill.si"
    naziv<-t%>%html_nodes("h1")%>%html_text()
    podjetje<-"Hill"
    
    description<-t%>%html_nodes(".entityDescription")%>%html_text()%>%gsub("\t","",.)%>%
      gsub("\n","",.)%>%gsub("\r"," ",.)
    
    opis<-t%>%html_nodes(".entityDetails")%>%html_text()%>%gsub("\t","",.)%>%
      gsub("\n"," ",.)%>%gsub("\r"," ",.)%>%gsub(" ","",.)
    
    
    datum<-as.Date(sub(".*Prijavezbiramodo\\:(.*?)", "\\1", opis), "%d.%m.%Y")
    kraj<-sub(".*[Delovno mesto|Sedež delovnega mesta] je v (.*?)[.].*", "\\1", description)
   
    
    #dodatni opis
    
    podrobno<-description
    
    nedolocen<-ifelse(grepl("nedoločen",podrobno),"Da",
                      ifelse(grepl("\\bdoločen\\b",podrobno),"Ne","Ni podatka"))
    
    
    return(data.frame(stran,naziv,podjetje,datum,kraj,opis,podrobno,nedolocen, stringsAsFactors = FALSE))
  }
  
  
})

final<-do.call(rbind, tidyy)



save(final, file="./podatki/Hill.Rda")

write.xlsx(final, file="./podatki/Hill.xlsx",sheetName="Podatki")

konec<-Sys.time()-start

print(paste("Preteklo je ",as.character(konec),sep=""))