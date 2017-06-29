library(rvest)
library(lubridate)
library(xlsx)

dir.create(file.path("podatki"), showWarnings = FALSE)

podatki<-as.list(rep(NA, 1615))

for (i in seq(from=1, to=81, by=1)) {
  
  print(i)
  
  url<-paste("https://www.mojedelo.com/prosta-delovna-mesta/vsa-podrocja?p=",as.character(i),sep="")
  
  
  podatkiAll<- url %>% read_html()%>% html_nodes(".overlayOnHover1")
  
  #posamezne pomečem v list
  for (j in 1:length(podatkiAll))  {
    
    try(podatki[(i-1)*20+j]<-podatkiAll[j])
    
  }
  
}


#Obdelava posameznih elementov
tidyy<-lapply(podatki,function(x) {
  
  print(x)
  

  if (!is.na(x)) {
  
  t<-x%>%html_nodes(".detail")%>%html_text()
  
  
  naziv<-x%>%html_node(".title")%>%html_text()
  
  podjetje<-t[2]
  
  datum<-as.Date(ifelse(t[1]=="Danes",format(Sys.time(), "%d.%m.%Y"),ifelse(t[1]=="Včeraj",format(Sys.time()-1, "%d.%m.%Y"),t[1])),"%d.%m.%Y")
  
  kraj<-t[3]
  
  opis<-x%>%html_nodes("p")%>%html_text("premiumDescription")
  
  opis<-ifelse(length(opis)>0,opis,"")
  
  return(data.frame(naziv,podjetje,datum,kraj,opis, stringsAsFactors = FALSE))
  }
  
  
})


final<-do.call(rbind, tidyy)

save(final, file="./podatki/MojeDelo.Rda")

write.xlsx(final, file="./podatki/MojeDelo.xlsx",sheetName="Podatki")
