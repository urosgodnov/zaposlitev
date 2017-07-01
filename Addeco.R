library(rvest)
library(lubridate)
library(xlsx)
library(XML)
library(RSelenium)

dir.create(file.path("podatki"), showWarnings = FALSE)

start<-Sys.time()

#koliko je max podstrani
url<-"http://www.adecco.si/sl/delovnamesta/?pagetype=iskalci"
max<- url %>% read_html()%>% html_nodes(".pagination")%>% html_text()

max<-regmatches(max, gregexpr("[[:digit:]]+", max))

max<-as.numeric(max[[1]][[4]])


naziv<-data.frame(naziv=NA)
description<-data.frame(description=NA)
datum<-data.frame(datum=as.Date(NA))
lokacija<-data.frame(lokacija=NA)

driver<- rsDriver()
remDr <- driver[["client"]]



# Open the browser webpage
remDr$open()


#Peljem se po podstraneh
for (j in seq(from=1, to=1, by=1)) {


  if (j==1)
  {url<-"http://www.adecco.si/sl/delovnamesta/"} else
  {url<-paste(url<-"http://www.adecco.si/sl/delovnamesta/page/",as.character(j),"/",sep="")}
  
#navigiram na spletno stran 
  remDr$navigate(url)
  

#poklikam vse + in predvidevam, da jih je 20
for (i in (1:20)) {

  
  try(loadmorebutton <- remDr$findElement(using = "xpath", paste("(//div[@class='col-xs-1 plusminus'])[",as.character(i),"]",sep="")))
  try(loadmorebutton$clickElement())
  
  # Wait for few seconds to get new results loaded

  Sys.sleep(1)
}
  page_source<-remDr$getPageSource()
  t<-read_html(page_source[[1]])
  
  nazivdf<-data.frame(naziv=t%>%html_nodes(".name-1")%>%html_text())
  naziv<-rbind(naziv,nazivdf)
  
  descdf<-data.frame(description=t%>%html_nodes(".work-desc-inner")%>%html_text())
  description<-rbind(description,descdf)
  
  datumdf<-data.frame(datum=t%>%html_nodes(".col-xs-11")%>%
                        html_text()%>%gsub("\n","",.)%>%
                        sub(".*\\t *(.*?) *\\|.*", "\\1", .)%>%gsub("\t","",.)%>%as.Date(.,"%d.%m.%Y"))
  datum<-rbind(datum,datumdf)
  
  lokacijadf<- data.frame(lokacija=t%>%html_nodes(".col-xs-11")%>%
    html_text()%>%gsub("\n","",.)%>%sub(".*\\| *(.*?) *\\t.*", "\\1", .)%>%gsub("\t","",.))
  lokacija<-rbind(lokacija,lokacijadf)
  
  
} #peljem se po podstraneh

try(remDr$close())


#Združitev podatkov
podatki<-cbind(naziv,datum,lokacija,description)
podatki<-na.omit(podatki)

#dodam še konstatne
final<-podatki%>%mutate(stran="adecco.si",podjetje="ADECCO H.R. d.o.o.",
                        kraj=lokacija,podrobno=description,
  nedolocen=ifelse(grepl("nedoločen",podrobno),"Da",
                    ifelse(grepl("\\bdoločen\\b",podrobno),"Ne","Ni podatka")),
  opis=description)%>%select(stran,naziv,podjetje,datum,kraj,opis,podrobno,nedolocen)
  
save(final, file="./podatki/Adecco.Rda")

write.xlsx(final, file="./podatki/Adecco.xlsx",sheetName="Podatki")

konec<-Sys.time()-start

print(paste("Preteklo je ",as.character(konec),sep=""))
