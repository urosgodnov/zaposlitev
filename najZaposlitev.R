library(rvest)
library(lubridate)
library(xlsx)
library(RSelenium)

dir.create(file.path("podatki"), showWarnings = FALSE)

start<-Sys.time()

#koliko je max podstrani
url<-"https://najzaposlitev.si/oglasi/"

driver<- rsDriver()
remDr <- driver[["client"]]



# Open the browser webpage
remDr$open()
remDr$navigate(url)


#
# vse poklikam
for (i in (1:5)) {
  try(loadmorebutton <- remDr$findElement(using = 'css selector', ".load_more_jobs"))
  try(loadmorebutton$clickElement())

# Wait for few seconds to get new results loaded

Sys.sleep(1)
}

page_source<-remDr$getPageSource()

links<- read_html(page_source[[1]])%>%html_nodes(".single-work , .work-desc-inner")%>%
  html_nodes("a")%>%html_attr("href")

links<-links[!links=="#"]
#zaprem rm
remDr$close()

#klasično grem skozi loop
podatki<-as.list(links)

counter <- 0

tidyy<-lapply(podatki,function(x) {
  
  if (!is.na(x)) {
    
    counter <<- counter + 1
    
    print(counter)
    
    x<-podatki[[3]]
    
    t<-x%>%read_html()
    
    stran<-"najZaposlitev.si"
    podjetje<-t%>%html_nodes(".padding-right strong")%>%
    html_text()
    
    podjetje<-podjetje[1]
    
    description<-t%>%html_nodes(".job-overview")%>%html_text()
    
    description<-gsub("\t","",description)
    description<-gsub("\n","",description)
    description<-gsub("\r"," ",description)
    
    naziv<-sub(".*Naziv delovnega mesta\\: *(.*?) *\\s\\s.*", "\\1", description)
    

    datum<-sub(".*Oglas poteče\\: *(.*?) *\\s\\s.*", "\\1", description)
    
    datum<-gsub("Objavljeno:","",datum)
    datum<-as.Date(datum, "%d.%m.%Y")  
    
    
    
    kraj<-sub(".*Lokacija\\: *(.*?) *\\s\\s.*", "\\1", description)
    
    
    opis<-description
    
    opis<-ifelse(length(opis)>0,opis,"")
    
    podrobno<-t%>%html_nodes(".padding-right")%>%html_text()
    
    nedolocen<-ifelse(grepl("nedoločen",podrobno),"Da",
                      ifelse(grepl("\\bdoločen\\b",podrobno),"Ne","Ni podatka"))
    
    return(data.frame(stran,naziv,podjetje,datum,kraj,opis,podrobno,nedolocen, stringsAsFactors = FALSE))
  }
  
  
})

final<-do.call(rbind, tidyy)

save(final, file="./podatki/najZaposlitev.Rda")

write.xlsx(final, file="./podatki/najZaposlitev.xlsx",sheetName="Podatki")

konec<-Sys.time()-start

print(paste("Preteklo je ",as.character(konec),sep=""))
