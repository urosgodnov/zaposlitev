library(xlsx)
library(dplyr)
library(Hmisc)
datoteke<- list.files(path="./podatki/", pattern="*.Rda", full.names=FALSE)
datoteke<-gsub(".Rda","",datoteke)


podatki_s=lapply(datoteke,function(x) {
  filenm=paste("./podatki/",as.character(x),".Rda",sep="")

  if (file.exists(filenm))
  {
    print(filenm)
    load(filenm)
    return(final)
    
  }
})

podatki=as.data.frame(try(do.call(rbind,podatki_s)))

podatki<-podatki%>%mutate(pogodba=ifelse(nedolocen=="Da","Nedoločen čas",ifelse(nedolocen=="Ne","Določen čas",nedolocen)))%>%
         select(-nedolocen,-opis,-podrobno)%>%mutate(
                                                      pogodba=trimws(pogodba),
                                                      kraj=capitalize(tolower(trimws(kraj))),
                                                      naziv=trimws(naziv),
                                                      podjetje=trimws(podjetje),
                                                      stran=gsub("www.","",stran)
         )


write.table(podatki, file = "Zaposlitev.txt", append = FALSE, quote = TRUE, sep = "|", row.names = FALSE)

write.xlsx(podatki, file="Zaposlitev.xlsx",sheetName="Podatki")