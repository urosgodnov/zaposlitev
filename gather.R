library(xlsx)
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

write.table(podatki, file = "Zaposlitev.txt", append = FALSE, quote = TRUE, sep = "|", row.names = FALSE)

write.xlsx(podatki, file="Zaposlitev.xlsx",sheetName="Podatki")