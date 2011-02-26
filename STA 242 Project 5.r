PeopleNames =list.files("enron/maildir/",full.names=TRUE)
Email= lapply(1:length(PeopleNames), function(i){
  ByPerson =list.files(PeopleNames[i],full.names = TRUE,recursive = TRUE)
  lapply(1:length(ByPerson), function(j){
      readLines(ByPerson[j])
    })
  })
   
#Looks for the first email after the From: and finds the line, then replaces it
#With blank text all around it.
grep("^From:[[:space:]]*[[:alpha:]\\.]+@[[:alpha:]\\.]+",LittleEmail[[1]][[2]])
gsub("^From:[[:space:]]*([[:alpha:]\\.]+@[[:alpha:]\\.]+)", "\\1", LittleEmail[[1]][[2]],perl = TRUE)

#A small test email which I know has multiple recipients on multiple lines. 
#The collapse gives the new line charcter, and paste collapses the character
#vector into one long string
TestEmail = paste(readLines("Enron/maildir/lay-k/family/6"),collapse= "\n")

#Take everything AFTER To:, up until the first (stuff): you see.
ToHeader = gsub("(?s).*?To:(.*?)\\n[[:alpha:]-]+:.*", "\\1", TestEmail, perl=TRUE)
#Get rid of all the damn spaces
gsub("[[:space:]]", "", ToHeader, perl=T)
#Split by commas, and we have all the recipient emails.
strsplit( gsub("[[:space:]]", "", ToHeader, perl=T), ",")