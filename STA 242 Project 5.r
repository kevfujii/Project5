#Reads in the email and creates a list of lists, the first holds the persons
  #name and the second holds all the emails they sent.
PeopleNames =list.files("enron/maildir/",full.names=TRUE)
Email= lapply(1:length(PeopleNames), function(i){
  ByPerson =list.files(PeopleNames[i],full.names = TRUE,recursive = TRUE)
  lapply(1:length(ByPerson), function(j){
      readLines(ByPerson[j])
    })
  })

#A small test email, from the first person on the list, and the second email 
  #listed.
LittleEmail = Email[[1]][[2]]  
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



################ R Help #######################

setwd("~/RHelp/")
#rhelp = lapply(list.files(), function(x) readLines(gzfile(x)))
rhelp1 = readLines(gzfile(list.files()[1])) # first month of RHelp e-mails
rhelp = readLines(gzfile(list.files()[length(list.files())])) # last month of RHelp e-mails

findEmailStart = function(txt){ # all e-mails seem to start with the same two lines
	a = grep("^From .* at ", txt)  # 1st line starts with "From"
	b = grep("^From: .* at ", txt) # 2nd line starts with "From:"
	c = sapply(a, function(x) any(b == x + 1)) # finds all lines beginning with "From" followed immediately by a line starting with "From:"
	a[c]
}


splitEmails = function(txt){ # splits an e-mail text file into individual e-mails
	emailStart = findEmailStart(txt)
	groups = rep(seq_along(emailStart), times=diff(c(emailStart, length(txt) + 1)))
	split(txt, groups)
}

rhTest = splitEmails(rhelp)[1]

findSender = function(message){ # extracts the sender's e-mail address and name from an e-mail
	fromLine = message[2] # the 2 might need to be changed for the other data set.
	person = gsub("^From: (.*) at (.*) \\((.*)\\).*", "\\1;\\2;\\3", fromLine)
	personSplit = strsplit(person, ";")[[1]]
	c(paste(personSplit[1], personSplit[2], sep = "@"), personSplit[3]) 
}



# the senders of the e-mails in the last month are:
senders = sapply(splitEmails(rhelp), function(x) findSender(x))

# Still need to extract time and date, subject, reply (or not), Message-ID of previous mail (if it's a reply), body of message

