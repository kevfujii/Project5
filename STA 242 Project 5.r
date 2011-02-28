### General functions ###

# This function, given an e-mail message, splits the message into two parts: the header and the body.
headerBody = function(txt){
	splitLine = min(which(txt == "")) # assumes that the header and body are split by the first blank line
	list(header = txt[1:(splitLine-1)], body = txt[(splitLine+1):length(txt)])
}

# Returns the row indices containing relevant header information (in progress)
headerLines = function(header){
	from = grep("^From:", header)
	date = grep("^Date:", header)
	subject = grep("^Subject:", header)
	messageID = grep("^Message-ID:", header)
	inReplyTo = grep("^In-Reply-To:", header)
	to = grep("^To:", header)
	
	lines = which(grepl("^[[:alpha:]|-]+:", header)) 
	attrNames = gsub("^([[:alpha:]|-]+): .*", "\\1", header[lines])
	extraToLines = integer(0)
	if(any(attrNames == "To")){
		if(lines[which(attrNames == "To")] != lines[which(attrNames == "To") + 1] - 1){
			extraToLines = seq(lines[which(attrNames == "To")] + 1, lines[which(attrNames == "To") + 1] - 1, by = 1)
		}
	}
	list(from = from, date = date, subject = subject, messageID = messageID, inReplyTo = inReplyTo, to = to, extraToLines = extraToLines)
}
	




#########################Enron##################################################
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

#A function to sort through all emails, make them into strings with paste, and
#extract all the regular expressions.
#Note, this will require cleaning after running this function.
#Note, you must give it the "replacement" argument.
GetRegExp = function(File,RegExp, ...){
  File = paste(File,collapse = "\n")
  WhatWeWant = gsub(RegExp, x = File, ...)
  return(WhatWeWant)
  }


#Showing how the above function works.  Now we are getting all of the things in 
  #between " "
TestEmail =readLines("Enron/maildir/lay-k/family/6")   
TestRegExp = "(?s)[^\"]*(?:(\".*?\")[^\"]*)"
GetRegExp(TestEmail,TestRegExp,perl = TRUE,replacement = "\\1")

#An example for removing all the recipiant emails
GetRegExp(TestEmail,"(?s).*?To:(.*?)\\n[[:alpha:]-]+:.*",perl = TRUE,replacement = "\\1")

################ R Help #######################
#setwd("~/enron/maildir/")
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
# Can we incorporate a way to identify the "Date:" line in the above function?
# Dates for both data sets look like: "Date: Sun, 2 Jan 2011 01:09:00 -0800 (PST)"
# The time zone in parentheses is not always present.


splitEmails = function(txt){ # splits an e-mail text file into individual e-mails
	emailStart = findEmailStart(txt)
	groups = rep(seq_along(emailStart), times=diff(c(emailStart, length(txt) + 1)))
	split(txt, groups)
}

rhTest = splitEmails(rhelp)[1]

# extracts the sender's e-mail address and name from an e-mail
findSender = function(message){ # maybe this can take in a row number as input (corresponding to the From: row)
	lineNumbers = headerLines(headerBody(message)$header)
	fromLine = message[lineNumbers$from] # the 2 might need to be changed for the other data set.
	person = gsub("^From: (.*) at (.*) \\((.*)\\).*", "\\1;\\2;\\3", fromLine)
	personSplit = strsplit(person, ";")[[1]]
	dateLine = message[lineNumbers$date] # suppose for now that date is always 3rd.
	date = strptime(dateLine, "Date: %a, %d %b %Y %T") # include %z at end for time zone
	subjectLine = message[lineNumbers$subject]
	subject = gsub("^Subject: (.*)", "\\1", subjectLine)
	c(paste(personSplit[1], personSplit[2], sep = "@"), personSplit[3], subject, date) 

}





# the senders of the e-mails in the last month are:
senders = sapply(splitEmails(rhelp), function(x) findSender(x))

# Still need to extract time and date, subject, reply (or not), Message-ID of previous mail (if it's a reply), body of message

