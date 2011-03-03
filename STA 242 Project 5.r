library(plyr)

### General functions ###

# This function, given an e-mail message, splits the message into two parts: the header and the body.
headerBody = function(txt){
	splitLine = min(which(txt == "")) # assumes that the header and body are split by the first blank line
	list(header = txt[1:(splitLine-1)], body = txt[(splitLine+1):length(txt)])
}

# Returns the row indices containing relevant header information (maybe unnecessary?)
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
	extraReferenceLines = integer(0)
	if(any(attrNames == "References")){
		if(lines[which(attrNames == "References")] != lines[which(attrNames == "References") + 1] - 1){
			extraReferenceLines = seq(lines[which(attrNames == "References")] + 1, lines[which(attrNames == "References") + 1] - 1, by = 1)
		}
	}
	list(from = from, date = date, subject = subject, messageID = messageID, inReplyTo = inReplyTo, to = to, extraToLines = extraToLines, extraReferenceLines = extraReferenceLines)
}





# formats the header so that the To:, cc:, References:, etc. are each on one line.
formatHeader = function(header){
	File = sapply(1:length(header), function(i){
		ifelse(grepl("^[[:alpha:]|-]*:", header[i]), paste("CUT HERE", header[i]), header[i])
		})
	File = paste(File,collapse = " ")
	File = gsub("\t", "", File)
	# find locations of the tags
	toReturn = strsplit(File, "CUT HERE ")[[1]]
	toReturn[toReturn != ""]
}
	
	
			

		
	

# Finds the date in a header
getDate = function(header){
	dateLine = headerLines(header)$date
	date = strptime(header[dateLine], "Date: %a, %d %b %Y %T")
	date
}

# Attempts to format the date (since there are multiple possible formats)
formatDate = function(date){
	toReturn = strptime(date, "%a, %d %b %Y %T")
	#if(toReturn$year %in% (c(97:99, 0:11) - 1900)){
	#	toReturn$year = toReturn$year + 1900 
	#}
	toReturn[which(is.na(toReturn))] = strptime(date[which(is.na(toReturn))], "%d %b %Y %T")
	toReturn[which(is.na(toReturn))] = strptime(date[which(is.na(toReturn))], "%a %b %d %T %Y")
	toReturn[which(is.na(toReturn))] = strptime(date[which(is.na(toReturn))], "%a, %d %b %y %T")
	toReturn[which(is.na(toReturn))] = strptime(date[which(is.na(toReturn))], "%d %b %y %T")
	toReturn[which(is.na(toReturn))] = strptime(date[which(is.na(toReturn))], "%a, %d %b %y %R")
	wrongYears = which(toReturn$year %in% (c(97:99, 0:11) - 1900))
	toReturn$year[wrongYears] = toReturn$year[wrongYears] + 1900
	toReturn
}
	
# Returns a one-observation data.frame of header attributes
#parseHeader = function(header, rows = NULL){
#	if(length(headerLines(header)$extraToLines) != 0){
#		#header = headerBody(TestEmail2)$header
#		to1 = GetRegExp(header,"(?s).*?To:(.*?)\\n[[:alpha:]-]+:.*",perl = TRUE,replacement = "\\1")
#		to2 = strsplit( gsub("[[:space:]]", "", to1, perl=T), ",")[[1]]
#		header[headerLines(header)$to] = paste("To: ", paste(to2, collapse = ", "), collapse = "")
#		header = header[-headerLines(header)$extraToLines]
#	}
#	cat(header, file = "tempfile.txt", sep = "\n")
#	data.frame(read.dcf("tempfile.txt", fields = rows), stringsAsFactors = FALSE)
#}


parseHeader = function(header, rows = NULL){
	formatted = formatHeader(header)
	con = textConnection(formatted)
	frame = data.frame(read.dcf(con, fields = rows), stringsAsFactors = FALSE)
	close(con)
	frame
}

OneNightStand = readLines("enron/maildir/williams-w3/bill/1")
TestEmail2 = readLines("enron/maildir/lay-k/family/6")
TestHeader = headerBody(TestEmail2)$header
   

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
#A function to sort through all emails, make them into strings with paste, and
#extract all the regular expressions.
#Note, this will require cleaning after running this function.
#Note, you must give it the "replacement" argument.
GetRegExp = function(File,RegExp, ...){
  File = paste(File,collapse = "\n")
  WhatWeWant = gsub(RegExp, x = File, ...)
  return(WhatWeWant)
  }

CommaSpaceKiller = function(Vector){
  Result = do.call(c,strsplit( gsub("[[:space:]]", "", Vector, perl=T), ","))
  return(Result)
  }  
  
BindFrom = function(TheFrom,TheOther,name1,name2){
  TheOther = CommaSpaceKiller(TheOther)
  Result = cbind(rep(TheFrom,length(TheOther)),TheOther)
  colnames(Result) = c(name1,name2)
  return(Result)
  }
#Takes a header and returns a three element list, each element is a two column 
#matrix, the first column has the sender email the second column the recipiant email.
GetToFrom = function(header){
  From = GetRegExp(header,"(?s).*?From: (.*?)\\n[[:alpha:]-]+:.*",replacement = "\\1",perl = T)
  To = GetRegExp(header,"(?s).*?To: (.*?)\\n[[:alpha:]-]+:.*",replacement = "\\1",perl = T)
  To = BindFrom(From,To,"Sender","To")
  if( length(grep("Bcc",header)) != 0){
    Cc = GetRegExp(header,"(?s).*?Cc: (.*?)\\n[[:alpha:]-]+:.*",replacement = "\\1",perl = T)
    Cc = BindFrom(From,Cc,"Sender","Cc")
    }else{
      Cc = NULL
      }
  Emails = list(To,Cc)  
  return(Emails)
  }
  
#Creating my n by 2 matrix  
Headers = lapply(1:length(Email),function(i){
  lapply(1:length(Email[[i]]),function(j) {
     headerBody(Email[[i]][[j]])$header
     })
  })
  
ToFromList = lapply(1:length(Headers),function(i){
  lapply(1:length(Headers[[i]]),function(j) {
     GetToFrom(Headers[[i]][[j]])
     })
  })
#A function which takes in a list of lists, and creates a two column matrix 
  #based.  Whichone is for the ToFrom list, or the FromCc list.
Matrify = function(List,WhichOne){
  List = unlist(List,recursive = FALSE)
  List = sapply(1:length(List), function(i){
    List[[i]][[WhichOne]]
    })
  List = do.call(rbind,List)
  Pasted = sapply(1:nrow(List),function(i){
    paste(List[i,],collapse = "::")
    })
  Table = table(Pasted)
  Table = cbind(do.call(rbind,strsplit(rownames(Table),"::")) ,Table)
  rownames(Table) = NULL   
  return(Table)
  } 
#As it turns out, Bcc = Cc exactly 
FromToMat = Matrify(ToFromList,1)
FromCcMat = Matrify(ToFromList,2)

FromToMat = FromToMat[order(as.numeric(FromToMat[,3]),decreasing = FALSE),]
FromCcMat = FromCcMat[order(as.numeric(FromCcMat[,3]),decreasing = FALSE),]

#A function which searches through each folder to find all the different emails
  #the same person sent from.
# PROBLEM:  Folders also contain emails the person RECIEVED. Badness 9000. 
FindAllEmails=function(List){
  AllEmails = sapply(1:length(List),function(i){
  GetRegExp(List[[i]],"(?s).*?From: (.*?)\\n[[:alpha:]-]+:.*",replacement = "\\1",perl = T)
  })
  return(AllEmails)
  }
  
################ R Help #######################
#setwd("~/enron/maildir/")
setwd("~/RHelp/")
#rhelp = lapply(list.files(), function(x) readLines(gzfile(x)))
rhelp1 = readLines(gzfile(list.files()[1])) # first month of RHelp e-mails
rhelp = readLines(gzfile(list.files()[length(list.files())])) # last month of RHelp e-mails
RHelp = sapply(1:length(list.files()), function(x) readLines(gzfile(list.files()[x])))

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
#findSender = function(message){ # maybe this can take in a row number as input (corresponding to the From: row)
#	lineNumbers = headerLines(headerBody(message)$header)
#	fromLine = message[lineNumbers$from] # the 2 might need to be changed for the other data set.
#	person = gsub("^From: (.*) at (.*) \\((.*)\\).*", "\\1;\\2;\\3", fromLine)
#	personSplit = strsplit(person, ";")[[1]]
#	dateLine = message[lineNumbers$date] # suppose for now that date is always 3rd.
#	date = strptime(dateLine, "Date: %a, %d %b %Y %T") # include %z at end for time zone
#	subjectLine = message[lineNumbers$subject]
#	subject = gsub("^Subject: (.*)", "\\1", subjectLine)
#	c(paste(personSplit[1], personSplit[2], sep = "@"), personSplit[3], subject, date) 
#
#}


rhelpHeaders = sapply(splitEmails(rhelp), function(x) headerBody(x[-1])$header)
rhelpBodies = sapply(splitEmails(rhelp), function(x) headerBody(x)$body)
parsedHeaders = lapply(rhelpHeaders, parseHeader)
table(sapply(parsedHeaders, function(x) dim(x)[2]))
headerFrame = do.call(rbind.fill, parsedHeaders)


############ Big data files #########
RHelpHeaders = sapply(1:(length(RHelp)), function(y){
	sapply(splitEmails(RHelp[[y]]), function(x) headerBody(x[-1])$header);
})
RHelpBodies = sapply(1:(length(RHelp)), function(y){
	sapply(splitEmails(RHelp[[y]]), function(x) headerBody(x)$body);
})
parsedRHelpHeaders = lapply(1:length(RHelpHeaders), function(y){
	lapply(RHelpHeaders[[y]], parseHeader)
})

headerList = lapply(1:length(parsedRHelpHeaders), function(y){
	do.call(rbind.fill, parsedRHelpHeaders[[y]])
})

<<<<<<< HEAD
fullRHelpBodies = do.call(c, RHelpBodies)
unlistedRHelpBodies = unlist(RHelpBodies)

#cat(unlistedRHelpBodies, file = "rhelpbodies.txt", sep = "\n")
#con = pipe("egrep -c '[[:alpha:]|.]+\\(' ~/Documents/RHelp/rhelpbodies.txt")
#con = pipe("../../Users/Kevin/rhelpbodies.txt")
#tmp = readLines(con, 100000)

######## STUFF TO FIND FUNCTIONS ###########
functions = gsub("([[:alpha:]|.]+\\()", "FUNCTION HERE! \\1", unlistedRHelpBodies)
functions = strsplit(functions, "FUNCTION")

allfunctions = character(0)
#for(i in 101:length(functions)%/%10000){
j = 0
while(j < 13){
	for(i in 1:100){
		functionstest = sapply(functions[(10000*i-9999):(10000*i)], function(x){
			gsub("^ HERE! ([[:alpha:]|.]+)\\(.*", "\\1", x[which(grepl("^ HERE! ", x))])
			})
		allfunctions = c(allfunctions, unlist(functionstest))
		print(i+100*j)
	}
	functions = functions[-(1:1000000)]
	j = j + 1
}
functionstest = sapply(functions, function(x){
	gsub("^ HERE! ([[:alpha:]|.]+)\\(.*", "\\1", x[which(grepl("^ HERE! ", x))])
	})
allfunctions = c(allfunctions, unlist(functionstest))



functionTable = sort(table(allfunctions), decreasing = TRUE)


####### STUFF TO FIND LIBRARIES ########
libraries = gsub("(library\\(.+?\\))", "LBRY HERE! \\1",unlistedRHelpBodies)

libraries = strsplit(libraries, "LBRY")

alllibraries = character(0)
#for(i in 101:length(functions)%/%10000){
j = 0
while(j < 13){
	for(i in 1:100){
		librariestest = sapply(libraries[(10000*i-9999):(10000*i)], function(x){
			gsub("^ HERE! library\\((.*?)\\).*", "\\1", x[which(grepl("^ HERE! ", x))])
			})
		alllibraries = c(alllibraries, unlist(librariestest))
		print(i+100*j)
	}
	libraries= libraries[-(1:1000000)]
	j = j + 1
}
librariestest = sapply(libraries, function(x){
	gsub("^ HERE! library\\((.*?)\\).*", "\\1", x[which(grepl("^ HERE! ", x))])
	})
alllibraries = c(alllibraries, unlist(librariestest))

libraryTable = sort(table(alllibraries), decreasing = TRUE)

#i = 0
#while(length(functions) > 0){
#	functionstest = gsub("^ HERE! ([[:alpha:]|.]+)\\(.*", "\\1", functions[[1]][which(grepl("^ HERE! ", functions[[1]]))])
#	allfunctions = c(allfunctions, functionstest)
#	functions = functions[-1]
#	if(i %% 10 == 0) print(i)
#	i = i + 1
#}


fullRHelp = do.call(rbind.fill, headerList)

subjects = fullRHelp$Subject
subjectfcns = gsub("([[:alpha:]|.]+\\()", "FUNCTION HERE! \\1", subjects)
subjectfcns = strsplit(subjectfcns, "FUNCTION")
subjectfcns = sapply(subjectfcns, function(x){
	gsub("^ HERE! ([[:alpha:]|.]+)\\(.*", "\\1", x[which(grepl("^ HERE! ", x))])
	})
subjectfcns = unlist(subjectfcns)
subjectfcnTable = sort(table(subjectfcns), decreasing = TRUE)
subjectlibs = gsub("(library\\(.+?\\))", "LBRY HERE! \\1", subjects)
subjectlibs = strsplit(subjectlibs, "LBRY")
subjectlibs = sapply(subjectlibs, function(x){
	gsub("^ HERE! library\\((.*?)\\).*", "\\1", x[which(grepl("^ HERE! ", x))])
	})
subjectlibs = unlist(subjectlibs)
subjectlibTable = sort(table(subjectlibs), decreasing = TRUE)




senders = sapply(fullRHelp$From, function(x) gsub(".*\\((.*)\\)", "\\1", x))
sendersTable = sort(table(senders), decreasing = TRUE)
sendersTable["Duncan Temple Lang"]
duncansEmails = fullRHelpBodies[which(senders == "Duncan Temple Lang")]
datestimes = formatDate(fullRHelp$Date)
dates = as.Date(datestimes)
hist(dates[dates > as.Date("1970-01-01")], breaks = "days")

plot(unlist(lapply(RHelpHeaders, length))[dateOrder], axes = FALSE, pch = 20, xlab = "Year (by month)", ylab = "E-mails sent", main = "E-mails sent through R-help by month")
axis(1, c(1,seq(10, 167, 12)), 1997:2011)
axis(2)

allFiles = list.files()
allFiles = gsub("(.*)\\.txt\\.gz", "\\1", allFiles)
allDates = as.Date(paste(allFiles, "01", sep = "-"), "%Y-%B-%d")
dateOrder = order(allDates)

=======
fullRHelp = do.call(rbind.fill, headerList)
dates = formatDate(fullRHelp$Date)
>>>>>>> f9daa4a48074fdd11552e5c4e3efd2cdac76d824





firstLines = unlist(sapply(1:(length(RHelp)), function(y){
	sapply(splitEmails(RHelp[[y]]), function(x) x[1])
}))
extractDates = gsub("^From [^ ]+ at [^ ]+ +(.*)", "\\1", firstLines)
firstDates = strptime(extractDates, format = "%a %b %d %X %Y")
firstDatesByDay = as.Date(firstDates)
dateTable = sort(table(firstDatesByDay), decreasing = TRUE) 
hist(firstDatesByDay, breaks = "days", freq = TRUE, xlab = "Year (by day)", ylab = "E-mails sent", main = "E-mails sent through R-help by day")


march2006 = readLines(gzfile("2006-March.txt.gz"))
march2006 = splitEmails(march2006)
march2006 = lapply(march2006, headerBody)

table(sapply(1:length(march2006), function(x) length(march2006[[x]]$header)))
march2006[7065]

#####################################


# the senders of the e-mails in the last month are:
senders = sapply(splitEmails(rhelp), function(x) findSender(x))


# Still need to extract time and date, subject, reply (or not), Message-ID of previous mail (if it's a reply), body of message

# include a plot of unique senders by month


%SystemRoot%\system32\cmd.exe
