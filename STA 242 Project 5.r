library(plyr)

### General functions ###

# This function, given an e-mail message, splits the message into two parts: the header and the body.
headerBody = function(txt){
	splitLine = min(which(txt == "")) # assumes that the header and body are split by the first blank line
	list(header = txt[1:(splitLine-1)], body = txt[(splitLine+1):length(txt)])
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

# creates a one-line data.frame of header information
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
#A function to sort through all emails, make them into strings with paste, and
#extract all the regular expressions.
#Note, this will require cleaning after running this function.
#Note, you must give it the "replacement" argument.
GetRegExp = function(File,RegExp, ...){
  File = paste(File,collapse = "\n")
  WhatWeWant = gsub(RegExp, x = File, ...)
  return(WhatWeWant)
  }

#Removes spaces and commas
CommaSpaceKiller = function(Vector){
  Result = do.call(c,strsplit( gsub("[[:space:]]", "", Vector, perl=T), ","))
  return(Result)
  }  
#Binds the sender to the recipient  
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
    Bcc = GetRegExp(header,"(?s).*?Bcc: (.*?)\\n[[:alpha:]-]+:.*",replacement = "\\1",perl = T)
    Bcc = BindFrom(From,Cc,"Sender","Bcc")
    }else{
      Cc = NULL
      Bcc = NULL
      }
  Emails = list(To,Cc,Bcc)  
  return(Emails)
  }
  
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

#Finds the subset of the frequency table in which the sender is "email"  
TakeOutEmail= function(List,Email){
  TakeOut = grep(Email,List[,1])
  AllEmails = List[TakeOut,] 
  return(AllEmails)
  }
  
#Take in the sorted frequency table and returns the top 5 people the specified
#emailer emailed.  
PlotNetwork=function(Matrix,n=5,l=0,StartNetwork,...){
  if( l > n){stop("You can't have more trees than branches")}
  if( n <=1 ){stop("You must have more than one branch")}
  Start = TakeOutEmail(Matrix,FindReplaceDot(StartNetwork))[1:n,]
  if(l == 0){Emails = FindReplaceDot(StartNetwork)}else{ 
    Emails = c(FindReplaceDot(StartNetwork),FindReplaceDot(Start[1:l,2]))
    }
  Keep =lapply(1:length(Emails),function(i){
    x = TakeOutEmail(Matrix,Emails[i])[1:n,]
    x[,1] =  gsub("(.*)@.*","\\1",x[,1])
    x[,2] =  gsub("(.*)@.*","\\1",x[,2])
    return(x)
    })
  Keep = do.call(rbind,Keep)
  Plot=ftM2graphNEL(Keep[,1:2], W=as.numeric(Keep[,3]), edgemode="directed")
  Plot = layoutGraph(Plot)
  plot(Plot,...)
  return(Keep)
  }
  
#Takes any thread with a ".", and replaces the "." with a "\\."
FindReplaceDot = function(char){
  char = gsub("\\.","\\\\\\.",char)
  return(char)
  }
#Gets the top "Top" frequences of column i, used for calculating how many
#time someone send AN email total   
TopNFreq = function(Mat,Top,i){
  Sum = aggregate(as.numeric(Mat[,3]),by = list(Mat[,i]),sum)
  Sum =Sum[order(Sum[,2],decreasing = TRUE),][1:Top,]
  return(Sum)
  }
#Outputs an xtable output of the top n recipiates for a specific sender
XtableFreq = function(Mat,Email,n){
  xtable(Mat[which(Mat[,1] == Email)[1:n],])
  }
  
########################Hard code   
#Creating my n by 2 matrix  
#Reads in the email and creates a list of lists, the first holds the persons
  #name and the second holds all the emails they sent.
PeopleNames =list.files("enron/maildir/",full.names=TRUE)
Email= lapply(1:length(PeopleNames), function(i){
  ByPerson =list.files(PeopleNames[i],full.names = TRUE,recursive = TRUE)
  lapply(1:length(ByPerson), function(j){
      readLines(ByPerson[j])
    })
  })
Headers = lapply(1:length(Email),function(i){
  lapply(1:length(Email[[i]]),function(j) {
     formatHeader(headerBody(Email[[i]][[j]])$header)
     })
  })
  
ToFromList = lapply(1:length(Headers),function(i){
  lapply(1:length(Headers[[i]]),function(j) {
     Headers[[i]][[j]]
     })
  })

FromToMat = Matrify(ToFromList,1)
FromCcMat = Matrify(ToFromList,2)
FromBccMat =Matrify(ToFromList,3)

FullFromCcBcc = FullFromCcBcc[order(as.numeric(FullFromCcBcc[,3]),decreasing=TRUE),]
FromToMat = FromToMat[order(as.numeric(FromToMat[,3]),decreasing = TRUE),]
FromCcMat = FromCcMat[order(as.numeric(FromCcMat[,3]),decreasing = TRUE),]
FromBccMat =  FromBccMat[order(as.numeric(FromBccMat[,3]),decreasing = TRUE),]

SortFromToMat = FromToMat[order(FromToMat[,1],partial =as.numeric(FromToMat[,3]) ,decreasing = TRUE),]
SortFromCcMat = FromCcMat[order(FromCcMat[,1],partial =as.numeric(FromCcMat[,3]) ,decreasing = TRUE),]
SortFromBccMat =  FromBccMat[order(FromBccMat[,1],partial =as.numeric(FromBccMat[,3]) ,decreasing = TRUE),]
SortFromCcBcc = FullFromCcBcc[order(FullFromCcBcc[,1],partial =as.numeric(FullFromCcBcc[,3]) ,decreasing = TRUE),]


XtableFreq(DamnPete,"jeff.dasovich@enron.com",5)
PlotNetwork(SortFromToMat,5,0,"jeff.dasovich@enron.com","neato")
PlotNetwork(SortFromToMat,3,3,"jeff.dasovich@enron.com")

DamnPete = SortFromCcBcc[-which(SortFromCcBcc[,1] == "pete.davis@enron.com" & SortFromCcBcc[,2] == "pete.davis@enron.com"),]
XtableFreq(DamnPete,"pete.davis@enron.com",10)
PlotNetwork(DamnPete,10,0,"pete.davis@enron.com","neato") 
PlotNetwork(DamnPete,3,3,"pete.davis@enron.com") 

ToFrom = TopNFreq(FromToMat,5,1)
ToCc = TopNFreq(FromCcMat,5,1)
ToBcc = TopNFreq(FromBccMat,5,1)
TopTotal =TopNFreq(FullFromCcBcc,5,1)

PlotNetwork(SortFromToMat,5,1,"kenneth.lay@enron.com")

AssistantKL = TakeOutEmail(SortFromToMat,"kenneth\\.lay@enron\\.com") 
JSkillingEmailed = TakeOutEmail(SortFromToMat,"jeff\\.skilling@enron\\.com") 
AssistantJS =  TakeOutEmail(SortFromToMat,"sherri\\.sera@enron\\.com")
  
################ R Help #######################
setwd("~/RHelp/")
RHelp = sapply(1:length(list.files()), function(x) readLines(gzfile(list.files()[x])))

# all e-mails seem to start with the same two lines
findEmailStart = function(txt){ 
	a = grep("^From .* at ", txt)  # 1st line starts with "From"
	b = grep("^From: .* at ", txt) # 2nd line starts with "From:"
	c = sapply(a, function(x) any(b == x + 1)) # finds all lines beginning with "From" followed immediately by a line starting with "From:"
	a[c]
}

# splits an e-mail text file into individual e-mails
splitEmails = function(txt){ 
	emailStart = findEmailStart(txt)
	groups = rep(seq_along(emailStart), times=diff(c(emailStart, length(txt) + 1)))
	split(txt, groups)
}

rhTest = splitEmails(rhelp)[1]


############ Big data files #########
### These break up the RHelp files into headers and bodies
### and also parse the headers into attributes

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

onlyFromLines = lapply(1:length(headerList), function(i) headerList[[i]]$From)
uniqueUsers = sapply(onlyFromLines, function(i) length(unique(i)))[dateOrder]
plot(uniqueUsers, xlab = "Year (by month)", ylab = "Unique users", main = "Unique users by month", axes = FALSE)
axis(1, c(1,seq(10, 167, 12)), 1997:2011)
axis(2)


fullRHelpBodies = do.call(c, RHelpBodies)
unlistedRHelpBodies = unlist(RHelpBodies)

######## STUFF TO FIND FUNCTIONS ###########
functions = gsub("([[:alpha:]|.]+\\()", "FUNCTION HERE! \\1", unlistedRHelpBodies)
functions = strsplit(functions, "FUNCTION")

allfunctions = character(0)
j = 0

# the while() loops seemed necessary from a memory standpoint...
# my computer didn't like it when I tried to find the functions
# all at once.

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

################## Finding libraries and functions in subject line

fullRHelp = do.call(rbind.fill, headerList)

subjects = fullRHelp$Subject

# makes a sorted table of all functions or libraries referenced in the subject line
makeSubjTable = function(dat = subjects, opt = "functions"){
	if(opt == "functions"){
		str1 = "([[:alpha:]|.]+\\()"
		str2 = "^ HERE! ([[:alpha:]|.]+)\\(.*"
	}
	if(opt == "libraries"){
		str1 = "(library\\(.+?\\))"
		str2 = "^ HERE! library\\((.*?)\\).*"
	}
	#dat = subjects
	subjectfcns = gsub(str1, "FUNCTION HERE! \\1", dat)
	subjectfcns = strsplit(subjectfcns, "FUNCTION")
	subjectfcns = sapply(subjectfcns, function(x){
		gsub(str2, "\\1", x[which(grepl("^ HERE! ", x))])
		})
	subjectfcns = unlist(subjectfcns)
	subjectfcnTable = sort(table(subjectfcns), decreasing = TRUE)
	subjectfcnTable
}


senders = sapply(fullRHelp$From, function(x) gsub(".*\\((.*)\\)", "\\1", x))
sendersTable = sort(table(senders), decreasing = TRUE)
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

dates = formatDate(fullRHelp$Date)

# A diagnostic tool to see if the messages on a day were quadrupled somehow
checkTimes4 = function(x, d, fl = onlyFromLines){
	dat = unlist(fl)[which(x == as.Date(d))]
	#extractDates[which(dates == as.Date("2006-03-06"))]
	sum(dat[1:(length(dat)/4)] != dat[(length(dat)/4+1):(length(dat)/2)])
		+ sum(dat[(length(dat)/2+1):(3*length(dat)/4)] != dat[(length(dat)/4+1):(length(dat)/2)])
		+ sum(dat[(length(dat)/2+1):(3*length(dat)/4)] != dat[(3*length(dat)/4+1):(length(dat))])

}

checkTimes4(dates, "2006-03-06")
times4 = which(sapply(1:365, function(i) checkTimes4(dates, as.Date(i, origin = as.Date("2006-01-01")))) == 0)
as.Date(times4, origin = as.Date("2006-01-01"))

firstLines = lapply(1:(length(RHelp)), function(y){
	sapply(splitEmails(RHelp[[y]]), function(x) x[1])
})


extractDates = gsub("^From [^ ]+ at [^ ]+ +(.*)", "\\1", unlist(firstLines))
firstDates = strptime(extractDates, format = "%a %b %d %X %Y")
firstDatesByDay = as.Date(firstDates)
dateTable = sort(table(firstDatesByDay), decreasing = TRUE) 
hist(firstDatesByDay, breaks = "days", freq = TRUE, xlab = "Year (by day)", ylab = "E-mails sent", main = "E-mails sent through R-help by day")


march2006 = readLines(gzfile("2006-March.txt.gz"))
march2006 = splitEmails(march2006)
march2006 = lapply(march2006, headerBody)

#####################################

