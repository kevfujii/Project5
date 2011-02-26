List = list.files("enron",recursive = TRUE,full.names=TRUE)
Emails = vector("list",length(List))
lapply(1:length(List), function(i){ Emails[[i]]=readLines(List[i])})

Test =list.files("enron/maildir/allen-p/",full.names=TRUE,recursive = TRUE)
Testlong =lapply(1:length(Test), function(i){ Emails[[i]]=readLines(Test[i])})

PeopleNames =list.files("enron/maildir/",full.names=TRUE)
Email= lapply(1:length(PeopleNames), function(i){
  ByPerson =list.files(PeopleNames[i],full.names = TRUE,recursive = TRUE)
  lapply(1:length(ByPerson), function(j){
      readLines(ByPerson[j])
    })
  })



x= lapply(1:10,function(i){i})