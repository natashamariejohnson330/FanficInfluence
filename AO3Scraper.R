#This function takes a url of a work and returns the url of the first kudos page
getKudosPage<-function(og.url){
  split.url<-unlist(strsplit(og.url, "\\?"))
  kudos.url<-paste(split.url[1], "kudos?page=1", sep="/")
  return(kudos.url)
}

#takes the url of a user and returns both their profile and works page
getUserPage<-function(user.url){
  user.url<-paste("https://archiveofourown.org", user.url, sep="")
  profile.url<-paste(user.url, "profile", sep="/")
  works.url<-paste(user.url, "works", sep="/")
  return(c(profile.url, works.url))
}

#takes the url of the kudos page and iterates through all of the users who left
#kudos (not anonymously)
#returns a table of names and user nodes
iterateKudos<-function(kudos.url, fic.number){
  user.names<-NULL
  user.nodes<-NULL
  print(kudos.url)
  kudos.page.data<-GET(kudos.url)
  kudos.tree<-htmlParse(kudos.page.data)
  kudos.nodes<-xpathSApply(kudos.tree, "//*/p[@class='kudos']/a", xmlAttrs)
  kudos.names<-xpathSApply(kudos.tree, "//*/p[@class='kudos']/a", xmlValue)
  page.number=1
  while(length(kudos.nodes)>0){
  #while(page.number<2){
    print(kudos.url)
    user.names<-c(user.names, kudos.names)
    user.nodes<-c(user.nodes, kudos.nodes)
    page.number<-page.number+1
    kudos.url<-unlist(strsplit(kudos.url, "\\?"))[1]
    kudos.url<-paste(kudos.url, "?page=", as.character(page.number), sep="")
    kudos.page.data<-GET(kudos.url)
    kudos.tree<-htmlParse(kudos.page.data)
    kudos.nodes<-xpathSApply(kudos.tree, "//*/p[@class='kudos']/a", xmlAttrs)
    kudos.names<-xpathSApply(kudos.tree, "//*/p[@class='kudos']/a", xmlValue)
    if(length(kudos.names)>5){
      print(kudos.names[1:5])
    } else {
      print(kudos.names[1])
    }
    Sys.sleep(sample(seq(3,5,by=1),1))
  }
  kudos.table<-data.frame(user.names, user.nodes)
  print(dim(kudos.table))
  current.kudos.file.name <- paste("KudosFanfic", fic.number, ".csv", sep = "")
  write.csv(kudos.table, file = current.kudos.file.name, row.names = F)
  return(kudos.table)
}

#uses user url to get the date that user joined AO3
getDateJoined<-function(user.base.url, ao3.base="https://archiveofourown.org"){
  profile.url<-paste(ao3.base, user.base.url, "profile", sep="/")
  profile.page<-GET(profile.url)
  profile.parse<-htmlParse(profile.page)
  categories<-xpathSApply(profile.parse, "//*/dt", xmlValue)
  data<-xpathSApply(profile.parse, "//*/dd", xmlValue)
  join.hit<-grep("joined", categories)
  user.date<-data[join.hit]
  Sys.sleep(sample(5,1))
  return(user.date)
}

#takes a user url and iterates through all their works and returns a vector of
#work id numbers for everything that is sherlock
getFandomFicIds<-function(user.base.url, ao3.base="https://archiveofourown.org", target.fandom="Sherlock \\(TV\\)"){
  page1.url<-paste(ao3.base, user.base.url, "works", sep="/")
  #find a users works page
  page1.page<-GET(page1.url)
  page1.data<-htmlParse(page1.page)
  page1.fandom.nodes<-getNodeSet(page1.data, "//*/h5[@class='fandoms heading']")
  #finds all the fandoms on the page
  fandoms<-lapply(page1.fandom.nodes, function(x) xpathSApply(x, "a", xmlValue))
  #finds the sherlock fanfics given all the fandoms
  fandom.hits<-grep(target.fandom, fandoms)
  if(length(fandom.hits)>0){
    page1.word.ids<-xpathSApply(page1.data, "//*/ol[@class='work index group']/li", xmlAttrs)
    page1.ids<-lapply(page1.word.ids[1,], function(x) unlist(strsplit(x, "_")))
    page1.ids<-unlist(lapply(page1.ids, function(x) x[2]))
    page1.ids<-page1.ids[fandom.hits]
    fandom.works<-page1.ids
  } else {
    fandom.works<-NULL
  }
  curr.page<-2
  #while there are any fanfics left
  while(length(fandoms)>0){
    print(curr.page)
    print("here! Line 91")
    new.page<-paste(page1.url, "?page=", as.character(curr.page), sep="")
    curr.url<-GET(new.page)
    curr.parse<-htmlParse(curr.url)
    curr.nodes<-getNodeSet(curr.parse, "//*/h5[@class='fandoms heading']")
    fandoms<-lapply(curr.nodes, function(x) xpathSApply(x, "a", xmlValue))
    fandom.hits<-grep(target.fandom, fandoms)
    print("fandom hits")
    print(length(fandom.hits))
    print(fandom.hits)
    if(length(fandom.hits)>0){
      curr.word.ids<-xpathSApply(curr.parse, "//*/ol[@class='work index group']/li", xmlAttrs)
      curr.ids<-lapply(curr.word.ids[1,], function(x) unlist(strsplit(x, "_")))
      curr.ids<-unlist(lapply(curr.ids, function(x) x[2]))
      curr.ids<-curr.ids[fandom.hits]
      fandom.works<-c(fandom.works, curr.ids)
    } 
    curr.page<-curr.page+1
    Sys.sleep(sample(5,1))
  }
  print("got here! Line 108")
  return(fandom.works)
}
    
  
#Takes in the id of a sherlock fic and finds the published and completed date
getFicDate<-function(work.id, ao3.base="https://archiveofourown.org/works"){
  work.url<-paste(ao3.base, work.id, sep="/")
  work.url<-paste(work.url, "?view_adult=true", sep="")
  work.page<-GET(work.url)
  work.data<-htmlParse(work.page)
  date.published<-xpathSApply(work.data, "//*/dd[@class='published']", xmlValue)
  if(length(date.published)==0){
    chapter.url<-xpathSApply(work.data, "//*/a", xmlAttrs)
    #new.url<-gsub("?view_adult=true", chapter.url, work.url)
    new.url<-paste("https://archiveofourown.org", chapter.url, sep="/")
    new.url<-paste(new.url, "?view_adult=true", sep="")
    #print(new.url)
    new.page<-GET(new.url)
    new.data<-htmlParse(new.page)
    date.published<-xpathSApply(new.data, "//*/dd[@class='published']", xmlValue)
    date.completed<-xpathSApply(new.data, "//*/dd[@class='status']", xmlValue)
  } else {
    #ate.completed<-xpathSApply(work.data, "//*/dd[@class='status']", xmlValue)
    date.completed<-date.published
  }
  return(c(date.published, date.completed))
  
  #dd[@class="published"]
  #dd[@class="completed"]
}


#takes in a user id, checks if they wrote at least 2 sherlock fanfics.
#if so, add their user id, the works, dates, etc to user.work.table
userWorkTable<-function(userID){
  all.user.works<-getFandomFicIds(userID)
  if(length(all.user.works)>1){
    all.user.pub.dates<-lapply(all.user.works, function(x) getFicDate(x))
    date.table<-as.data.frame(do.call("rbind", all.user.pub.dates))
    user.work.table<-data.frame(rep(userID, nrow(date.table)), all.user.works, date.table)
    colnames(user.work.table)<-c("UserID", "Work_ID", "Date_Published", "Date_Completed")
  } else {
    user.work.table<-c(userID,"NA", "NA", "NA")
    names(user.work.table)<-c("UserID", "Work_ID", "Date_Published", "Date_Completed")
  }
  return(user.work.table)
}

#takes in fic name and url
#returns everything
workKudos<-function(fic.name, fic.url, all.files, fic.number){
  current.kudos.file <- paste("KudosFanfic", fic.number , ".csv", sep = "")
  current.dates.file <- paste("UserDatesFanfic", fic.number , ".csv", sep = "")
  current.userworks.file <- paste("UserWorks", fic.number , ".csv", sep = "")
  
  print("here1")
  
  if (!(current.kudos.file %in% all.files)) {
  kudos.page<-getKudosPage(fic.url)
  all.kudos.users<-iterateKudos(kudos.page, fic.number)
  }
  #CHECK THIS
  else {
    print("here2")
    all.kudos.users<-read.csv(file=current.kudos.file, header = F)
  }
  
  if (current.dates.file %in% all.files) {
    user.dates.progress<-read.csv(file=current.dates.file)
    start = nrow(user.dates.progress) + 1
  } 
  else {
    user.dates.progress<-NULL
    user.info<-NULL
    start = 1
  }
  for(i in start:nrow(all.kudos.users)) {
    current.user<-all.kudos.users[i, 2]
    current.date<-getDateJoined(current.user)
    print(current.date)
    current.row<-c(all.kudos.users[i,1], current.date)
    if(length(current.row)==2){
      print(current.row)
      print(dim(user.dates.progress))
      print(user.dates.progress[1,])
      #user.info<-c(all.kudos.users[i,1], current.date)
      user.dates.progress<-rbind(user.dates.progress, current.row)
      write.csv(user.dates.progress, file = current.dates.file, row.names = F)
    }
  }

  

  # if (current.userworks.file %in% all.files) {
  #   user.works.progress<-read.csv(file=current.userworks.file)
  #   latest.user<-user.works.progress[nrow(user.works.progress),1]
  #   start = grep(latest.user, all.kudos.users[, 2])
  #   start = start + 1
  # }
  # else {
  #   user.works.progress<-NULL
  #   start = 1
  # }
  # for (i in start:nrow(all.kudos.users)) {
  #   print(all.kudos.users[i,1])
  #   current.user<-all.kudos.users[i, 2]
  #   current.user.works<-userWorkTable(current.user)
  #   current.row<-c(all.kudos.users[i,1], current.user.works)
  #   user.works.progress<-rbind(user.works.progress,current.row)
  #   write.csv(user.works.progress, file = current.userworks.file, row.names = F)
  # }

  }

newMainFunction<-function(top.fics) {
  require(httr)
  require(XML)
  fic.names<-top.fics$title
  fic.urls<-top.fics$Url
  all.files<-list.files()
  print("hi")
  #check this if statement
  if (!"KudosFanfic5.csv" %in% all.files) {
    for (fic.number in 1:5) {
      fic.kudos.count <- paste("KudosFanfic", fic.number , ".csv", sep = "")
      if (!fic.kudos.count %in% all.files) {
        kudos.page<-getKudosPage(fic.urls[fic.number])
        all.kudos.users<-iterateKudos(kudos.page, fic.number)
        }
    }
  }
  else {
    print("got here")
    count = 1
    for (fic.number in 1:5) {
      #
      fic.dates.count <- paste("UserDatesFanfic", fic.number , ".csv", sep = "")
      if (!fic.dates.count %in% all.files) {
        break
      }
      else {
        count = count + 1
      }
    }
    print("Count:", count)
    for (fic.number in count:5) {
      print(paste("count:", count))
      workKudos(ficnames[fic.number], fic.urls[fic.number], all.files, fic.number)
    }
  }
}





#web.address<-"https://archiveofourown.org/works/244826?view_adult=true"

#?view_adult=true

#kudos.address<-unlist(strsplit(web.address, "\\?"))[1]
#kudos.address<-paste(kudos.address, "kudos?/page=1", sep="/")

#fic.data<-GET(web.address)
#fic.parsed<-htmlParse(fic.data)

#kudos.data<-GET(kudos.address)
#kudos.tree<-htmlParse(kudos.data)


#kudos.nodes<-xpathSApply(kudos.tree, "//*/p[@class='kudos']/a", xmlAttrs)
#kudos.names<-xpathSApply(kudos.tree, "//*/p[@class='kudos']/a", xmlValue)
#test.user<-getUserPage(kudos.nodes[1])
#user.profile.data<-GET(user.pages)
#user.works.data<-GET(user.pages[2])
#user.works.parse<-htmlParse(user.works.data)
#fandoms<-xpathSApply(user.works.parse, "//*/h5[@class='fandoms heading']/a", xmlValue)



#Base Page
#https://archiveofourown.org/users/BenAddictViolaBatch

#Works page
#https://archiveofourown.org/users/BenAddictViolaBatch/works

#Profile 
#https://archiveofourown.org/users/BenAddictViolaBatch/profile


#Filtered Page
#https://archiveofourown.org/users/BenAddictViolaBatch/pseuds/BenAddictViolaBatch/works?commit=Sort+and+Filter&include_work_search%5Bfandom_ids%5D%5B%5D=133185&page=2&utf8=%E2%9C%93&work_search%5Bcomplete%5D=&work_search%5Bcrossover%5D=&work_search%5Bdate_from%5D=&work_search%5Bdate_to%5D=&work_search%5Bexcluded_tag_names%5D=&work_search%5Blanguage_id%5D=&work_search%5Bother_tag_names%5D=&work_search%5Bquery%5D=&work_search%5Bsort_column%5D=revised_at&work_search%5Bwords_from%5D=&work_search%5Bwords_to%5D=
