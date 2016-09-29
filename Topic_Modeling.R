##################
# TOPIC MODELING #
##################
require(mallet)
require(wordcloud)
require(RMySQL)
require(tcltk)

#############
# FUNCTIONS #
#############

# creates topic.model object
model = function(doc,stop.words,ntopics=10,nsteps=400)
{
  mallet.instances <- mallet.import(doc$id,doc$text,stop.words,F,"[\\p{L}]{1,}")
  topic.model <- MalletLDA(ntopics)
  topic.model$loadDocuments(mallet.instances)
  topic.model$train(nsteps)
  return(topic.model)
}

# creates a scatterplot
# doc.idxs is a numerical vector indicating positions between 1 and
# the total number of chunks. A blue line will be plotted at each position.
# (This usually indicates at which chunk a novel begins and another ends)
# abbs is a character vector of one- or two-character abbreviations to be
# printed between each set of blue lines
makeplot = function(title,i,doc.topics,works.start,works.end,abbs)
{
  n <- length(works.start)
  spots <- rep(list(list()),n)
  y <- 0.54
  for(j in 1:n)
  {
    spots[[j]]$x <- floor((works.start[j]+works.end[j])/2)
    spots[[j]]$y <- y
  }
  
  par(mai=c(0.1,0.5,0.5,0.1))
  plot(doc.topics[,i], xaxt='n', ann=FALSE, ylim=c(0,0.55))
  
  for(x in 1:n)
  {
    abline(v=works.start[x],col="blue")
    abline(v=works.end[x],col="blue")
  }

  title(main = paste("Topic",title,collapse = " "))
  for(j in 1:n)
  {
    text(spots[[j]],abbs[j])
  }
}

#makes a plot of topic concentrations in chunk #i
t.in.doc.plot = function(i,doc.topics)
{
  n <- dim(doc.topics)[2]
  ts <- doc.topics[i,]
  ts.sort <- sort(ts)
  barplot(as.matrix(ts.sort),col=c("white","gray"))
  title(main = paste("Chunk",i,collapse = ""))
  spots <- rep(list(list()),n)
  spots[[1]]$x <- 0.3
  spots[[1]]$y <- 1 - ts.sort[n]/2
  text(spots[[1]],toString(which(ts==ts.sort[n])))
  for(j in 2:n)
  {
    spots[[j]]$x <- spots[[j-1]]$x + 0.15
    if(spots[[j]]$x > 1.1)
    {
      spots[[j]]$x <- spots[[j]]$x - 0.9
    }

    spots[[j]]$y <- spots[[j-1]]$y - ts.sort[n-j+1]/2 - ts.sort[n-j+2]/2
    if(spots[[j]]$y>0.025)
    {
      text(spots[[j]],toString(which(ts==ts.sort[n-j+1])))
    }
  }
}

# makes a word cloud
wcloud = function(vocab,freq,i)
{
  par(adj=0.5, mai=c(0.1,0.1,0.15,0.1))
  wordcloud(vocab,freq,max.words=100,rot.per=0,scale=c(2,.5),colors=c("black","purple","blue","green"))
  par(adj=0)
  title(main = i)
}

# finds the words that are assigned to only one topic (ignoring singletons)
# and their word counts in the corpus
topic.u.words = function(topic.words, ntopics, vocab)
{
  spec.l <- lapply(1:ntopics, function(i) c())
  spec.c.l <- lapply(1:ntopics, function(i) c())
  for(i in 1:(length(vocab))){
    if(sum(topic.words[,i])==max(topic.words[,i]) && max(topic.words[,i]>1))
    {
      spec.word <- vocab[i]
      this.topic <- which(topic.words[,i] == max(topic.words[,i]))
      spec.l[[this.topic]] <- c(spec.l[[this.topic]], spec.word)
      spec.c.l[[this.topic]] <- c(spec.c.l[[this.topic]], max(topic.words[,i]))
    }
  }
  return(list(spec.l,spec.c.l))
}

#finds which ad hoc name a vector of words matches with most closely
#using the lookup environment env
id.ah = function(these.words, env, ah.n, minHits=3)
{
  ah.weights <- rep(0, ah.n)
  for(word in these.words)
  {
    if(exists(word, envir = env, inherits = FALSE))
    {
      idx <- get(word, envir = env)
      ah.weights[idx] <- ah.weights[idx] + 1
    }
  }

  if(max(ah.weights) >= minHits)
  {
    allHits <- which(ah.weights == max(ah.weights))
    return(allHits[1])
  } 

  else 
  {
    return('none')
  }
}

#returns the doc.topics matrix from a .txt file
load.mat = function(filename)
{
  df <- read.table(filename)
  return(as.matrix(df))
}

# naming convention for the "global_id" field in all tables
make.global.id = function(i=0, collective=FALSE, root=FALSE)
{
  global.id <- paste(corpus.name, '_', chunk.type, pos.type, '_', sep="")
  if(collective)
  {
    global.id <- paste(global.id, ntopics, sep="")
  } 

  else if(!root) 
  {
    global.id <- paste(global.id, i, '/', ntopics, sep="")
  }

  return(global.id)
}

#######################
# INTERFACE FUNCTIONS #
#######################

# Interface for choosing a directory with a GUI.
# Error handling allows you to try again if no folder is selected
# (e.g. when Cancel is selected)
choose.dir = function(caption, default="")
{
  this.dir <- NA
  while(is.na(this.dir))
  {
    this.dir <- tk_choose.dir(default=default, caption=caption)
    if(is.na(this.dir))
    { 
      read.out <- readline("No directory selected. Press enter to try again. ") 
    }
  }

  return(this.dir)
}

# Interface for choosing a txt file with a GUI.
# Error handling allows you to try again if no file is selected
# (e.g. when Cancel is selected)
choose.txt.file = function(caption, default="")
{
  file <- c()
  txt.filter <- matrix(c("Text", ".txt"), 1, 2, byrow = TRUE)
  while(length(file) != 1)
  {
    file <- tk_choose.files(default=default, caption=caption, multi=FALSE, filters = txt.filter)
    if(length(file) != 1)
    {
      read.out <- readline("Select one .txt file. Press enter to try again. ")
    }
  }

  return(file)
}

# Interface for choosing an integer greater than zero
# Prompt is repeated if an unsatisfactory number is given
# Use escape=TRUE if you want NA to be returned when the user gives empty input
choose.num <- function(prompt.in, escape=FALSE)
{
  try.num <- TRUE
  while(try.num)
  {
    num <- as.numeric(readline(prompt.in))
    if(is.na(num))
    {
      try.num <- !escape
    } 

    else 
    {
      try.num <- (as.integer(num) != num) || (as.integer(num) < 1)
      if(escape && try.num)
      { 
        cat("Invalid input.\n") 
      }
    }
  }
  return(num)
}

# Interface for 'yes or no' prompts. Allows a default choice.
# Error handling repeats the prompt until a string starting with
# 'Y', 'y', 'N', or 'n' is entered.
yn.prompt = function(caption, default="")
{
  ans <- "init"
  while(ans != 'Y' && ans != 'N')
  {
    user.prompt <- paste('\n', caption, " (Y/N) ", sep="")
    if(default != "")
    {
      user.prompt <- paste(user.prompt, '[', default, '] ', sep="")
    }

    ans <- readline(user.prompt)
    if(ans == "")
    { 
      ans <- default 
    }
    ans <- substring(toupper(ans), 1, 1)
  }

  return(ans)
}

# database connection with error handling
db.init = function(dbname="")
{
  db.success <- FALSE
  while(db.success == FALSE)
  {
    db.success <- TRUE
    username <- readline("MySQL username? ")
    dbPass <- readline("MySQL password? ")
    if(dbname == "")
    {
      tryCatch(
        db <- dbConnect(MySQL(), host="XXXX", username=username, password=dbPass),
        error = function(e){
          print(e)
          db.success <<- FALSE
        }
      )
    } 

    else 
    {
      tryCatch(
        db <- dbConnect(MySQL(), dbname=dbname, host="XXXX", username=username, password=dbPass),
        error = function(e){
          print(e)
          db.success <<- FALSE
        }
      )
    }
  }
  return(db)
}

# Allows user to choose which database to use
db.choose = function(db)
{
  all.dbs <- dbGetQuery(db, "show databases;")$Database
  cat(paste("Available databases: '", paste(all.dbs, collapse="', '"), "'\n", sep=""), fill=80)
  check.dbname <- TRUE
  while(check.dbname)
  {
    dbname <- readline("Select database. ")
    check.dbname <- !(dbname %in% all.dbs)
  }

  read.out <- dbGetQuery(db, paste("use ", dbname, ';', sep=""))
  return(dbname)
}

# escapes char in string, allowing the string to be used
# with system() (by escaping spaces)
# or a MySQL prompt (by escaping '_')
escape.char = function(char, string)
{
  string <- unlist(strsplit(string, char))
  string <- paste(string, collapse=paste("\\", char, sep=""))
  return(string)
}

# checks if the global_id for n topics is already in the database.
# If not, returns FALSE
global.in.db = function(n)
{
  global.id.match <- paste(make.global.id(root=TRUE), "%", n, sep="")
  global.id.match <- escape.char('_', global.id.match)
  df.out <- dbGetQuery(db, paste('select * from main where global_id like "', global.id.match, '";', sep=""))
  if(dim(df.out)[1] != 0)
  {
    cat("This output already exists in the database.\n")
    return(TRUE)
  } 

  else 
  {
    return(FALSE)
  }
}

# checks if output matching tag and a number of topics in ns is already in outputdir.
# If not, returns FALSE
output.in.dir = function(tag, ns)
{
  any.output.exists <- FALSE
  for(n in ns)
  {
    regex <- paste(tag, ".+of", n, "\\.", sep="")
    this.output.exists <- any(grepl(regex, dir(outputdir)))
    any.output.exists <- any(any.output.exists, this.output.exists)
  }

  if(any.output.exists)
  {
    cat(paste("An output file with this tag and a selected number of topics already exists in ", normalizePath(outputdir), "\n", sep=""), fill=80)
    cat("If you want to overwrite previous output, delete unwanted output manually.\n")
  }

  return(any.output.exists)
}

##########################
# TOPIC MODELING ROUTINE #
##########################

wd <- getwd()

# GUI allows you to chooose directory containing chunks
chunk.dir <- choose.dir("Select chunk directory")
setwd(chunk.dir)

#Loads default stop.words and/or database name
if(file.exists("../../.RData")){
  load("../../.RData")
}

# Stop words
if(!exists("stop.words"))
{
  stop.words <- choose.txt.file("Select stop words file", "../..")
} 

else 
{
  # if a default has been loaded
  if(file.exists(stop.words))
  {
    # if the default file still exists, ensure the user wants to use it
    default.stop.words <- yn.prompt(paste("Use\n", stop.words, "\nfor stop words?", sep=""), 'Y')
    if(default.stop.words == 'N')
    {
      stop.words <- choose.txt.file("Select stop words file", "../..")
    }
  } 

  else 
  {
    # if the default file doesn't exist, prompt the user for another one
    stop.words <- choose.txt.file("Select stop words file", "../..")
  }
}

# Output directory
if(!file.exists("../../Output"))
{
  outputdir <- choose.dir("Choose output directory", "../..")
} 

else 
{
  default.outputdir <- yn.prompt(paste("Use\n", normalizePath("../../Output"), "\nfor output directory?", sep=""), 'Y')
  if(default.outputdir == 'Y')
  {
    outputdir <- "../../Output"
  } 

  else 
  {
    outputdir <- choose.dir("Choose output directory", "../..")
  }
}

# Scatterplot abbreviations for larger works
if(!file.exists("../../abbs.txt"))
{
  abbs.path <- choose.txt.file("Select scatterplot abbreviations", "../..")
} 

else 
{
  default.abbs <- yn.prompt(paste("Use\n", normalizePath("../../abbs.txt"), "\nfor scatterplot abbreviations?", sep=""), 'Y')
  if(default.abbs == 'Y')
  {
    abbs.path <- "../../abbs.txt"
  } 

  else 
  {
    abbs.path <- choose.txt.file("Select scatterplot abbreviations", "../..")
  }
}

# Gets information about corpus, chunking, pos type, from the corpus directory structure
chunk.info <- tail(unlist(strsplit(getwd(), '/')), 3)
corpus.name <- chunk.info[1]
chunk.type <- chunk.info[2]
pos.type <- chunk.info[3]

# Database use
use.db.prompt <- yn.prompt("Populate database with this output?", 'Y')
use.db <- use.db.prompt == 'Y'
if(use.db)
{
  if(!exists("dbname"))
  {
    db <- db.init()
    dbname <- db.choose(db)
  } 

  else 
  {
    use.def.db <- yn.prompt(paste("Use ", dbname, " as database?", sep=""), 'Y')
    if(use.def.db == 'Y')
    {
      db <- db.init(dbname)
    } 

    else 
    {
      db <- db.init()
      dbname <- db.choose(db)
    }
  }
}

# Number of optimization steps per topic model
nsteps <- choose.num("Number of optimization steps per topic model? [400] ", escape=TRUE)
if(is.na(nsteps))
{ 
  nsteps <- 400 
} 

# Number of topics
if(use.db){
  # if using the database, this ensures existing output is not remade
  allntopics <- choose.num("Number of topics? ")
  while(global.in.db(allntopics))
  {
    allntopics <- choose.num("Number of topics? ")
  }

  choose.next.n <- TRUE
  while(choose.next.n)
  {
    next.n <- choose.num("Additional number of topics? (Press enter to finish) ", escape=TRUE)
    if(is.na(next.n))
    {
      choose.next.n <- FALSE
    } 

    else 
    {
      if(!global.in.db(next.n))
      {
        allntopics <- c(allntopics, next.n)
      }
    }
  }
} 

else 
{
  # or collects allntopics without consulting a database
  allntopics <- choose.num("Number of topics? ")
  choose.next.n <- TRUE
  while(choose.next.n)
  {
    next.n <- choose.num("Additional number of topics? (Press enter to finish) ", escape=TRUE)
    if(is.na(next.n))
    {
      choose.next.n <- FALSE
    } 

    else 
    {
      allntopics <- c(allntopics, next.n)
    }
  }
}

# disables database access if a model with the same number of topics is being run multiple times
if( (length(unique(allntopics)) != length(allntopics)) && use.db )
{
  cat("The database should contain only one topic model for a given number of topics and chunking type.\n", fill=80)
  cat("Database connection is being disabled, because you have chosen a particular number of topics to be run multiple times.", fill=80)
  read.out <- dbDisconnect(db)
  use.db <- FALSE
}

# tags, to be added to output filenames
def.tag <- paste(chunk.type, pos.type, '_', sep="")
keep.default.tag <- yn.prompt(paste("Do you want to use '", def.tag, "' as a tag for your output?", sep=""), 'Y')
if(keep.default.tag == 'Y')
{
  # error handling catches attempt to overwrite existent output
  if(output.in.dir(def.tag, allntopics))
  {
    read.out <- readline("Press enter when you are ready to continue. ")
    tags <- c()
    new.tags.needed <- TRUE
  } 

  else 
  {
    # if output does not yet exist, the tag for all filenames is the default
    tags <- def.tag
    new.tags.needed <- FALSE
  }
}

# user-defined tags
if(keep.default.tag == 'N' || new.tags.needed)
{
  custom.tags.needed <- TRUE
  while(custom.tags.needed)
  {
    tags <- readline(paste("Enter an output tag for", allntopics[1], "topic model. (Press enter to use no tags) "))
    if(!output.in.dir(tags, allntopics[1]))
    {
      if(tags != "")
      {
        continue.tags <- TRUE
        while(continue.tags && length(tags) < length(allntopics))
        {
          next.tag <- readline(paste("Enter tag for ", allntopics[length(tags)+1], " topic model. (Press enter to stop and use previous tag). ", sep=""))
          continue.tags <- next.tag != ""
          if(continue.tags && !output.in.dir(next.tag, allntopics[length(tags)+1]))
          {
            tags <- c(tags, next.tag)
          }

          if(!continue.tags)
          {
            # makes sure previous tag, applied to all the rest of allntopics, doesn't clash with existing output
            custom.tags.needed <- output.in.dir(next.tag, tail(allntopics, length(allntopics) - length(tags)))
            if(custom.tags.needed){ cat("Restarting custom tagging from the beginning.\n")}
          }
        }

        if(length(tags) == length(allntopics))
        {
         custom.tags.needed <- FALSE 
        } 
      } 

      else 
      {
        custom.tags.needed <- output.in.dir(tags, allntopics)
      }
    }
  }
  cat("Tags complete.\n")
}

# Saves default stop words path and database name
if(use.db)
{
  save(list=c("stop.words", "dbname"), file="../../.RData")
} 

else 
{
  save(list="stop.words", file="../../.RData")
}

# recognition of memes within produced topics
recognize.memes.prompt <- yn.prompt("Attempt to identify memes?", 'N')
# disables use of memes, which is not yet supported
recognize <- FALSE
if(recognize.memes.prompt == 'Y')
{
  cat("Recognition of memes is not yet supported.\n")
}

# creates the document to model
doc <- mallet.read.dir('.')

# reads memes into lookup tables ah.10.e and ah.u.e
if(recognize)
{
  ah.10.in <- scan("../../../Scripts/1500N_10_ah10.txt", what = "character", sep = '\n', encoding = 'UTF-8')
  ah.u.in <- scan("../../../Scripts/1500N_10_ahu.txt", what = "character", sep = '\n', encoding = 'UTF-8')
  ah.10.e <- new.env(hash = TRUE, size = length(ah.10.in))
  ah.u.e <- new.env(hash = TRUE, size = length(ah.u.in))
  ah.n <- length(ah.10.in) #number of memes
  for(i in 1:ah.n)
  {
    this.ah.10.words <- unlist(strsplit(ah.10.in[i], " "))
    this.ah.u.words <- unlist(strsplit(ah.u.in[i], " "))
    for(a.word in this.ah.10.words)
    {
      if(!exists(a.word, envir=ah.10.e, inherits=FALSE))
      {
        assign(x=a.word, value=i, envir=ah.10.e)
      } 

      else 
      {
        curVal <- get(a.word, envir=ah.10.e, inherits=FALSE)
        assign(x=a.word, value=c(curVal,i), envir=ah.10.e)
      }
      
    }
    for(a.word in this.ah.u.words)
    {
      if(!exists(a.word, envir=ah.u.e, inherits=FALSE))
      {
        assign(x=a.word, value=i, envir=ah.u.e)
      } 

      else 
      {
        curVal <- get(a.word, envir=ah.10.e, inherits=FALSE)
        assign(x=a.word, value=c(curVal,i), envir=ah.u.e)
      }
    }
  }
}

# begins tracking which tag to use
w.tag <- 1

# Creates topic modeling output for each chosen number of topics
for(ntopics in allntopics)
{
  #chooses user's tags to prepend to file names
  tag <- tags[w.tag]
  w.tag <- w.tag + 1
  if(w.tag > length(tags))
  {
    w.tag <- length(tags)
  }
  
  #creates the topic model
  topic.model <- model(doc,stop.words,ntopics=ntopics,nsteps=nsteps)
  doc.topics <- mallet.doc.topics(topic.model)
  norm.doc.topics <- mallet.doc.topics(topic.model, normalized = TRUE)
  topic.words <- mallet.topic.words(topic.model)
  norm.topic.words <- mallet.topic.words(topic.model, normalized = TRUE)
  vocab <- topic.model$getVocabulary()
  # saves topic model info in an .RData file
  save.file <- paste(outputdir, '/', tag, '_', chunk.type, pos.type, '_', ntopics, ".RData", sep="")
  save(list=c("doc.topics", "topic.words", "vocab"), file=save.file)

  #recognizes topics from memes
  if(recognize)
  {
    recog.topics <- c()
    topics.ah.names <- c()
    #organized by mallet number
    spec.l <- topic.u.words(topic.words, ntopics, vocab)[[1]]
    for(i in 1:ntopics)
    {
      top.10 <- mallet.top.words(topic.model, topic.words[i,],10)$words
      topic.u <- spec.l[[i]]
      hit.10 <- id.ah(top.10, ah.10.e, ah.n)
      hit.u <- id.ah(topic.u, ah.u.e, ah.n)
      if(hit.10 == hit.u && hit.10 != "none")
      {
        topics.ah.names[i] <- paste("ah",hit.10,sep="")
        recog.topics <- c(recog.topics, i)
      }
    }
    ah.order <- order(topics.ah.names)
    ah.order <- ah.order[!is.na(topics.ah.names[ah.order])]
    topics.order <- ah.order
    for(i in 1:ntopics)
    {
      if(!(i %in% ah.order))
      {
        topics.order <- c(topics.order, i)
      }
    }
  } 

  else 
  {
    recog.topics <- c()
    topics.order <- 1:ntopics
  }
  
  if(use.db)
  {
    # Loads table "main"
    global.ids <- sapply(1:ntopics, make.global.id)
    chunk.types <- rep(paste(chunk.type, pos.type, sep=""), ntopics)
    num.topics <- rep(ntopics, ntopics)
    topic.names <- rep(" ", ntopics)
    date.time <- rep(as.character(Sys.time()), ntopics)
    main.df <- data.frame(global.ids, chunk.types, num.topics, topic.names, 1:ntopics, date.time, stringsAsFactors = FALSE)
    names(main.df) <- dbGetQuery(db, "describe main;")$Field
    dbWriteTable(db, "main", main.df, append=TRUE, row.names=FALSE)
    
    # Loads table "top_chunks"
    global.id.all <- c()
    chunk.ranks.all <- c()
    chunk.weights.all <- c()
    chunk.names.all <- c()
    for(i in 1:ntopics)
    {
      global.id <- make.global.id(i)
      chunk.weights <- norm.doc.topics[,i]
      chunk.ranks <- rank(-chunk.weights, ties.method = "first")
      chunk.nums <- which(chunk.ranks <= 20)
      chunk.names <- doc$id[chunk.nums]
      chunk.weights <- chunk.weights[chunk.nums]
      chunk.ranks <- chunk.ranks[chunk.nums]
      len.data <- length(chunk.nums)
      
      global.id.all <- c(global.id.all, rep(global.id, len.data))
      chunk.ranks.all <- c(chunk.ranks.all, chunk.ranks)
      chunk.weights.all <- c(chunk.weights.all, chunk.weights)
      chunk.names.all <- c(chunk.names.all, chunk.names)
    }

    data.df <- data.frame(global.id.all, chunk.ranks.all, chunk.weights.all, chunk.names.all, stringsAsFactors = FALSE)
    names(data.df) <- dbGetQuery(db, "describe top_chunks;")$Field
    dbWriteTable(db, "top_chunks", data.df, append=TRUE, row.names=FALSE)
        
    # Loads table "top_words"
    global.id.all <- c()
    words.all <- c()
    word.ranks.all <- c()
    word.weights.all <- c()
    for(i in 1:ntopics)
    {
      global.id <- make.global.id(i)
      word.weights <- norm.topic.words[i,]
      word.ranks <- rank(-word.weights, ties.method = "first")
      word.nums <- which(word.ranks <= 20)
      words <- vocab[word.nums]
      word.weights <- word.weights[word.nums]
      word.ranks <- word.ranks[word.nums]
      len.data <- length(word.nums)
      
      global.id.all <- c(global.id.all, rep(global.id, len.data))
      words.all <- c(words.all, words)
      word.ranks.all <- c(word.ranks.all, word.ranks)
      word.weights.all <- c(word.weights.all, word.weights)
    }

    data.df <- data.frame(global.id.all, words.all, word.ranks.all, word.weights.all, stringsAsFactors = FALSE)
    names(data.df) <- dbGetQuery(db, "describe top_words;")$Field
    dbWriteTable(db, "top_words", data.df, append=TRUE, row.names=FALSE)
  }
    
  #prepares work abbreviations for scatterplot
  check.abbs.content <- TRUE
  while(check.abbs.content)
  {
    check.abbs <- TRUE
    while(check.abbs)
    {
      abbs.in <- scan(abbs.path, what = "character", sep = '\n')
      abbs.l <- strsplit(abbs.in, " ")
      invalid.entries <- which(sapply(abbs.l, function(l) length(l) < 2))
      if(length(invalid.entries > 0))
      {
        abbs.l <- abbs.l[-invalid.entries]
      }

      if(length(abbs.l) == 0)
      {
        cat(paste("No valid abbreviations were found in ", abbs.path, "\n", sep=""), fill=80)
        cat("Abbreviations should take up one line per work, in this format:\n")
        cat("GB GostaBerling\n\n")
        read.out <- readline(paste("Press enter when ", abbs.path, "\ncontains valid abbreviations. "))
      } 

      else 
      {
        check.abbs <- FALSE
      }
    }

    abbs <- sapply(abbs.l, function(l) l[[1]])
    works.regex <- sapply(abbs.l, function(l) l[[2]])
    invalid.abbs <- c()
    works.start <- c()
    works.end <- c()
    for(ri in 1:length(works.regex))
    {
      r <- works.regex[ri]
      locations <- grep(r, doc$id)
      if(length(locations) == 0)
      {
        cat(paste("Warning: No files matching '", r, "' found.\n"))
        invalid.abbs <- c(invalid.abbs, ri)
      } 

      else 
      {
        works.start <- c(works.start, min(locations))
        works.end <- c(works.end, max(locations))
      }

      if(length(invalid.abbs) > 0)
      {
        abbs <- abbs[-invalid.abbs]
      }
    }

    if(length(abbs) == 0)
    {
      cat(paste("None of the abbreviations in\n", abbs.path, "\nmatch the chunks in\n", getwd(), '\n', sep=""), fill=80)
      read.out <- readline("Press enter when the abbreviations have been corrected. ")
    } 

    else 
    {
      check.abbs.content <- FALSE
    }
  }
  
  if(use.db)
  {
    # Initializes population of "images" table
    global.ids <- make.global.id(collective = TRUE)
  }
  
  # filenames for collective images
  scatter.names <- paste(tag, "ScatterPlots_", ntopics, ".png", sep="")
  cloud.names <- paste(tag, "WordClouds_", ntopics, ".png", sep="")
  uCloud.names <- paste(tag, "uWordClouds_", ntopics, ".png", sep="")
  
  #creates collective scatterplot file
  pname <- paste(outputdir, '/', scatter.names[1] ,sep="")
  png(pname, width=3.5, height=3.5, units="in", res=300)
  for(k in 1:ceiling(ntopics/5))
  {
    if(k < ceiling(ntopics/5))
    {
      topics <- ((1+5*(k-1)):(5*k))
    }

    else
    {
      topics <- ((1+5*(k-1)):ntopics)
    }

    n.g <- length(topics)
    par(mfrow=c(n.g,1), mai=c(0.3,1,0.3,1))
    for(i in topics.order[topics])
    {
      if(i %in% recog.topics)
      {
        title <- paste(topics.ah.names[i], i, sep=" ")
      } 

      else 
      {
        title <- i
      }
      makeplot(title,i,norm.doc.topics,works.start,works.end,abbs)
    }
  }
  dev.off()
  
  #creates collective wordcloud file
  pname <- paste(outputdir, '/', cloud.names[1], sep="")
  png(pname, width=3.5, height=3.5, units="in", res=300)
  for(k in 1:ceiling(ntopics/6))
  {
    if(k < ceiling(ntopics/6)){
      topics <- ((1+6*(k-1)):(6*k))
    }

    else
    {
      topics <- ((1+6*(k-1)):ntopics)
    }

    par(mfrow=c(3,2))
    for(i in topics.order[topics])
    {
      if(i %in% recog.topics)
      {
        title <- paste(topics.ah.names[i], i, sep=" ")
      } 

      else 
      {
        title <- i
      }
      wcloud(vocab,topic.words[i,],title)
    }
  }
  dev.off()
  
  #creates collective unique wordcloud file
  spec.l.all <- topic.u.words(topic.words, ntopics, vocab)
  spec.l <- spec.l.all[[1]]
  spec.c.l <- spec.l.all[[2]]
  pname <- paste(outputdir, '/', uCloud.names[1], sep="")
  png(pname, width=3.5, height=3.5, units="in", res=300)
  for(k in 1:ceiling(ntopics/6))
  {
    if(k < ceiling(ntopics/6))
    {
      topics <- ((1+6*(k-1)):(6*k))
    }

    else
    {
      topics <- ((1+6*(k-1)):ntopics)
    }

    par(mfrow=c(3,2))
    for(i in topics.order[topics])
    {
      if(i %in% recog.topics)
      {
        title <- paste(topics.ah.names[i],"u", i, sep="")
      } 

      else 
      {
        title <- paste("u",i,sep="")
      }
      wcloud(spec.l[[i]],spec.c.l[[i]],title)
    }
  }
  dev.off()
  
  # make individual files
  for(i in 1:ntopics)
  {
    maxwidth <- nchar(as.character(ntopics))
    i.c <- formatC(i, width=maxwidth, format='d', flag=0)
    # individual filename templates
    scatter.name <- paste(tag, "SP_", i.c, 'of', ntopics, ".png", sep="")
    cloud.name <- paste(tag, "WC_", i.c, 'of', ntopics, ".png", sep="")
    uCloud.name <- paste(tag, "UWC_", i.c, 'of', ntopics, ".png", sep="")
    
    if(use.db)
    {
      global.ids <- c(global.ids, make.global.id(i))
    }

    scatter.names <- c(scatter.names, scatter.name)
    cloud.names <- c(cloud.names, cloud.name)
    uCloud.names <- c(uCloud.names, uCloud.name)
    
    # individual scatterplots
    pname <- paste(outputdir, '/', scatter.name, sep="")
    png(pname, width=9, height=3.5, units="in", res=300)
    makeplot(i,i,norm.doc.topics,works.start,works.end,abbs)
    dev.off()
    
    # individual wordclouds
    pname <- paste(outputdir, '/', cloud.name, sep="")
    png(pname, width=3.5, height=3.5, units="in", res=300)
    wcloud(vocab,topic.words[i,],i)
    dev.off()
    
    # individual unique wordclouds
    pname <- paste(outputdir, '/', uCloud.name, sep="")
    png(pname, width=3.5, height=3.5, units="in", res=300)
    wcloud(spec.l[[i]],spec.c.l[[i]],title)
    dev.off()
  }
  
  if(use.db)
  {
    # Loads table "images"
    images.df <- data.frame(global.ids, scatter.names, cloud.names, uCloud.names, stringsAsFactors = FALSE)
    names(images.df) <- dbGetQuery(db, "describe images;")$Field
    dbWriteTable(db, "images", images.df, append=TRUE, row.names=FALSE)
  }
}

if(use.db)
{ 
  read.out <- dbDisconnect(db) 
}

setwd(wd)