createMortData1 = function(year_start,year_end,age_start,age_end,sex,sex_cat){
  
  # function to import multiple data files 
  read.multiple.files = function(path,pattern,DEL){
    # note: DEL: "death"/"exposure"/"lifetable"
    list.filenames = list.files(path,pattern)
    
    list.data = list()
    
    for (i in 1:length(list.filenames)){
      state = sub(paste(DEL,"-",sep=" ")," ",list.filenames[i])
      state = sub("-.*"," ",state)
      current.data = read.table(paste(path,list.filenames[i],
                                      sep="/"),header=TRUE)
      current.data$State = state
      list.data[[i]] = current.data
    }
    out = do.call(rbind,list.data)
    
    return(out)
  }
  
  # import death text files:
  D = read.multiple.files("Death", pattern=".txt$", DEL = "death1")
  D = data.table::as.data.table(D)
  
  # import exposure text files:
  E = read.multiple.files("Exposure", pattern=".txt$", DEL = "exposure1")
  E = data.table::as.data.table(E)
  
  # convert Age in both files to numeric format:
  D$Age = as.character(D$Age); D = D[Age!="110+"]; D$Age = as.numeric(D$Age)
  E$Age = as.character(E$Age); E = E[Age!="110+"]; E$Age = as.numeric(E$Age)
  
  
  # if sex_cat="yes" then sex is added as categorical variable:
  if (sex_cat=="yes"){
    Dl = melt(D,id.vars=c("Year","Age","State","Total")); El = melt(E,id.vars=c("Year","Age","State","Total"))
    Dl = Dl[Year %in% year_start:year_end & Age %in% age_start:age_end]
    El = El[Year %in% year_start:year_end & Age %in% age_start:age_end]
    rate = Dl$value/El$value
    dat = cbind(Dl[,.(Year,Age,State,variable)],rate)
    dat = dat[,y:=log(rate)]
    names(dat)[names(dat)=="variable"]="sex"
    dat = dat[order(State,Year,Age,sex),.(Year,Age,State,sex,rate,y)]
    levels(dat$sex) = c("0","1"); dat$sex=as.numeric(dat$sex)-1
    # Female is coded as 0 and Male is code as 1
    names(dat) = tolower(names(dat))
  }
  else if (sex_cat=="no"){
    if (sex=="female"|sex=="Female"|sex=="F"|sex=="f"){
      D = D[Year %in% year_start:year_end & Age %in% age_start:age_end,.(Year,Age,Female,State)]
      E = E[Year %in% year_start:year_end & Age %in% age_start:age_end,.(Year,Age,Female,State)]
      rate = D$Female/E$Female}
    
    else if (sex=="male"|sex=="Male"|sex=="M"|sex=="m"){
      D = D[Year %in% year_start:year_end & Age %in% age_start:age_end,.(Year,Age,Male,State)]
      E = E[Year %in% year_start:year_end & Age %in% age_start:age_end,.(Year,Age,Male,State)]
      rate = D$Male/E$Male}
    
    else {
      D = D[Year %in% year_start:year_end & Age %in% age_start:age_end,.(Year,Age,Total,State)]
      E = E[Year %in% year_start:year_end & Age %in% age_start:age_end,.(Year,Age,Total,State)]
      rate = D$Total/E$Total}
    
    dat = cbind(D[,.(Year,Age,State)],rate)
    dat = dat[,y:=log(rate)]
    dat = dat[order(State,Age,Year),.(Age,Year,State,rate,y)]
    names(dat) = tolower(names(dat))
  }
  else stop ("sex_cat must be either yes or no")
  
  return(dat)
}