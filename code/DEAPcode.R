##########################
##  user customization  ##
##########################
TEST = FALSE
# Change the coding from factor levels to numbers
#levels(data$physical_activity1_y) = c("", 0:7);
#data$physical_activity1_y = as.numeric(as.character(data$physical_activity1_y))

# Change the reference category of a factor level variable
# data$sex <- factor(data$sex, levels=c("M","F"))

print(toJSON(inputs))

if (!('gamm4'     %in% installed.packages()[,"Package"])) install.packages('gamm4')
if (!('rjson'     %in% installed.packages()[,"Package"])) install.packages('rjson')
if (!('stargazer' %in% installed.packages()[,"Package"])) install.packages('stargazer')
if (!('knitr'     %in% installed.packages()[,"Package"])) install.packages('knitr')
if (!('MuMIn'     %in% installed.packages()[,"Package"])) install.packages('MuMIn')
if (!('R.matlab'  %in% installed.packages()[,"Package"])) install.packages('R.matlab')
if (!('tableone'  %in% installed.packages()[,"Package"])) install.packages('tableone')
if(!"gamm4" %in% .packages())     library(gamm4)
if(!"rjson" %in% .packages())     library(rjson)
if(!"stargazer" %in% .packages()) library(stargazer)
if(!"knitr" %in% .packages())     library(knitr)
if(!"MuMIn" %in% .packages())     library(MuMIn)
if(!"R.matlab" %in% .packages())  library(R.matlab)
if(!"tableone" %in% .packages())  library(tableone)

extract.variables = function(a){
  rt = c()
  for (l in 1:length(a) ){ 
    if(length(a[[l]]) > 0){
      for(e in 1:length(a[[l]])){
        if(unlist(a[[l]][[e]]) != "")
          rt = c(rt, unlist(a[[l]][[e]]))
      } 
    }
  }
  rt_inster = c()
  for( item in 1:length(rt)){
    if(!is.character(rt[item])){
      next
    }
    if(length(unlist(strsplit(rt[item],"[*]"))) > 1){
      rt_inster = c(rt_inster, unlist(strsplit(rt[item],"[*]")))
    }
    else if(length(unlist(strsplit(rt[item],"[+]"))) > 1){
      rt_inster = c(rt_inster, unlist(strsplit(rt[item],"[+]")))
    }
    
    else if(length(unlist(strsplit(rt[item],"^2", fixed=TRUE))) > 1){
      rt_inster = c(rt_inster, unlist(strsplit(rt[item],"^2", fixed=TRUE)))
    }
    else if( length(regmatches(rt[item], gregexpr("(?<=\\().*?(?=\\))", rt[item], perl=T))[[1]] ) > 0 ){
      
      rt_inster = c(rt_inster, regmatches(rt[item], gregexpr("(?<=\\().*?(?=\\))", rt[item], perl=T))[[1]])
    }else{
      rt_inster = c(rt_inster, rt[item])
    }
  }
  rt = rt_inster
  return(rt);
}

is.user.score = function(name){
  rt = FALSE
  name = paste0("/var/www/html/data/ABCD/Scores/data/", strsplit(username,"_")[[1]][1],"/",name,".rds")
  if(file.exists(name))
    rt = TRUE
  return(rt)
}

is.user.score.admin = function(name){
  rt = FALSE
  name = paste0("/var/www/html/data/ABCD/Scores/data/admin/",name,".rds")
  if(file.exists(name))
    rt = TRUE
  return(rt)
}

if(length(unlist(inputs[['dep.var.']])) == 0 ){
  stop("Dependent variable is empty.")
}



varList.initial = extract.variables(inputs)
#if smooth by variables, need to split first
if(sum(grepl("by =",varList.initial))>=1){
  s.by.split = varList.initial[grepl("by =",varList.initial)]
  split.vars = unlist(strsplit(s.by.split, ", by = "))
  varList.initial = c(varList.initial,split.vars)
  varList.initial = varList.initial[!duplicated(varList.initial)]
}

vars.in.data = varList.initial[varList.initial %in% names(data)]
vars.keep = c("src_subject_id","eventname","rel_family_id","abcd_site",vars.in.data)
vars.keep = vars.keep[!duplicated(vars.keep)]
data = data[,vars.keep]

for( name in extract.variables(inputs)){
  if ( name %in% names(data)){
    next;
  }else if ( is.user.score(name)){
    user_data =  readRDS( paste0("/var/www/html/data/ABCD/Scores/data/",strsplit(username,"_")[[1]][1],"/",name,".rds"))
    data = merge(data, user_data);
  }else if ( is.user.score.admin(name)){
    user_data =  readRDS( paste0("/var/www/html/data/ABCD/Scores/data/admin/",name,".rds"))
    data = merge(data, user_data);
  }
  #else {
  #    stop(paste( name,"does not exist."))
  #}
  
  
}
##################
##  functions   ##
##################

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

sep.vars = function(x){
  x = gsub(" ", "", x, fixed = TRUE)
  x = unlist(strsplit(x,"+",fixed=T))
  return(x)
}

censor =  function(x, fraction=.005){
  if(length(fraction) != 1 || fraction < 0 || fraction > 1){
    stop("bad value for 'fraction'")
  }
  lim <- quantile(x, probs=c(fraction/2, 1-fraction/2), na.rm = T)
  x[ x < lim[1] ] <- NA
  x[ x > lim[2] ] <- NA
  x
}

#########################
##  data  extraction   ##
#########################

trigger.warning = F

#exit script silently if @inputs is empyt
empty = T;
for (key in names(inputs)){
  #print(inputs[[key]]);
  #print(length(inputs[[key]]))
  if(length(inputs[[key]]) != 0 && inputs[[key]] != ""){
    empty = F;
  }
}

if(empty){
  options(warn=-1)
  opt <- options(show.error.messages=FALSE) 
  on.exit(options(opt)) 
  stop() 
}

#By using Rserve gamm4 is already loaded

### censor/windsorize first
wsVar = unlist(inputs[['ws.var']])
wsVar = sep.vars(wsVar)
if(length(wsVar)>0){
  for(ii in 1:length(wsVar)){
    if(class(data[,wsVar[ii]]) == "numeric"){
      data[,wsVar[ii]] = censor(data[,wsVar[ii]])
    }
  }
}

dependendVar   = unlist(inputs[['dep.var.']]);
dependendVar.name = NULL

if(length(dependendVar) == 0 ){
  stop("Dependent variable is empty.")
}


##if y is log-transformed...
if(substring(dependendVar,1,4) == "log("){
  dependendVar.name = substring(dependendVar,5,nchar(dependendVar)-1)
  if(sum(data[[dependendVar.name]] <= 0, na.rm=T) > 0){
    data[[dependendVar.name]][data[[dependendVar.name]] <= 0] = NA
    trigger.warning = T
    warning.logging.0 = paste0("1 or more log transformed variable contains values <=0. All <=0 values replaced with NA.")
  }
  data$Y.log = log(data[[dependendVar.name]])
  new.name = paste0("log.",dependendVar.name)
  names(data)[names(data) == "Y.log"] = new.name
  dependendVar = new.name
}

if( dependendVar %in% names(data)){
  if(is.factor(data[[dependendVar]])){
    if(nlevels(data[[dependendVar]]) > 2){
      stop("Categorical variables with more than 2 levels are not supported as dependent variables. \nConsider converting your categorical variable into a continuous variable.")
    }
  }
} else {
  stop(paste("Dependent variable <", dependendVar ,">does not exist in the database"));
}


if( dependendVar %in% names(data)){
  if(is.factor(data[[dependendVar]])){
    if(nlevels(data[[dependendVar]]) > 2){
      stop("Categorical variables with more than 2 levels are not supported as dependent variables. \nConsider converting your categorical variable into a continuous variable.")
    }
  }
} else {
  stop(paste("Dependent variable <", dependendVar ,">does not exist in the database"));
}


independendVar = unlist(inputs[['ind.var.']]);
usercovVar     = paste(unlist(inputs[['usercov.']]),  sep='+')

smoothVar.all = unlist(inputs[['smo.var']])
smoothVar.all = sep.vars(smoothVar.all)

logVar = unlist(inputs[['log.var']])
logVar = sep.vars(logVar)
#check if 0's in logged vars. if so, add 0.0001
strip.log = substring(logVar,5,nchar(logVar)-1)
if(length(strip.log)>0){
  if(sum(data[,strip.log]<=0 , na.rm=T) > 0){
    trigger.warning = T
    warning.logging.0 = paste0("1 or more log transformed variable contains values <=0. All <=0 values replaced with NA.")
    for(ii in 1:length(strip.log)){
      log.var_i = strip.log[ii]
      if(sum(data[,log.var_i]<=0 , na.rm=T) > 0){
        #data[[log.var_i]][data[[log.var_i]] == 0] = 0.0001
        data[[log.var_i]][data[[log.var_i]] <= 0] = NA
        
      }
    }
  }
}

interactionVar = unlist(inputs[['int.var']])

sqVar = unlist(inputs[['sq.var']])
sqVar = sep.vars(sqVar)
sqVar = substring(sqVar,1,nchar(sqVar)-2)

sqVar_SQUARED = NULL
if(length(sqVar)>0){
  for(ii in 1:length(sqVar)){
    sqVar_SQUARED[ii] = paste0(sqVar[ii],"_SQUARED")
    data[,sqVar_SQUARED[ii]] = data[,sqVar[ii]]^2
  }
}

groupVar = unlist(inputs[['gr.var']])
if(length(groupVar) > 0){
  if(is.character(groupVar) & nchar(groupVar) ==0){
    groupVar = NULL 
  } 
}

subsetVar   = unlist(inputs[['fl.var']]);
if(length(subsetVar) > 0){
  if(is.character(subsetVar) & nchar(subsetVar) ==0){
    subsetVar = NULL 
  } 
}
#Interacting grouping variable with independent variable
if(length(groupVar)>0){
  #may need to change it instead of searching for the string, it strips string first and looks for exact match
  is.smooth = grepl(independendVar, smoothVar.all)
  is.log =    independendVar %in% substring(logVar,5,nchar(logVar)-1)
  is.square = independendVar %in% sqVar
  ## If independent var is smooth
  if( sum(is.smooth) > 0 ){
    #replace s(independendVar) with s(independendVar, by = groupVar)
    smoothVar.all[is.smooth] = paste0("s(", independendVar,",by=",groupVar, ")")
  } else if(is.square){
    #else if squared, add var*groupvar and var_SQUARED*groupvar
    interactionVar = c(interactionVar, paste0(independendVar,"*",groupVar), paste0(independendVar,"_SQUARED*",groupVar) )
  } else if(is.log){    
    # else if log, interact with log(var)
    log.independent = logVar[independendVar == substring(logVar,5,nchar(logVar)-1)]
    interactionVar = c(interactionVar, paste0(log.independent,"*",groupVar) )
  } else {
    # else make normal interaction
    interactionVar = c(interactionVar, paste0(independendVar,"*",groupVar) )
  }
}  #may need to do another if with ^2 independent variables
smoothVarInt.ind = grepl(",by=", smoothVar.all)
smoothVarInt = smoothVar.all[smoothVarInt.ind]
smoothVar =    smoothVar.all[!smoothVarInt.ind]

if(0 %in% nchar(sqVar)) sqVar = character()

print(sqVar)
print(paste("length sqVar",length(sqVar)))
#nestVar = c("Site", "FamilyID")
#usercovVar =  usercovVar[!(usercovVar %in% nestVar)]

#TODO: seperate Site and Familiy to another catagory of random effect

#if(include.random.site){
#  inputs[['cov.fixed']][[which(unlist(inputs[['cov.fixed']]) == "abcd_site")]] = NULL
#}
#inputs[['cov.fixed']][[which(unlist(inputs[['cov.fixed']]) == "rel_family_id")]] = NULL

covfixedVar    = paste(unlist(inputs[['cov.fixed']]), sep='+')

smoothVarInt.stripped.term1 = ""
smoothVarInt.stripped.term2 = ""


if(length(smoothVarInt)>0){
  smoothVarInt.stripped.term1 = unlist(lapply( strsplit(smoothVarInt,","), function(x)x[[1]]))
  smoothVarInt.stripped.term1 = substring(smoothVarInt.stripped.term1,3,nchar(smoothVarInt.stripped.term1))
  smoothVarInt.stripped.term2 = unlist(lapply( strsplit(smoothVarInt,"by="), function(x)x[[2]]))
  smoothVarInt.stripped.term2 = substring(smoothVarInt.stripped.term2,1,nchar(smoothVarInt.stripped.term2)-1)
}

### if usercovVar's have been transformed, then they are stored in usercovVar as well as the 
### transformed vars (smoothVar, logVar, etc.), in which they will need to be removed from
### usercovVar before putting into formula
print(paste("before remove",usercovVar))
print(c( substring(smoothVar,3,nchar(smoothVar)-1),
         smoothVarInt.stripped.term1,
         smoothVarInt.stripped.term2,
         substring(logVar,5,nchar(logVar)-1),
         sqVar ))
cov.ind.remove = usercovVar %in% c( substring(smoothVar,3,nchar(smoothVar)-1),
                                    smoothVarInt.stripped.term1,
                                    smoothVarInt.stripped.term2,
                                    substring(logVar,5,nchar(logVar)-1),
                                    sqVar )
if(sum(cov.ind.remove)>0 ){
  usercovVar = usercovVar[!cov.ind.remove]
}
print(paste("after remove",usercovVar))

#remove covfixedVar when it is a smooth, smooth interaction (first variable), or log -- should usually only be age of covfixedVar
#ok to keep it in as interaction, squared, or smooth interaction term
covfixedVar.ind.remove = covfixedVar %in% c( substring(smoothVar,3,nchar(smoothVar)-1),
                                             smoothVarInt.stripped.term1,
                                             substring(logVar,5,nchar(logVar)-1))
if(sum(covfixedVar.ind.remove)>0 ){
  covfixedVar = covfixedVar[!covfixedVar.ind.remove]
}


form_arr = c(independendVar, usercovVar,covfixedVar, smoothVar.all, logVar, interactionVar, sqVar, sqVar_SQUARED);
#form_arr = c(independendVar, usercovVar,covfixedVar, smoothVar, logVar, interactionVar, sqVar, sqVar_SQUARED);
#form_arr = c(independendVar, usercovVar,covfixedVar, smoothVar, logVar, interactionVar, sqVar, sqVar_SQUARED, groupVar);

### similarly for the independent variable...
#if independent variable is a smooth variable, log variable, or squared variable remove independendendVar from form_arr
if(independendVar %in% c(  substring(smoothVar,3,nchar(smoothVar)-1),
                           smoothVarInt.stripped.term1,
                           substring(logVar,5,nchar(logVar)-1),
                           sqVar ) ){
  form_arr = c(usercovVar,covfixedVar, smoothVar.all, logVar, interactionVar, sqVar, sqVar_SQUARED);
  #form_arr = form_arr[form_arr != independendVar]
}
form_arr = form_arr[form_arr!=""]
#########################
##  remove duplicates  ##
#########################
#take out duplicate variables
form_arr = form_arr[!duplicated(form_arr)]
formulastr = paste(dependendVar," ~ ",paste(form_arr,collapse='+'))
#get variables involve in the formula
varList = all.vars(as.formula(formulastr));
varList.independent = varList[-1]

#trigger.warning = F
#take out duplicated variables according to their values
if(length(varList.independent)>1){
  var.combin = combn(varList.independent,2)
  identical.values = c()
  for(ii in 1:ncol(var.combin)){
    vars_i = var.combin[,ii]
    #if two variables perfectly correlated, store both variables
    if(cor(as.numeric(data[,vars_i[1]]),as.numeric(data[,vars_i[2]]), use = "complete.obs") %in% c(1,-1)){
      # identical.values[ii] = list(vars_i)
      identical.values = c(identical.values,list(vars_i))
    }
  }
  vars.duplicate.remove = c()
  if(length(identical.values) > 0){
    for(ii in 1:length(identical.values)){
      vars_both_i = identical.values[[ii]]
      #if independendVar is in vars_both, remove other variable, otherwise doesn't matter which is removed
      if(independendVar %in% vars_both_i){
        vars.duplicate.remove[ii] = vars_both_i[vars_both_i != independendVar]
      } else{
        vars.duplicate.remove[ii] = vars_both_i[2]
      }
    }
    trigger.warning = T
    warning.duplicates = paste0("perfectly correlated variables detected, removed variable '", vars.duplicate.remove, "'")
    form_arr = form_arr[!form_arr %in% vars.duplicate.remove]
    formulastr = paste(dependendVar," ~ ",paste(form_arr,collapse='+'))
    varList = all.vars(as.formula(formulastr));
    varList.independent = varList[-1]
  }
}

print(varList)
print(formulastr)

#########################
##  data manipulation  ##
#########################
#data = data[data$eventname == "baseline_year_1_arm_1",]
print(summary(data[[independendVar]]))
#if independent variable has 5 or less unique values change it to character/factor variable
categorical.independent = FALSE
#if( length(table(data[[independendVar]])) < 6 ){
#  data[[independendVar]] = as.character(data[[independendVar]])
#  categorical.independent = TRUE
#} else{
#  data[[independendVar]] = as.numeric(as.character(data[[independendVar]]))
#}

if(class(data[[independendVar]]) != "numeric"){
  categorical.independent = TRUE
}

#user defined covariates
#for(ucov in unlist(inputs[['usercov.']])){
#  data[[ucov]] = as.numeric(as.character(data[[ucov]]))
#}

print(summary(data[[independendVar]]))
#determine if logistic regression or not
categorical.dependent = FALSE
data[[dependendVar]][data[[dependendVar]] == ""] = NA
if( length(table(data[[dependendVar]])) == 2 ){
  data[[dependendVar]] = as.factor(data[[dependendVar]])
  categorical.dependent = TRUE
} else{
  data[[dependendVar]] = as.numeric(as.character(data[[dependendVar]]))
}
#data[[dependendVar]] = as.numeric(as.character(data[[dependendVar]]))

#if("demo_prnt_marital_v2" %in% colnames(data)){
# Type of household
# 1 = Married, 6 = Living with a partner
# 2 = Widowed, 3 = Divorced, 4 = Separated, 5 = Never married
#  marital_v2.old = data$demo_prnt_marital_v2
#  data$demo_prnt_marital_v2 = NA
#  data$demo_prnt_marital_v2[marital_v2.old %in% c(1,6)] = 1
#  data$demo_prnt_marital_v2[marital_v2.old %in% 2:5] = 0
#}

# if("gender" %in% colnames(data)){
#   data = data[data$gender %in% c("M","F"),]
# }
#if("race.ethnicity" %in% colnames(data)){
#  data$race.ethnicity = as.factor(data$race.ethnicity)
#}
#income
##NDA using demo_prtnr_income_v2
#if("demo_comb_income_v2" %in% colnames(data)){
#  data$hhinc = NA
#  data$hhinc[data$demo_comb_income_v2 %in% 1:6]  = "[<50K]"
#  data$hhinc[data$demo_comb_income_v2 %in% 7:8]  = "[>=50K&<100K]"
#  data$hhinc[data$demo_comb_income_v2 %in% 9:10] = "[>=100K]"
#  data$demo_comb_income_v2 = data$hhinc
#}
if("household.income.bl" %in% names(data)){
  data$household.income.bl = as.character(data$household.income.bl)
  data$household.income.bl[data$household.income.bl == "[>=50K & <100K]"] = "[>=50K& <100K]"
  data$household.income.bl = as.factor(data$household.income.bl)
}



#data = data[c("src_subject_id","rel_family_id","abcd_site",varList)]
##################
##  subset data ##
##################
if(length(subsetVar)>0){
  json_data = rjson::fromJSON(file = subsetVar);
  subset = data.frame(src_subject_id=unlist(lapply(json_data[[1]]$set,function(d){ d[1] })), eventname=unlist(lapply(json_data[[1]]$set,function(d){d[2]})));
  data = merge(subset, data, all.x = T, all.y = F)
}


##################################################
##  identify variables of different timepoints  ##
##################################################
####before complete.cases, need to figure out which variables are in which events, and potentially transform


# var.timepoints.with.data = matrix(NA, nrow = length(varList), ncol =  length(unique(data$eventname))+1)
# var.timepoints.with.data[,1] = varList
var.timepoints.with.data = list()
for(ii in 1:length(varList)){
  var_i = varList[ii]
  valid.data.per.timepoint = aggregate(data[var_i], list(data$eventname), function(x)sum(!is.na(x)))
  timepoints.with.data = as.character(valid.data.per.timepoint$Group.1[valid.data.per.timepoint[var_i] != 0])
  # var.timepoints.with.data[ii,(1:length(timepoints.with.data))+1] = timepoints.with.data
  var.timepoints.with.data[[ii]] = timepoints.with.data
}
n.timepoints.per.var = unlist(lapply(var.timepoints.with.data,length))

dep.timepoints = unlist(var.timepoints.with.data[dependendVar == varList])
indep.timepoints = unlist(var.timepoints.with.data[independendVar == varList])

#if DEPENDENT is follow-up only (no baseline visits), and INDEPENDENT is baseline only, shift DEPENDENT rows to baseline
if(sum(dep.timepoints != "baseline_year_1_arm_1") >= 1 & sum(dep.timepoints == "baseline_year_1_arm_1") == 0 & 
   sum(indep.timepoints != "baseline_year_1_arm_1") == 0 & sum(indep.timepoints == "baseline_year_1_arm_1") == 1){
  
  data.dep = data[data$eventname == dep.timepoints , c("src_subject_id","eventname",dependendVar)]
  data.dep$eventname[data.dep$eventname == dep.timepoints] = indep.timepoints
  data[[dependendVar]] = NULL
  data = merge(data, data.dep, by = c("src_subject_id","eventname"), all = T)
}

###################################################


data = data[complete.cases(data),]

print(dim(data))

###################################################
##  remove columns from model if 1 unique value  ##
###################################################

#less.than.2.levels = sapply(data , function(x) length(unique(x)) ) < 2
less.than.2.levels = sapply(data[,varList] , function(x) length(unique(x)) ) < 2

#trigger.warning = F
if(sum(less.than.2.levels) >0 ){
  trigger.warning = T
  #vars.to.remove = names(data)[less.than.2.levels]
  vars.to.remove = names(less.than.2.levels)[less.than.2.levels]
  
  
  #stop script if dependent variable has less than 2 unique values
  if(dependendVar %in% vars.to.remove){
    stop(paste0("'", dependendVar,"'" ," variable has <2 unique values"))
  }
  if(independendVar %in% vars.to.remove){
    stop(paste0("'", independendVar,"'" ," variable has <2 unique values"))
  }
  
  #remove vars.to.remove from form_arr, formulastr, varList, and varList.independent
  form_arr = form_arr[!form_arr %in% vars.to.remove]
  formulastr = paste(dependendVar," ~ ",paste(form_arr,collapse='+'))
  
  varList = all.vars(as.formula(formulastr));
  varList.independent = varList[-1]
  
  warning.1.unique.value = paste0("'", vars.to.remove,"'" ," variable has <2 unique values - removed from model")
}

########################################################################
##  remove independendVar (& groupVar) from formula to get delta R^2  ##
########################################################################

run.effect.size = T

if(run.effect.size){
  
  
  logVar.stripped    = substring(logVar,5,nchar(logVar)-1)
  smoothVar.stripped = substring(smoothVar,3,nchar(smoothVar)-1)
  
  if(length(interactionVar)>0){
    ##INTERACTION
    interaction.stripped.list = strsplit(interactionVar,"*",fixed=T)
  } else{
    interaction.stripped.list = character()
  }
  
  
  if(independendVar %in% smoothVar.stripped){
    ##SMOTH
    smoothVar.remove = smoothVar[smoothVar.stripped == independendVar]
    form_arr2 = form_arr[!(form_arr %in% smoothVar.remove)]
  } else if(independendVar %in% smoothVarInt.stripped.term1){
    ##SMOTH INTERACTION
    smoothVarInt.remove = smoothVarInt[smoothVarInt.stripped.term1 == independendVar]
    form_arr2 = form_arr[!(form_arr %in% smoothVarInt.remove)]
  } else if(independendVar %in% logVar.stripped){
    ##LOG
    logVar.remove = logVar[logVar.stripped == independendVar]
    form_arr2 = form_arr[!(form_arr %in% logVar.remove)]
    if(length(groupVar)>0){
      logVarinteraction.remove = c(paste0(logVar.remove,"*",groupVar) , groupVar)
      form_arr2 = form_arr2[!(form_arr2 %in% logVarinteraction.remove)]
    }
  } else if(independendVar %in% sqVar){
    ##SQUARED
    sqVar.remove = c(independendVar, paste0(independendVar,"_SQUARED"))
    form_arr2 = form_arr[!(form_arr %in% sqVar.remove)]
    if(length(groupVar)>0){
      sqVarinteraction.remove = c(paste0(sqVar.remove,"*",groupVar), groupVar)
      form_arr2 = form_arr2[!(form_arr2 %in% sqVarinteraction.remove)]
    }
  } else if(independendVar %in% unlist(interaction.stripped.list)){
    ##INTERACTION
    independ.location = c()
    for(ii in 1:length(interaction.stripped.list)){
      int.stripped.list_i = interaction.stripped.list[[ii]]
      independ.location[ii] = sum(independendVar == int.stripped.list_i)
    }
    interaction.remove = interactionVar[independ.location == 1]
    interactionVars.remove = unlist(strsplit(interaction.remove,"*",fixed=T))
    all.interactionVars.remove = c(interaction.remove,interactionVars.remove)
    form_arr2 = form_arr[!(form_arr %in% all.interactionVars.remove)]
  } else {
    #NO INDEPENDENT VAR TRANSFORMATION
    form_arr2 = form_arr[!(form_arr %in% independendVar)]
  }
  
  if(length(form_arr2) > 0){
    formulastr2 = paste(dependendVar," ~ ",paste(form_arr2,collapse='+'))
  } else{
    formulastr2 = paste(dependendVar," ~ 1")
  }
  
  
}

########################################
##  make sure log doesn’t create NAs  ##
########################################

# if(length(logVar.stripped)>0){
#   for(ii in 1:length(logVar.stripped)){
#     min.log_i = min(data[[logVar.stripped[ii]]])
#     if(min.log_i < 0)  
#       data[[logVar.stripped[ii]]] = data[[logVar.stripped[ii]]] - min.log_i + 1
#   }
# }



print("Before calling the model")



######################
##  random effects  ##
######################
include.random.scanner = "mri_info_deviceserialnumber" %in% unlist(inputs$rand.var)
include.random.site    = "abcd_site" %in% unlist(inputs$rand.var)
include.random.subject = "src_subject_id" %in% unlist(inputs$rand.var)

#IF less than 2 unique "eventname" & "include.random.subject" toggled on, 
#   need to toggle off random subject effect and produce warning (or error)
if(length(unique(data$eventname)) <2 & include.random.subject){
  include.random.subject = FALSE
  trigger.warning = T
  warning.1.unique.eventname = paste0("Your model data contains a single data point per subject (event name “baseline_year_1_arm_1”) only. To prevent problems with model convergence suggest to remove SUBJECT from the list of random effects. It is not required with this particular cross-sectional analysis.")
}
#IF 2+ unique "eventname" & "include.random.subject" toggled off, 
#   need to toggle on random subject effect and produce warning (or error)
if(length(unique(data$eventname)) > 1 & !include.random.subject){
  # include.random.subject = TRUE
  trigger.warning = T
  warning.2.unique.eventname = paste0("Your model data contains multiple data points per subject (event name “baseline_year_1_arm_1”). Suggest to include SUBJECT from the list of random effects.")
}



formula.random.str = c()
if(include.random.site & !include.random.subject){
  formula.random.str = "(1|abcd_site/rel_family_id)"
} else if(include.random.scanner & !include.random.subject){
  formula.random.str = "(1|mri_info_deviceserialnumber/rel_family_id)"
} else{
  if(include.random.site) formula.random.str[1] = "(1|abcd_site)"
  if(include.random.scanner) formula.random.str[1] = "(1|mri_info_deviceserialnumber)"
  formula.random.str[2] = "(1|rel_family_id)"
  if(include.random.subject) formula.random.str[3] = "(1|src_subject_id)"
}
formula.random.str = formula.random.str[!is.na(formula.random.str)]
formula.random.str = paste0("~",paste(formula.random.str, collapse = "+"))
formula.random = formula.random.str

#if(include.random.site == T & include.random.subject == T){
#  formula.random = "~(1|abcd_site)+(1|rel_family_id)+(1|src_subject_id)"
#} else if(include.random.site == F & include.random.subject == T){
#  formula.random = "(1|rel_family_id)+(1|src_subject_id)"
#} else if(include.random.site == T & include.random.subject == F){
#  formula.random = "~(1|abcd_site/rel_family_id)"
#} else if(include.random.site == F & include.random.subject == F){
#  formula.random = "~(1|rel_family_id)"
#}



#################
##  run model  ##
#################

if(categorical.dependent){ #logistic regression
  #model  = gamm4(as.formula(formulastr) , random = as.formula(formula.random), family = binomial , data = data)
  #model2 = gamm4(as.formula(formulastr2) , random = as.formula(formula.random), family = binomial , data = data)
  #intercept model
  #model3 = gamm4(as.formula(paste0(dependendVar, " ~1")) , random = as.formula(formula.random), family = binomial, data = data)
  
  formula_list = c(formulastr, formulastr2, paste0(dependendVar, " ~1"));
  cl <- makeCluster(3)
  registerDoParallel(3)
  model_list = foreach(ii= 1:length(formula_list)) %dopar% {
    gamm4(as.formula(formula_list[ii]), random = as.formula(formula.random), family = binomial, data = data)
  }
  print(model_list)
  model = model_list[[1]]
  model2 = model_list[[2]]
  model3 = model_list[[3]]
  stopCluster(cl)
  
  
} else{ #linear regression
  formula_list = c(formulastr, formulastr2, paste0(dependendVar, " ~1"));
  cl <- makeCluster(3)
  registerDoParallel(3)
  model_list = foreach(ii= 1:length(formula_list)) %dopar% {
    gamm4(as.formula(formula_list[ii]), random = as.formula(formula.random), data = data)
  }
  print(model_list)
  model = model_list[[1]]
  model2 = model_list[[2]]
  model3 = model_list[[3]]
  stopCluster(cl)
}

print(summary(model))
print(summary(model2$gam))

########################
##  other statistics  ##
########################
#N subjects, aic, bic, & r^2
n = summary(model$gam)$n
aic = round(AIC(model$mer),2)
bic = round(BIC(model$mer),2)
#r2   = round(summary(model$gam)$r.sq,5)
#r2_2 = round(summary(model2$gam)$r.sq,5)
r2       = round(as.numeric(r.squaredLR(model$mer,model3$mer)),5)
r2_delta = round(as.numeric(r.squaredLR(model$mer,model2$mer)),5)
aic2 = round(AIC(model2$mer),2)
bic2 = round(BIC(model2$mer),2)


#compute R^2 and delta R^2
#model.r2 = r2beta(model$mer, method = 'nsj')
#model2.r2 = r2beta(model2$mer, method = 'nsj')
#r2dt = r2dt(model.r2,model2.r2)

#r2 = round(model.r2$Rsq[model.r2$Effect ==  "Model"],5)
#r2_delta = round(r2dt$d,5)

######################
######################
##    NEW PLOTS    ###
######################
######################

#################################################################
##  1. x-y scatter plot with fit (dot plot for categorical.independent x)  ##
#################################################################

if(categorical.independent == FALSE){
  ###continuous
  #make predicted data
  n.pred.pts = 100
  xrange = max(data[[independendVar]], na.rm = T) - min(data[[independendVar]], na.rm = T) 
  #WHEN SMOOTH TERM AS INDEPENDENT VARIABLE, IT MOVES IT TO THE LAST LOCATION OF THE varList.independent
  x = seq(min(data[[independendVar]], na.rm = T) - (xrange/10),
          max(data[[independendVar]], na.rm = T) + (xrange/10),
          length.out = n.pred.pts)
  
  if(independendVar %in% strip.log & sum(x<=0)>0 ){
    x = seq(min(data[[independendVar]], na.rm = T),
            max(data[[independendVar]], na.rm = T),
            length.out = n.pred.pts)
  }
  dat_predict = as.data.frame(matrix(nrow = n.pred.pts, ncol = length(varList.independent)))
  #dat_predict = as.data.frame(matrix(nrow = n.pred.pts, ncol = length(form_arr)))
  
  ##if independent variable is not 1st spot in varList.independent, switch location with the 1st spot variable
  x.ind = which(independendVar == varList.independent)
  if(x.ind != 1){
    var.switch = varList.independent[1]
    varList.independent[1] = independendVar
    varList.independent[x.ind] = var.switch
    x.ind = 1
  }
  dat_predict[,1] = x
  
  if(length(varList.independent)>1){
    for(ii in 2:length(varList.independent)){
      # for(ii in 2:length(form_arr)){
      #var = data[[form_arr[ii]]]
      var = data[[varList.independent[ii]]]
      if(class(var) == "numeric" & length(table(var)) > 5){
        dat_predict[,ii] = as.numeric(mean(var, na.rm = T))
      } else{
        dat_predict[,ii] = Mode(var)
      }
    }
  }
  #names(dat_predict) = form_arr
  names(dat_predict) = varList.independent
  
  #change independent squared variables
  ## MAY NEED TO REDO WITH MULTIPLE INDEPENDENT VARIABLES
  independendSqVar =  paste0(independendVar,"_SQUARED")
  if(independendSqVar %in% varList.independent){
    dat_predict[,independendSqVar] = dat_predict[,independendVar]^2
  }
  
  ###ADD IN GROUPING TO PREDICTION
  levels = 1
  if(length(groupVar)>0){
    dat_predict_list = list()
    levels = unique(data[[groupVar]])
    for(ii in 1:length(levels)){
      dat_predict_i = dat_predict
      dat_predict_i[, names(dat_predict_i) == groupVar] = levels[ii]
      dat_predict_list[[ii]] = dat_predict_i
    }
    names(dat_predict_list) = levels
  } else { 
    ## if no grouping variable, set dat_predict_list as the list version of dat_predict
    dat_predict_list = list(dat_predict)
  }
  
  # if(categorical.dependent){ #logistic
  #   pred = predict(model$gam, dat_predict, se.fit = TRUE, type = "response")
  # } else{ #linear
  #   pred = predict(model$gam, dat_predict, se.fit = TRUE)
  # }
  
  #predictions for list of predict dats
  if(categorical.dependent){ #logistic
    pred = lapply(dat_predict_list, function(x) predict(model$gam, x, se.fit = TRUE, type = "response"))
  } else{ #linear
    pred = lapply(dat_predict_list, function(x) predict(model$gam, x, se.fit = TRUE))
  }
  
  #with(data, plot(cbcl_scr_syn_anxdep_t, nihtbx_picvocab_uncorrected))
  #lines(x, pred$fit)
  
  fit = list()
  upper = list()
  lower = list()
  for(ii in 1:length(levels)){
    pred_i = pred[[ii]]
    fit[[ii]] = pred_i$fit
    upper[[ii]] = pred_i$fit + pred_i$se.fit*1.96
    lower[[ii]] = pred_i$fit - pred_i$se.fit*1.96
  }
  
  # upper = pred$fit + pred$se.fit*1.96
  # lower = pred$fit - pred$se.fit*1.96
  #lines(x, upper, col = "red")
  #lines(x, lower, col = "red")
  #x,y's of points
  #plot(data[[independendVar]], as.numeric(data[[dependendVar]])-1)
  #plot(data[[independendVar]], data[[dependendVar]])
  # cols = c("red","blue")
  # for(ii in 1:length(levels)){
  #   lines(x,fit[[ii]],   col = cols[ii])
  #   lines(x,upper[[ii]], col = cols[ii])
  #   lines(x,lower[[ii]], col = cols[ii])
  # }
  
  #lines(x,pred$fit)
  #lines(x,upper)
  #lines(x,lower)
  #x,y's of fit
  #x
  #pred$fit
  #y's of 95% confidence intervals
  #as.numeric(lower)
  #as.numeric(upper)
  
  scatter_out1 = data
  if(categorical.dependent){
    scatter_out1[[paste0(dependendVar,"_level")]] = scatter_out1[[dependendVar]]
    scatter_out1[[dependendVar]] = as.numeric(scatter_out1[[dependendVar]])-1
  }
  
  #change names of logged dependent and independent variables for plot output 
  if(length(dependendVar.name)==1){
    names(scatter_out1)[names(scatter_out1) == dependendVar] = paste0("log(",dependendVar.name,")")
  }
  # logVar.remove = logVar[logVar.stripped == independendVar]
  if(independendVar %in% logVar.stripped){
    x = seq(min(log(data[[independendVar]]), na.rm = T),
            max(log(data[[independendVar]]), na.rm = T),
            length.out = n.pred.pts)
    scatter_out1[[independendVar]] = log(scatter_out1[[independendVar]])
    names(scatter_out1)[names(scatter_out1) == independendVar] = paste0("log(",independendVar,")")
  }
  
  #for rezidualized Y
  scatter_out8 = scatter_out1
  res2    = model2$gam$residuals
  y.fit.plot = mean(data[[dependendVar]] , na.rm = T) + res2
  scatter_out8[[dependendVar]] = as.numeric(y.fit.plot)
  
  # if(length(groupVar) >0)
  
  # lines_sub1 = data.frame(x = x, y =as.numeric(pred$fit))
  # lines_sub2 = data.frame(x = x, y =as.numeric(lower))
  # lines_sub3 = data.frame(x = x, y =as.numeric(upper))
  # lines_out1 = list(lines_sub2,lines_sub1,lines_sub3)
  
  #if(length(levels) > 1){
  lines_out1 = list()
  for(ii in 1:length(levels)){
    lines_sub1 = data.frame(x = x, y = as.numeric(fit[[ii]]))
    lines_sub2 = data.frame(x = x, y =as.numeric(lower[[ii]]))
    lines_sub3 = data.frame(x = x, y =as.numeric(upper[[ii]]))
    lines_out1[[ii]] = list(lines_sub2,lines_sub1,lines_sub3)
  }
  
  if(length(levels) > 1){
    names(lines_out1) = levels
  }
  #plot(data[[independendVar]], data[[dependendVar]])
  #lines(lines_out1[[1]][[1]]$x,lines_out1[[1]][[1]]$y, col = "red")
  #lines(lines_out1[[1]][[2]]$x,lines_out1[[1]][[2]]$y, col = "red")
  #lines(lines_out1[[1]][[3]]$x,lines_out1[[1]][[3]]$y, col = "red")
  #lines(lines_out1[[2]][[1]]$x,lines_out1[[2]][[1]]$y, col = "blue")
  #lines(lines_out1[[2]][[2]]$x,lines_out1[[2]][[2]]$y, col = "blue")
  #lines(lines_out1[[2]][[3]]$x,lines_out1[[2]][[3]]$y, col = "blue")
  
} else{
  
  ###categorical.independent
  coef = summary(model$gam)$p.coeff
  vcov = as.matrix(summary(model$mer)$vcov)
  beta.names = rownames(vcov)
  
  ind.contrasts = which(grepl(independendVar, beta.names))
  vcov = vcov[c(1,ind.contrasts),c(1,ind.contrasts)]
  c = diag(length(ind.contrasts))
  c = cbind(1, c)
  
  new.variances = diag(c%*%vcov%*%t(c))
  new.sds = sqrt(new.variances)
  
  int = as.numeric(coef["(Intercept)"])
  
  mean.per.category = int + coef[ind.contrasts]
  upper.95 = mean.per.category + 1.96*new.sds
  lower.95 = mean.per.category - 1.96*new.sds
  
  point.estimates = as.data.frame(cbind(mean.per.category,upper.95,lower.95))
  point.estimates$eff.names = rownames(point.estimates)
  rownames(point.estimates) = NULL
  
  #add back intercept
  int.stats = c(int, int + 1.96*sqrt(vcov[1,1]), int - 1.96*sqrt(vcov[1,1]), NA)
  point.estimates = rbind(int.stats,point.estimates)
  
  all.levels = paste0(independendVar,unique(data[[independendVar]]))
  ref.level = all.levels[!all.levels %in% point.estimates$eff.names]
  point.estimates$eff.names[1] = ref.level
  # point.estimates$eff.names[1] = paste0(independendVar,"_reference")
  
  #plot
  #ggplot(data = point.estimates, aes(x = eff.names, y = mean.per.category, ymin = lower.95, ymax = upper.95)) +
  #  geom_point(position = position_dodge(width = 0.2)) +
  #  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
  #  coord_flip() +
  #  scale_colour_manual(values = c("blue", "red")) +
  #  theme_bw() + labs(y = "Mean", x = "Category") + 
  #  theme(panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
  #        panel.grid.major.x = element_blank(),
  #        panel.grid.minor.x = element_blank()) 
  
  #means and confidence intervals for each category of a categorical predictor
  #point.estimates
  #scatter_out1 = list()
  scatter_out1 = point.estimates
  scatter_out8 = list()
  
  lines_out1 = list()
}
print("Finish plot 1")
#################
##  2. qqplot  ##
#################
#qq = qq.gam(model$gam, type = "response")

## Quick QQ-plot of deviance residuals
pdf(NULL)
res = as.numeric(residuals(model$gam))
res = scale(res)
qq = qqnorm(res,pch=19,cex=.3)

#qq = qqnorm(residuals(model$gam),pch=19,cex=.3)
#qqline = qqline(residuals(model$gam))
#qqline = qqline(res) #or...
#lines(seq(-4,4,length.out = 100),seq(-4,4,length.out = 100))
#x,y's of points
#qq$x
#qq$y
scatter_out2 = data.frame(qq.x = qq$x,qq.y = as.numeric(qq$y))
lines_out2 = data.frame(x = seq(-4,4,length.out = 100), y = seq(-4,4,length.out = 100))
print("Finish plot 2")
##########################################################################################################################
##  3. histogram of dependent variable, and histogram of independent variable (barplots for categorical.independent x)  ##
##########################################################################################################################
###INDEPENDENT
###continuous & independent
if(categorical.independent == FALSE){
  #if logged...
  if(independendVar %in% logVar.stripped){
    hist.ind = hist(log(data[[independendVar]]),breaks= 20)
  } else{
    hist.ind = hist(data[[independendVar]],breaks= 20)
  }
  
  scatter_out3 = list(breaks = hist.ind$breaks,counts = hist.ind$counts, type = "numerical")
  lines_out3 = data.frame()
  # #dependent
  # hist.dep = hist(data[[dependendVar]],breaks = 20)
  # 
  # scatter_out4 = list(breaks = hist.dep$breaks,counts = hist.dep$counts)
  lines_out4= data.frame()
} else {   
  ###categorical & independent
  counts = table(data[[independendVar]])
  #barplot(counts, col = "white",xlab = independendVar,ylab = "Frequency",main = paste0("Barplot of ",independendVar))
  
  scatter_out3 = list(breaks = names(counts), counts = as.numeric(counts) , type = "categorical")
  lines_out3 = data.frame()
  scatter_out4 = data.frame()
  lines_out4 = data.frame()
}
###DEPENDENT
###continuous & dependent
if(categorical.dependent == FALSE){
  hist.dep = hist(data[[dependendVar]],breaks = 20)
  scatter_out4 = list(breaks = hist.dep$breaks , counts = hist.dep$counts)
} else{ 
  ###categorical & dependent
  counts.dep = table(data[[dependendVar]])
  scatter_out4 = list(breaks = names(counts.dep), counts = as.numeric(counts.dep) , type = "categorical")
  
  #barplot(counts.dep, col = "white",xlab = dependendVar,ylab = "Frequency",main = paste0("Barplot of ",dependendVar))
}

print("Finish plot 3")
######################################################
##  4. residuals against fitted with lowess smooth  ##
######################################################
#residplot = plot(model$mer)
#or
# fitted = fitted(model$mer)
fitted = as.numeric(fitted(model$gam))
res    = model$gam$residuals
#plot(fitted, res)

loess = lowess(fitted, res)
#lines(loess)

#x,y's of points
#as.numeric(fitted)
#as.numeric(res)
#x,y's of loess fit
#loess$x
#loess$y

data$fitted = fitted;
data$res = res;
scatter_out5 = data;

#scatter_out5 = data.frame(fitted, res)
lines_out5 = list(data.frame(x = loess$x, y = loess$y))
print("Finish plot 4")
##########################################
##  5. random effect histograms + sd’s  ##
##########################################
#standard deviations of random effects
ranefs = as.data.frame(summary(model$mer)$varcor)[c(1,5)]

#if(include.random.site & include.random.subject){
#  #BOTH SITE AND SUBJECT
#  sd.site = ranefs[ranefs$grp == "abcd_site",2]
#  sd.family.site = ranefs[ranefs$grp == "rel_family_id",2]
#  sd.subject = ranefs[ranefs$grp == "src_subject_id",2]
#  ranefs = ranefs[ranefs$grp %in% c("abcd_site", "rel_family_id","src_subject_id","Residual"),]
#} else if(include.random.site & !include.random.subject){
#  #SITE, NO SUBJECT
#  sd.site = ranefs[ranefs$grp == "abcd_site",2]
#  sd.family.site = ranefs[ranefs$grp == "rel_family_id:abcd_site",2]
#  sd.subject = NA
#  ranefs = ranefs[ranefs$grp %in% c("abcd_site", "rel_family_id:abcd_site","Residual"),]
#} else if(!include.random.site & include.random.subject){
#  #SUBJECT, NO SITE
#  sd.site = NA
#  sd.family.site = ranefs[ranefs$grp == "rel_family_id",2]
#  sd.subject = ranefs[ranefs$grp == "src_subject_id",2]
#  ranefs = ranefs[ranefs$grp %in% c("rel_family_id","src_subject_id","Residual"),]
#}else{
#  #NO SITE NOR SUBJECT
#  sd.site = NA
#  sd.family.site = ranefs[ranefs$grp == "rel_family_id",2]
#  sd.subject = NA
#  ranefs = ranefs[ranefs$grp %in% c("rel_family_id","Residual"),]
#}

sd.site = NA
sd.family.site = NA
sd.subject = NA
sd.scanner = NA

if((include.random.site | include.random.scanner) & include.random.subject){
  #BOTH (SITE or SCANNER) AND SUBJECT
  if(include.random.site){
    #SITE
    sd.site = ranefs[ranefs$grp == "abcd_site",2]
    ranefs = ranefs[ranefs$grp %in% c("abcd_site", "rel_family_id","src_subject_id","Residual"),]
  } else{
    #SCANNER
    ranefs = ranefs[ranefs$grp %in% c("mri_info_deviceserialnumber", "rel_family_id","src_subject_id","Residual"),]
    sd.scanner = ranefs[ranefs$grp == "abcd_site",2]
  }
  sd.family.site = ranefs[ranefs$grp == "rel_family_id",2]
  sd.subject = ranefs[ranefs$grp == "src_subject_id",2]
  
} else if((include.random.site | include.random.scanner) & !include.random.subject){
  #(SITE or SCANNER), NO SUBJECT
  if(include.random.site){
    #SITE
    sd.site = ranefs[ranefs$grp == "abcd_site",2]
    sd.family.site = ranefs[ranefs$grp == "rel_family_id:abcd_site",2]
    ranefs = ranefs[ranefs$grp %in% c("abcd_site", "rel_family_id:abcd_site","Residual"),]
  } else{
    #SCANNER
    sd.family.site = ranefs[ranefs$grp == "rel_family_id:mri_info_deviceserialnumber",2]
    ranefs = ranefs[ranefs$grp %in% c("mri_info_deviceserialnumber", "rel_family_id:mri_info_deviceserialnumber","Residual"),]
    sd.scanner = ranefs[ranefs$grp == "mri_info_deviceserialnumber",2]
  }
} else if(!(include.random.site | include.random.scanner) & include.random.subject){
  #SUBJECT, NO (SITE or SCANNER)
  sd.family.site = ranefs[ranefs$grp == "rel_family_id",2]
  sd.subject = ranefs[ranefs$grp == "src_subject_id",2]
  ranefs = ranefs[ranefs$grp %in% c("rel_family_id","src_subject_id","Residual"),]
}else{
  #NO (SITE or SCANNER) NOR SUBJECT
  sd.family.site = ranefs[ranefs$grp == "rel_family_id",2]
  ranefs = ranefs[ranefs$grp %in% c("rel_family_id","Residual"),]
}


names(ranefs) = c("Group","SD")
rownames(ranefs) = NULL

#site
ran = ranef(model$mer)


#if(include.random.site & include.random.subject){
#  #BOTH SITE AND SUBJECT
#  ran.site     = hist(ran$abcd_site[,1], breaks = 20,xlim = c(-9,11))
#  ran.fam.site = hist(ran$rel_family_id[,1], breaks = 20)
#  ran.subject  = hist(ran$src_subject_id[,1], breaks = 20)
#} else if(include.random.site & !include.random.subject){
#  #SITE, NO SUBJECT
#  ran.site     = hist(ran$abcd_site[,1], breaks = 20,xlim = c(-9,11))
#  ran.fam.site = hist(ran$`rel_family_id:abcd_site`[,1], breaks = 20)
#  ran.subject  = list(breaks = NA , counts = NA)
#} else if(!include.random.site & include.random.subject){
#  #SUBJECT, NO SITE
#  ran.site     = list(breaks = NA , counts = NA)
#  ran.fam.site = hist(ran$`rel_family_id`[,1], breaks = 20)
#  ran.subject  = hist(ran$src_subject_id[,1], breaks = 20)
#}else{
#  #NO SITE NOR SUBJECT
#  ran.site     = list(breaks = NA , counts = NA)
#  ran.fam.site = hist(ran$`rel_family_id`[,1], breaks = 20)
#  ran.subject  = list(breaks = NA , counts = NA)
#}

ran.site     = list(breaks=NA, counts = NA) 
ran.scanner  = list(breaks=NA, counts = NA) 
ran.subject  = list(breaks = NA , counts = NA)

if((include.random.site | include.random.scanner) & include.random.subject){
  #BOTH (SITE or SCANNER) AND SUBJECT
  if(include.random.site){
    #SITE
    ran.site     = hist(ran$abcd_site[,1], breaks = 20,xlim = c(-9,11))
  } else{
    #SCANNER
    ran.scanner  = hist(ran$mri_info_deviceserialnumber[,1], breaks = 20,xlim = c(-9,11))
  }
  ran.fam.site = hist(ran$rel_family_id[,1], breaks = 20, xlim = c(-9,11))
  ran.subject  = hist(ran$src_subject_id[,1], breaks = 20)
} else if((include.random.site | include.random.scanner) & !include.random.subject){
  #(SITE or SCANNER), NO SUBJECT
  if(include.random.site){
    #SITE
    ran.site     = hist(ran$abcd_site[,1], breaks = 20 , xlim = c(-9,11))
    ran.fam.site = hist(ran$`rel_family_id:abcd_site`[,1], breaks = 20)
  } else{
    #SCANNER
    ran.scanner  = hist(ran$mri_info_deviceserialnumber[,1], breaks = 20,xlim = c(-9,11))
    ran.fam.site = hist(ran$`rel_family_id:mri_info_deviceserialnumber`[,1], breaks = 20)
  }
} else if(!(include.random.site | include.random.scanner) & include.random.subject){
  #SUBJECT, NO (SITE nor SCANNER)
  ran.fam.site = hist(ran$`rel_family_id`[,1], breaks = 20)
  ran.subject  = hist(ran$src_subject_id[,1], breaks = 20)
}else{
  #NO (SITE nor SCANNER) NOR SUBJECT
  ran.fam.site = hist(ran$`rel_family_id`[,1], breaks = 20)
}

ran.fam.site$breaks
ran.fam.site$counts

#xmin = min(c(range(ran.site$breaks)[1],range(ran.fam.site$breaks)[1],range(ran.subject$breaks)[1]),na.rm=TRUE)
#xmax = max(c(range(ran.site$breaks)[2],range(ran.fam.site$breaks)[2],range(ran.subject$breaks)[2]),na.rm=TRUE)

xmin = min(range(ran.site$breaks)[1],range(ran.fam.site$breaks)[1],range(ran.subject$breaks)[1], range(ran.scanner$breaks)[1], na.rm=T)
xmax = max(range(ran.site$breaks)[2],range(ran.fam.site$breaks)[2],range(ran.subject$breaks)[2], range(ran.scanner$breaks)[2], na.rm=T)


scatter_out6 = list(breaks = ran.site$breaks,counts = ran.site$counts, xmin = xmin, xmax = xmax, sd = sd.site)
lines_out6= data.frame()

scatter_out7 = list(breaks = ran.fam.site$breaks ,counts = ran.fam.site$counts, xmin = xmin, xmax = xmax, sd = sd.family.site)
lines_out7= data.frame()

scatter_out9 = list(breaks = ran.subject$breaks ,counts = ran.subject$counts, xmin = xmin, xmax = xmax, sd = sd.subject)
lines_out9= data.frame()

scatter_out10 = list(breaks = ran.scanner$breaks ,counts = ran.scanner$counts, sd = sd.scanner)
lines_out10= data.frame()


print("Finish plot 5")
###################
##  Data Export  ##
###################
#include scatter_out8 - residualized y-axis
#scatter = list(scatter_out1,scatter_out2,scatter_out3,scatter_out4,scatter_out5,scatter_out6,scatter_out7,scatter_out9)
scatter = list(scatter_out1,scatter_out2,scatter_out3,scatter_out4,scatter_out5,scatter_out6,scatter_out7,scatter_out9,scatter_out10)
if(TEST){
  #scatter = list(scatter_out1,scatter_out2,scatter_out3,scatter_out4,scatter_out5,scatter_out6,scatter_out7,scatter_out8)
  #scatter = list(scatter_out1,scatter_out2,scatter_out3,scatter_out4,scatter_out5,scatter_out6,scatter_out7,scatter_out9,scatter_out8)
  scatter = list(scatter_out1,scatter_out2,scatter_out3,scatter_out4,scatter_out5,scatter_out6,scatter_out7,scatter_out9,scatter_out10,scatter_out8)
  
  
}
line
lineplot = list(lines_out1,lines_out2,lines_out3,lines_out4,lines_out5,lines_out6,lines_out7,lines_out9,scatter_out10)
#lineplot = list(lines_out1,lines_out2,lines_out3,lines_out4,lines_out5,lines_out6,lines_out7,lines_out9)
#lineplot = list(lines_out1,lines_out2,lines_out3,lines_out4,lines_out5,lines_out6,lines_out7)

statistics = list()

#add warning
#if(trigger.warning){
#  statistics[["warning"]] = warning.1.unique.value
#}
if(trigger.warning){
  warning.disp = c()
  if(exists("warning.logging.0")) warning.disp = c(warning.disp, warning.logging.0)
  if(exists("warning.1.unique.value")) warning.disp = c(warning.disp, warning.1.unique.value)
  if(exists("warning.duplicates")) warning.disp = c(warning.disp, warning.duplicates)
  if(exists("warning.1.unique.eventname")) warning.disp = c(warning.disp, warning.1.unique.eventname)
  if(exists("warning.2.unique.eventname")) warning.disp = c(warning.disp, warning.2.unique.eventname)
  warning.disp = c("WARNING: ",warning.disp)
  
  
  statistics[["warning"]] = warning.disp
}

ptab = as.data.frame(summary(model$gam)$p.table)
ptab$Estimate = round(ptab$Estimate, 5)
ptab$`Std. Error` = round(ptab$`Std. Error`, 5)
#ptab$`t value` = round(ptab$`t value`, 2)
if(categorical.dependent){
  ptab$`z value` = round(ptab$`z value`, 2)
}else{
  ptab$`t value` = round(ptab$`t value`, 2)
}

ptab$sig = ""
if(categorical.dependent){
  ptab$sig[ptab$`Pr(>|z|)` < 0.1] = "."
  ptab$sig[ptab$`Pr(>|z|)` < 0.05] = "*"
  ptab$sig[ptab$`Pr(>|z|)` < 0.01] = "**"
  ptab$sig[ptab$`Pr(>|z|)` < 0.001] = "***"
  
  ptab$`Pr(>|z|)` = round(ptab$`Pr(>|z|)`,7)
  ptab$`Pr(>|z|)`[ptab$`Pr(>|z|)` < 0.000001] = "<1e-6"
}else{
  ptab$sig[ptab$`Pr(>|t|)` < 0.1] = "."
  ptab$sig[ptab$`Pr(>|t|)` < 0.05] = "*"
  ptab$sig[ptab$`Pr(>|t|)` < 0.01] = "**"
  ptab$sig[ptab$`Pr(>|t|)` < 0.001] = "***"
  
  ptab$`Pr(>|t|)` = round(ptab$`Pr(>|t|)`,7)
  ptab$`Pr(>|t|)`[ptab$`Pr(>|t|)` < 0.000001] = "<1e-6"
}

#if *'s in usercovVar, split the interaction terms (all to be included in final ptable)
if(any(grepl("[*]",usercovVar))){
  vars.to.split = usercovVar[grepl("[*]",usercovVar)]
  #only keep ptab variables that are in independent variables & other independent variables
  interactionVars.all = unlist(strsplit(vars.to.split,"*",fixed=T))
  #vars.to.include.in.ptab = paste(c(independendVar,usercovVar,interactionVars.all,sqVar,groupVar,logVar),collapse="|")
  vars.to.include.in.ptab = paste(c(independendVar,usercovVar,interactionVars.all,sqVar,sqVar_SQUARED,groupVar,logVar),collapse="|")
  
  #vars.to.include.in.ptab = c(independendVar,usercovVar,interactionVars.all,sqVar,groupVar,logVar)
} else{
  #only keep ptab variables that are in independent variables & other independent variables
  #vars.to.include.in.ptab = paste(c(independendVar,usercovVar,sqVar,groupVar,logVar),collapse="|")
  vars.to.include.in.ptab = paste(c(independendVar,usercovVar,sqVar,sqVar_SQUARED,groupVar,logVar),collapse="|")
  
  #vars.to.include.in.ptab = c(independendVar,usercovVar,sqVar,groupVar,logVar)
}
#ptab = ptab[grepl(vars.to.include.in.ptab, rownames(ptab)),]
ptab = ptab[grepl(vars.to.include.in.ptab, rownames(ptab)),]
#ptab = ptab[rownames(ptab) %in% vars.to.include.in.ptab,]


#ptab$sig[ptab$`Pr(>|t|)` < 0.1] = "."
#ptab$sig[ptab$`Pr(>|t|)` < 0.05] = "*"
#ptab$sig[ptab$`Pr(>|t|)` < 0.01] = "**"
#ptab$sig[ptab$`Pr(>|t|)` < 0.001] = "***"
#
#ptab$`Pr(>|t|)` = round(ptab$`Pr(>|t|)`,7)
#ptab$`Pr(>|t|)`[ptab$`Pr(>|t|)` < 0.000001] = "<1e-6"
ptab = as.matrix(ptab)

if(nrow(ptab) == 0){
  statistics[["ptable"]] = ""
} else{
  statistics[["ptable"]] = stargazer(ptab , type = "html")
}


#statistics[["ptable"]] = stargazer(summary(model$gam)$p.table,type = "html")
statistics[["formula"]] = format(summary(model$gam)$formula);
statistics[["formula.random"]] = formula.random

eftab = matrix(c(n,r2,paste0(r2_delta," (",round(100.0*r2_delta,digits=2),"%)")),ncol=1,byrow=TRUE);
colnames(eftab) = c("Effect Size");
rownames(eftab) = c("N","R-squared","ΔR-squared")
statistics[["eftab"]] = stargazer(eftab,type="html",summary=F,rownames=T)


ranefs$Group[ranefs$Group == "rel_family_id:abcd_site"] = "rel_family_id_abcd_site"
ranefs$Group[ranefs$Group == "rel_family_id:mri_info_deviceserialnumber"] = "rel_family_id_mri_info_deviceserialnumber"

statistics[["rantable"]] = stargazer(ranefs,type="html",summary=F,rownames=F)
statistics[["other.stats"]] = list()
statistics[["other.stats"]][["r2"]]  =     paste0("R-squared: ", r2);
statistics[["other.stats"]][["r2_delta"]]= paste0("ΔR-squared (from independent and grouping variables): ",r2_delta," (",round(100.0*r2_delta,digits=2),"%)");
statistics[["other.stats"]][["n"]]   =     paste0("N: ",n);
aic_difference = round(aic-aic2,digits=0)
if (aic_difference > 0)
  aic_difference = paste0("+", aic_difference)
bic_difference = round(bic-bic2,digits=0)
if (bic_difference > 0)
  bic_difference = paste0("+", bic_difference)
statistics[["other.stats"]][["aic2"]] =    paste0("AIC (base model): ",aic2);
statistics[["other.stats"]][["bic2"]] =    paste0("BIC (base model): ",bic2);
statistics[["other.stats"]][["aic"]] =     paste0("AIC (full model): ",aic," (",aic_difference,")");
statistics[["other.stats"]][["bic"]] =     paste0("BIC (full model): ",bic," (",bic_difference,")");

statistics[["formula"]] = format(summary(model$gam)$formula)

#statistics = list(ptable = stargazer(summary(model$gam)$p.table,type = "html"), rantable = stargazer(ranefs,type="html"));

#statistics = list(stargazer(summary(model$gam)$p.table,type = "html"),
#                  stargazer(anova(model$gam)$pTerms.table,type = "html"));

#if(length(s.table) > 0) statistics = list(statistics, stable = stargazer(s.table,type = "html"));
s.table = as.data.frame(summary(model$gam)$s.table)

if(length(s.table) > 0){
  s.table$edf = round(s.table$edf,2)
  s.table$Ref.df = round(s.table$Ref.df,2)
  #s.table$F = round(s.table$F,2)
  if(categorical.dependent){
    s.table$Chi.sq = round(s.table$Chi.sq,2)
  }else{
    s.table$F = round(s.table$F,2)
  }
  
  s.table$sig = ""
  s.table$sig[s.table$`p-value` < 0.1] = "."
  s.table$sig[s.table$`p-value` < 0.05] = "*"
  s.table$sig[s.table$`p-value` < 0.01] = "**"
  s.table$sig[s.table$`p-value` < 0.001] = "***"
  
  s.table$`p-value` = round(s.table$`p-value`,7)
  s.table$`p-value`[s.table$`p-value` < 0.000001] = "<1e-6"
  s.table = as.matrix(s.table)
  
  statistics[["stable"]] = stargazer(s.table,type = "html");
}


###ERROR HERE stargazer(anova(model$gam)$pTerms.table,type = "html")
### if only a smooth term and no other terms, then you an error will show
anova.tab = as.data.frame(anova(model$gam)$pTerms.table)
if(length(anova.tab)>0){
  #statistics[["anova"]] = list(stargazer(anova(model$gam)$pTerms.table,type = "html"))
  #anova.tab$F = round(anova.tab$F,2)
  if(categorical.dependent){
    anova.tab$Chi.sq = round(anova.tab$Chi.sq,2)
  }else{
    anova.tab$F = round(anova.tab$F,2)
  }
  
  anova.tab$sig = ""
  anova.tab$sig[anova.tab$`p-value` < 0.1] = "."
  anova.tab$sig[anova.tab$`p-value` < 0.05] = "*"
  anova.tab$sig[anova.tab$`p-value` < 0.01] = "**"
  anova.tab$sig[anova.tab$`p-value` < 0.001] = "***"
  
  anova.tab$`p-value` = round(anova.tab$`p-value`,7)
  anova.tab$`p-value`[anova.tab$`p-value` < 0.000001] = "<1e-6"
  
  ##  stargazer fails when variable contains 3 underscores, "___", so,
  ##  need to replace underscores w/placeholder (_3UNDERSCORES_), and then replace placeholder w/the underscores back in the HTML
  underscores.to.replace = sum(grepl("___",rownames(anova.tab)))>=1
  if(underscores.to.replace){
    # add.placeholder2 = rownames(anova.tab)[grepl("__",rownames(anova.tab))]
    add.placeholder3 = rownames(anova.tab)[grepl("___",rownames(anova.tab))]
    placeholders = unlist(lapply(strsplit(add.placeholder3,"___"), function(x)paste0(x[1], "_3UNDERSCORES_", x[2])))
    rownames(anova.tab)[grepl("___",rownames(anova.tab))] = placeholders
  }
  
  anova.tab = as.matrix(anova.tab)
  statistics[["anova"]] = list(stargazer(anova.tab , type = "html"))
  
  if(underscores.to.replace){
    for(ii in 1:length(statistics[["anova"]][[1]])){
      if(grepl("_3UNDERSCORES_",statistics[["anova"]][[1]][ii])){
        statistics[["anova"]][[1]][ii] = gsub("_3UNDERSCORES_" ,"___" , statistics[["anova"]][[1]][ii])
      }
    }
  }
  
} else{
  statistics[["anova"]] =  list()
}


# Add a table1 data summary
# WORK IN PROGRESS
# Issue: this table does not show the categorical variables (see https://stackoverflow.com/questions/25389683/obtaining-separate-summary-statistics-by-categorical-variable-with-stargazer-pac)
#statistics[["table1"]] = list(stargazer(data, type="html", summary.stat = c("n", "mean", "sd")))
#data$src_subject_id = NULL;
#statistics[["table1"]] = list(stargazer(print(CreateTableOne(data = data,), showAllLevels = TRUE),type = "html"));

table1.1 = CreateTableOne(vars = varList, data = data)
statistics[["table1"]] = list(stargazer(print(table1.1, showAllLevels = TRUE),type = "html"))

if(length(groupVar)>0){
  table1.2 = CreateTableOne(vars = varList, data = data, strata = groupVar)
  table1.2 = print(table1.2, showAllLevels = TRUE)
  table1.2 = table1.2[,1:(ncol(table1.2)-1)]
  statistics[["table1"]] = list(stargazer(table1.2 , type = "html"))
}

tunnel = data;
#table1 = list(stargazer(data, type="html", summary.stat = c("n", "mean", "sd")))
#append(table1[[1]],"42",after=3)
#statistics[["table1"]] = table1