# functions

main = function(){
  source("main.R")
}

# Set graphics device to windows
dev.win = function(){
  if("windows" %in% names(dev.list())){
    dev.set(dev.list()["windows"])
  }
  if(!"windows" %in% names(dev.list())){
    windows()
  }
}

# Unique values of each column in a data fram
unique.data = function(df){
  return(lapply(df,unique))
}

# plot.matrix
plot.matrix = function(x, cutoff=NULL, ...){
  if(class(x) == "factanal"){
    x = unclass(x$loadings)
    # if(!is.null(cutoff)){
    #   x[x<cutoff] = 0
    #   x[x != 0] = 1
    # }
    image(t(apply(x,2,rev)),axes=F, ...)
    lnx = par("usr")[2]-par("usr")[1]
    lny = par("usr")[4]-par("usr")[3]
    # abline(v = seq(par("usr")[1],par("usr")[2],by=lnx/ncol(x)),col=2)
    
    axis(1,at = lnx/ncol(x)*(0:(ncol(x)-1)),labels=paste("factor",1:ncol(x)),xpd=NA) # 1/2*ln/ncol(x)+
    axis(2,at = lny/nrow(x)*(0:(nrow(x)-1)),labels=rev(rownames(x)),las=1,xpd=NA)
    segments(seq(par("usr")[1],par("usr")[2],by=lnx/ncol(x)),rep(par("usr")[3],ncol(x)),
             seq(par("usr")[1],par("usr")[2],by=lnx/ncol(x)),rep(par("usr")[4],ncol(x)),col=2)
    title(paste0("Cutoff ",cutoff))
    return(invisible(NULL))
  }
  if(!is.null(cutoff)){
    x[x<cutoff] = 0
    col = x
    col[col != 0] = 1
  }
  image(t(apply(x,2,rev)), ...)
}

# proper positions for text on plots
my.par = function(side,scale){
  environment(globalenv())
  side.opt = c("below","left","above","right")
  side.print = paste(side.opt,1:4,collapse=", ")
  if(!side %in% side.opt & !side %in% 1:4){
    print(paste("side must be on of: ",side.print,collapse=" "))
  }
  p = par("usr")
  if(side == "below" | side == 1){
    x = mean(p[1:2])
    y = p[3] - dist(p[3:4])*scale
  }
  if(side == "above" | side == 3){
    x = mean(p[1:2])
    y = p[4] + dist(p[3:4])*scale
  }
  #
  if(side == "left" | side == 2){
    y = mean(p[3:4])
    x = p[1] - dist(p[1:2])*scale
  }
  
  if(side == "right" | side == 4){
    y = mean(p[3:4])
    x = p[2] + dist(p[1:2])*scale
  }
  return(list(x=x,y=y))
}

# plot NA
plot.na = function(df,col=NULL,...){
  df = is.na(df)
  if(is.null(col)){col=grey.colors(2)}
  image(t(apply(df,2,rev)),col=grey.colors(2),yaxt="n",xaxt="n",xlab="",ylab="",...)
  # title("Data image")
  p = my.par(3,.2)
  legend(p$x,p$y,
         xjust=.5,yjust=.5,c("Missing data"),xpd=NA,bty="n",fill=grey.colors(2)[2],cex=1.25)
}

# for(i in 1:length(full_questions_names)){
#   print(unique(full_questions_names)[i])
#   writeClipboard(unique(full_questions_names)[i])
#   readline("continue?")
# }

my.count = function(x){
  y = unique(x)
  out = list()
  for(i in 1:length(y)){
    out[i] = sum(x %in% y[i])
  }
  return(unlist(out))
}

# proper barplot
my.barplot.old = function(df, ...){
  df = df[,!sapply(df,is.numeric)]
  if(1 %in% dim(df) | 0 %in% dim(df)){
    return()
  }
  # layout(matrix(1:2,nrow=2,byrow=F),height=c(8,1))
  x = lapply(df,table)
  ps = barplot(as.matrix(x[[1]]),width=1/length(x),xlim=c(0,1),col=grey.colors(max(unlist(lapply(x,length)))),...)
  for(i in 2:length(x)){
    par(new=T)
    ps2 = barplot(as.matrix(x[[i]]),bty="n",yaxt="n",xaxt="n",xlim=c(0,1),space=(i-1)+.1,width=1/length(x),
                  col=grey.colors(max(unlist(lapply(x,length)))))
    ps = c(ps,ps2)
  }
  title()
  text(x=ps,y=par("usr")[3]-dist(par("usr")[3:4])/10,labels=names(df),xpd=NA,srt=45,col=2,cex=.75)
  legend(par("usr")[2]+dist(par("usr")[1:2])/50,mean(par("usr")[3:4]),
         names(x[[which.max(unlist(lapply(x,length)))]]),
         as.character(levels(df[,1])),bty="n",xpd=NA,
         fill = grey.colors(max(unlist(lapply(x,length)))),yjust=.5)
}

# rename levels to max level
rename.level = function(df,include.na = NULL, ordered){
  if(ncol(df)<2 & !is.factor(df[,1])){
    return(df)
  }
  max.level = which.max(unlist(lapply(df,function(x) length(levels(x)))))
  new.level = levels(df[,max.level])
  if(!is.null(include.na)){
    df = lapply(df,function(x,y) factor(as.character(x),new.level,ordered = ordered),y=new.level)
    df = do.call(cbind.data.frame,lapply(df,addNA))
  }
  if(is.null(include.na)){
    df = lapply(df,function(x,y) factor(as.character(x),new.level,ordered = ordered),y=new.level)
    df = do.call(cbind.data.frame,df)
  }
  
  return(df) 
}


# goood stuff
.BucketEnv = new.env()
if("bucket.RData" %in% dir()){
  load("bucket.RData",.BucketEnv)
}
bucket = function(...,add = F,env = .BucketEnv,short=T,rmv=F,filef = "bucket.RData",empty.bucket=F){
  #
  if(empty.bucket){
    ans = readline("Empty the bucket? yes/no \n")
    if(ans == "yes"){
      env = new.env()
      return(invisible(NULL))
    }
    if(ans != "yes"){
      return(invisible(NULL))
    }
  }
  # Exit if add=F and empty bucket
  if(!add & length(as.list(env))==0 & !rmv){
    cat("Empty bucket, nothing to find here.\n")
    return(invisible(NULL))
  }
  # display items if add=F and !empty bucket
  if(!add & length(as.list(env))!=0 & length(as.character(as.list(substitute(list(...)))[-1]))==0 & !rmv){
    cat("In storage: \n")
    if(short){
      zz = ls(envir = env)
      zz2 = lapply(as.list(env),class)
      names(zz2) = NULL
      print(data.frame(class=unlist(zz2),name = zz))
      return(invisible(data.frame(class=unlist(zz2),name = zz)))
    }
    if(!short){
      print(as.list(env))
    }
    return(invisible(NULL))
  }
  arg = as.character(as.list(substitute(list(...)))[-1])
  
  # Check for existence
  out = arg %in% ls(env)
  # -----------------------------------------
  # Return from bucket
  if(!add & !rmv){
    if(!all(out)){
      cat(paste0(paste0(arg[!out],collapse=", ")," not in the bucket. \n"))
      return(invisible(NULL))
    }
    if(all(out)){
      cat(paste0("Returning ",paste0(arg[out],collapse = ", "),".\n"))
      # new ---
      for(i in 1:length(out)){
        assign(arg[out[i]],as.list(env)[arg[out[i]]],envir = .GlobalEnv)
      }
      # end new ---
      return(as.list(env)[arg[out]])
    }
    
  }
  # -----------------------------------------
  # Add to bucket
  if(add & !rmv){
    # Overwriting
    if(any(out)){
      for(i in arg[out]){
        ans = readline(paste0(i," already exist in bucket. Overwrrite? y/n: \n"))
        if(ans == "y"){
          assign(i,eval(parse(text = i)),envir = env)
          cat(paste0(i," added to the bucket!\n"))
        }
        if(ans == "n"){}
        if(!ans %in% c("y","n")){
          warning("Exitted function - answer me! ")
        }
      }
    }
    # Not overwriting, just adding
    for(i in arg[!out]){
      if(exists(i,envir = .GlobalEnv)){
        assign(i,eval(parse(text = i)),envir = env)
        cat(paste0(i," added to the bucket!\n"))
      }
      if(!exists(i,envir = .GlobalEnv)){
        cat(paste0("Object ",i," does not exist!\n"))
      }
    }
  }
  # Remove items from bucket
  if(rmv){
    for(i in arg){
      if(!all(out)){
        cat(paste0(paste0(arg[!out],collapse=", ")," not in the bucket. \n"))
      }
      if(all(out)){
        ans = readline(paste0("Are you sure you want to remove ",i," from the bucket. y/n: "))
        if(ans == "y"){
          rm(list=i,envir = env)
        }
      }
    }
  }
  # save(list=ls(envir = .BucketEnv),file=paste0(getwd(),"/",filef),envir = env)
  with(env,save.image(paste0(getwd(),"/",filOef)))
}

# Add classes to bucket
bucket.classes = function(destination = .BucketEnv,classes,from=.GlobalEnv,...){
  varz = ls(name=from)
  # varz = varz[216:220]
  for(i in 1:length(varz)){
    tmp = eval(parse(text=varz[i]))
    if(class(tmp) == "function"){
      next
    }
    # got to search for classes in lists...
    tmp = break.list(tmp)
    if(class(tmp) != "list"){
      tmp = list(tmp)
    }
    tmp.class = unlist(lapply(tmp,class))
    for(j in 1:length(tmp)){
      if(length(tmp) > 0 && !is.null(tmp) && tmp.class[[j]] %in% classes){
        obj = tmp[[j]]
        # ifelse(is.null(names(tmp)[j]),nameend <- "", nameend <- names(tmp)[j])
        # assign(paste0(varz[i],"_",nameend),obj,envir = destination)
        if(is.null(names(tmp)[j])){
          assign(paste0(varz[i]),obj,envir = destination)
        }
        if(!is.null(names(tmp)[j])){
          assign(paste0(varz[i],"_",names(tmp)[j]),obj,envir = destination)
        }
      }
    }
  }
}

# break list into list of non list objects 
break.list = function(x){
  out = list()
  if(class(x) == "list" & length(x) > 0){
    for(i in 1:length(x)){
      if(length(x[[i]]) > 0){
        out[[i]] = break.list(x[[i]])
        if(is.null(names(x))){
          names(out)[i] = paste0(names(x)[i],"NEW_",i)
        }else{
          names(out)[i] = names(x)[i]
        }
        
      }
    }
    return(out)
  }
  if(!class(x) == "list"){
    out = x
    return(out)
  }
}

bucket.classes(classes=c("clm","lm","factanal","cfa","data.frame"))
with(.BucketEnv,save.image(paste0(getwd(),"/bucket.RData")))
# Remove env
# with(.BucketEnv,rm(list=ls()))


# save variables to data_file.RData
record = function(file = data_file){
  save.image(file)
}

# data frame factor to numerics
fact2num = function(y,all=NULL){
  if(is.null(all)){
    print("opt1")
    for(i in 1:ncol(y)){
      y[,i] = as.numeric(as.character(y[,i]))
    }
    return(y)
  }else{
    print("opt2")
   for(i in all){
     y[,i] = as.numeric(as.character(y[,i]))
   }
    return(y)
  }
}

# Brute force PCA, returning the PCs
brute.force.pca = function(x){
  y = prcomp(x)
  return(y$x[, summary(y)$importance[1,] > 1])
}

# Brute force FA with with pca and pval evaluation
brute.force.fa = function(x,...){
  nfac = sum(prcomp(x)$sdev > 1)
  nfac = min(floor(ncol(x)/2) - 1,nfac)
  fa.list = list()
  for(i in 1:nfac){
    fa.list[[i]]  = factanal(x,factors = i,...)
    names(fa.list)[i] = paste0(i,"factors")
    if(is.null(fa.list[[i]]$PVAL) || fa.list[[i]]$PVAL < .05){
      fa.list[[i]] = NULL
    }
  }
  return(fa.list[!sapply(fa.list,is.null)])
}

# plot factor loadings
plot.loadings = function(x,...){
  y = unclass(x$loadings)
  apply(y,2, my.barplot, namez = rownames(y), ...)
  return(invisible(NULL))
}

# barplot with tilted labels
my.barplot = function(x, namez, ...){
  b = barplot(x, names.arg=NA,...)
  text(y=par("usr")[3],x = b, labels=namez,srt=45,xpd=NA,...)
}

# predicted classifications from CLM
class.pred = function(model){
  tbl = table(model$y,predict(model,type="class")$fit,useNA = "no")
  perc = round(diag(tbl)/rowSums(tbl),2)
  pergroup = rowSums(tbl)
  total = sum(pergroup)
  perc2 = round(perc*pergroup/total,2)
  return(list(table = tbl, percentages = perc,
              pop = pergroup, total = sum(perc2)))
}

# Predicting a questions. df of the form cbind(Q1,Q2)
model.list = function(pred,resp, ...){
  # x predictors
  # y responses
  clms = list()
  pred = as.data.frame(pred)
  resp = as.data.frame(resp)
  
  for(i in 1:ncol(resp)){
    df = cbind(pred,Y=resp[,i])
    frmla = as.formula(paste0("Y"," ~ ",paste0(names(pred),collapse=" + ")))
    # perform LM if numeric
    if(is.numeric(df$Y)){
      clms[[i]] = step(lm(formula=frmla,data=df,...),test="F",trace = 0)
    }
    # perform CLM if ordinal
    if(!is.numeric(df$Y)){
      clms[[i]] = step(clm(formula=frmla,data=df,...),test="Chisq",trace = 0)
    }
    cat(paste0("Estimating model ",i," of ", ncol(resp),"\n"))
  }
  names(clms) = names(resp) 
  return(clms)
}

# model.list + warnings parameter
model.listZ = function(pred,resp,exclude.warnings=T, ...){
  # x predictors
  # y responses
  clms = list()
  pred = as.data.frame(pred)
  resp = as.data.frame(resp)
  
  for(i in 1:ncol(resp)){
    cat(paste0("Estimating model ",i," of ", ncol(resp),"\n"))
    df = cbind(pred,Y=resp[,i])
    frmla = as.formula(paste0("Y"," ~ ",paste0(names(pred),collapse=" + ")))
    # perform LM if numeric
    if(is.numeric(df$Y)){
      clms[[i]] = step(lm(formula=frmla,data=df,...),test="F",trace = 0)
    }
    # perform CLM if ordinal
    if(!is.numeric(df$Y)){
      if(exclude.warnings){
        clms[[i]] <- step(clm(formula=frmla,data=df,...),test="Chisq",trace = 0)
        if(clms[[i]]$convergence$code != 0){
          cat(paste0("CLM convergence code for i = ",i," is ", clms[[i]]$convergence$code,"...\n"))
          clms[i] = "bad"
        }
      }
      if(!exclude.warnings){
        clms[[i]] <- step(clm(formula=frmla,data=df,...),test="Chisq",trace = 0)
      }
    }
  }
  
  names(clms) = names(resp)
  clms = clms[clms!="bad"]
  return(clms)
}

# model.list + warnings parameter + second order interactions
model.listZ2 = function(pred,resp,exclude.warnings=T,order=2,trace=0, ...){
  # x predictors
  # y responses
  clms = list()
  pred = as.data.frame(pred)
  resp = as.data.frame(resp)
  
  for(i in 1:ncol(resp)){
    cat(paste0("Estimating model ",i," of ", ncol(resp),"\n"))
    df = cbind(pred,Y=resp[,i])
    frmla =  as.formula(paste0("Y"," ~ ","( ",paste0(names(pred),collapse=" + ")," )^",order))
    # perform LM if numeric
    if(is.numeric(df$Y)){
      clms[[i]] = step(lm(formula=frmla,data=df,...),test="F",trace = trace)
    }
    # perform CLM if ordinal
    if(!is.numeric(df$Y)){
      if(exclude.warnings){
        clms[[i]] <- step(clm(formula=frmla,data=df,...),test="Chisq",trace = trace)
        if(clms[[i]]$convergence$code != 0){
          cat(paste0("CLM convergence code for i = ",i," is ", clms[[i]]$convergence$code,"...\n"))
          clms[i] = "bad"
        }
      }
      if(!exclude.warnings){
        clms[[i]] <- step(clm(formula=frmla,data=df,...),test="Chisq",trace = trace)
      }
    }
  }
  
  names(clms) = names(resp)
  clms = clms[clms!="bad"]
  return(clms)
}

# Predict pred with resp, w/o overlapping. trims NAs. Wrapper of model.list
model.list.adv = function(pred,resp,...){
  newdf = na.omit(cbind(pred,resp))
  print(paste0("Dim new dat = ",paste0(dim(newdf),collapse=" ")))
  fk = model.listZ(pred = newdf[,!grepl(deparse(substitute(resp)),names(newdf))],
                   resp = newdf[,grepl(deparse(substitute(resp)),names(newdf))],...)
  return(model.list = fk)
}

# Predicting each var in a df
clm.each = function(x,...){
  clms = list()
  j = 0
  for(i in 1:ncol(x)){
    if(!"factor" %in% class(x[,i])){
      next 
    }
    j = j + 1
    cat(paste0("Estimating model ",i," of ", ncol(x),"\n"))
    frmla = as.formula(paste0(names(x)[i],"~."))
    clms[[j]] = step(clm(frmla , data =x,...),trace=0)
    names(clms)[[j]] = names(x)[i]
  }
  # names(clms) = names(x)
  return(clms)
}

# Evaluate model
eval.model = function(x,which = c(1:3, 5),...){
  if(class(x) == "lm"){
    plot(x,which=which,...)
    acf(resid(x),...)
    cat(paste0(lillie.test(resid(x))$p.value,"\n"))
    return(invisible(NULL))
  }
  if(class(x) == "clm"){
    return(class.pred(x))
  }
}

# ordered factor to un-ordered factor. non factors remain the same
ordfactor = function(x, ordered){
  if(is.factor(x)){
    return(factor(x, ordered = ordered))
  }else{
    return(x)
  }
}

# as.unordered applied to data frames
ordfactordf = function(x,ordered){
  return(do.call(cbind.data.frame,lapply(x,ordfactor,ordered = ordered)))
}

# pred.clm, CLM or list of CLMs
plot.clm = function(x,type="l",lwd=3,...){
  if(class(x)!="list")x = list(x)
  if("lm" %in% unlist(lapply(x,class))) {
    return(stop("Please supply CLM objects"))
  }
  plot(rep(1,length(x[[1]]$y)),pch=15,col="grey",bty="n",ylab="",yaxt="n",ylim=c(.8,.8+length(x)*.2),...)
  for(i in 1:length(x)){
    points(rep(1,length(x[[1]]$y)) +.2*i,col="grey",pch=15,...)
    z = x[[i]]
    zz = ordfactor(z$y,ordered = F)
    out = zz == predict(x[[i]],type="class")$fit
    out[!out] = NA
    points(out+.2*i - .2,lwd=lwd,pch=15,...)
    legend("bottom",c("Correct prediction","Wrong prediction"),fill=c(1,"grey"),bty="n",horiz=T,xpd=NA,...)
  }
  text(x = mean(par("usr")[1:2]),y = seq(.9,.9+length(x)*.2-.1,.2), labels=names(x),xpd=NA)
}

# plot the actual matrix
plot.matrix2 = function(x,rnd=3,srt=45,lty=2,cutoff=NULL,cex=1,scale=F,scale.lab=.03,...){
  plot.matrix(round(x),bty="n",col="white",yaxt="n",xaxt="n")
  xseq = seq(0,1,len=ncol(x))
  xseq = rep(xseq,each=nrow(x))
  yseq = seq(0,1,len=nrow(x))
  yseq = rep(yseq,ncol(x))
  abline(v = (unique(xseq) - 1/(ncol(x)*2))[-1],col="grey",lty=lty)
  abline(h = (unique(yseq) - 1/(nrow(x)*2))[-1],col="grey",lty=lty)
  if(!is.null(cutoff)){
    x[x<cutoff] = 0
  }
  if(scale){
    cex = cex * x/max(x)
  }
  lx = par("usr")[2] - par("usr")[1]
  ly = par("usr")[4] - par("usr")[3]
  posx = par("usr")[1] - lx*scale.lab
  posy = par("usr")[3] - ly*scale.lab
  text(xseq,rev(yseq),labels = round(x,rnd),xpd=NA,cex=cex)
  text(unique(xseq),rep(posy,ncol(x)),labels = colnames(x),xpd=NA,srt=srt,...)
  text(rep(posx,nrow(x)),unique(yseq),labels = rownames(x),xpd=NA,srt=srt,...)
}

# invert levels of  factors and factors in data.frames
invert.level = function(x,vars = NULL){
  # vars character string of vars to reverse
  if(class(x)!= "data.frame"){
    x = as.data.frame(x=x)
  }
  if(is.null(vars)){
    x = do.call(cbind.data.frame,lapply(x,function(x) factor(x,labels=rev(levels(x)[levels(x)%in%unique(x)]))))
  }
  for(i in vars){
    x[,i] = factor(x[,i],labels=rev(levels(x[,i])[levels(x[,i])%in%unique(x[,i])]))
  }
  return(x)
}

# Chronebach's alpha recursive
alpha.recursive = function(dta){
  x = alpha(dta)
  whch = which.max(x$alpha.drop$raw_alpha)
  if(x$alpha.drop$raw_alpha[whch] > x$total$raw_alpha){
    print(paste0(rownames(x$alpha.drop)[whch],": ",
                 round(x$alpha.drop$raw_alpha[whch],3)," vs ",
                 round(x$total$raw_alpha,3)))
    print("--------------")
    return(alpha.recursive(dta[,-whch]))
  }
  if(!x$alpha.drop$raw_alpha[whch] > x$total$raw_alpha){
    return(dta)
  }
}


# Gymnasium 2 functions ---------------------------------------------------

# Add missing factor levels
fill.levels = function(x,y,...){
  # x the data frame
  # y the qustions as character vector
  tmp = x[,grepl(y,names(x),fixed=T)]
  maxlev = which.max(unlist(lapply(tmp,function(x) length(levels(x)))))
  maxlev = levels(tmp[,maxlev])
  tmp = do.call(cbind.data.frame,lapply(tmp,function(x,y) x=factor(x,levels=y),y=maxlev))
  x[,grepl(y,names(x),fixed=T)] = tmp
  return(x)
}

# Rename factor levels new
rm.char.factor = function(x){
  # x the factor vector
  if(is.factor(x)){
    shitbug = c("æ","ø","å")
    for(i in shitbug){
      x = tolower(x)
      x = gsub(" ","",x,fixed=T)
      x = iconv(x, "latin1", "ASCII", sub="")
    }
    #x = factor(x)
    return(x)
  }
  if(!is.factor(x)){
    return(x)
  }
}

# Cramer's V
cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
  print.noquote("Cramér V / Phi:")
  return(as.numeric(CV))
}

