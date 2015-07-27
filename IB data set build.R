plot.folder="/Users/eric8226/Box Sync/Dedicated user base survey/analyses/plots/descriptive stats/"
sdat=read.csv("/Users/eric8226/Box Sync/Dedicated user base survey/analyses/raw data/Dedicated_IB_usage_survey__Main.csv",skip=1)

# -----------
# functions
# -----------
tonum=function(x,keep.rm=F) {
  nr=nrow(x)
  nc=ncol(x)
  vals=as.numeric(as.matrix(x))
  if (max(vals,na.rm=T)==1 & keep.rm==F) {
    vals[is.na(vals)]=0
  }
  matrix(vals,nr,nc)
}

# process multiple choice and multiple response items
mcmr=function(name,qtext,alt.cnames=colnames(qtext)) {
  indxs=grepl(qtext,colnames(sdat))
  colnames(sdat)[indxs]=substr(colnames(sdat)[indxs],nchar(qtext)+1,1000)
  
  # convert to numeric matrix
  qmat=tonum(as.matrix(sdat[,indxs]))
  # set question scale type (1=binary, 2=ratings)
  qscale=(max(qmat,na.rm=1)>1)*1+1
  colnames(qmat)=colnames(sdat)[indxs]
  colnames(qmat)=alt.cnames
  # create summary vector
  qmat.sum=sort(apply(qmat,2,mean,na.rm=1),decreasing=T)
  other.col=grep("Other text",names(qmat.sum))
  if (length(other.col)>0) {qmat.sum=qmat.sum[-other.col]}
  
  # create and save output list
  mcmr.out=list(mat=qmat,sumvec=qmat.sum)
  eval(parse(text=paste(name,"<<-mcmr.out",sep="")))
  
  # save matrix to allvars data frame
  qmat.allvars=qmat
  colnames(qmat.allvars)=gsub("[[:punct:]]","",colnames(qmat.allvars))
  colnames(qmat.allvars)=paste(name,colnames(qmat.allvars),sep=".")
  colnames(qmat.allvars)=gsub(" ",".",colnames(qmat.allvars))
  allvars=cbind(allvars,qmat.allvars)
  allvars<<-allvars
  
  # plot
  res=200
  png(paste(plot.folder,name,".png",sep=""),
      res*10,res*5,res=res)
  par(xpd=T)
  par(mar=c(7,4,2,2))
  xs=barplot(qmat.sum,ylim=c(0,c(1,max(qmat,na.rm=1))[qscale]),xaxt="n",main=name,
             ylab=c("% of respondents","average rating")[qscale])
  text(xs,-max(qmat.sum)/8,names(qmat.sum),srt=45,adj=c(1,0))
  dev.off()
}

mcsr=function(name,qtext,opts) {
  indx=grepl(qtext,colnames(sdat))
  resp=as.factor(as.character(sdat[,indx]))
  levels(resp)=opts[as.numeric(levels(resp))]
  
  # save response
  eval(parse(text=paste("allvars$",name,"=resp",sep="")))
  allvars<<-allvars
  
  # plot
  res=200
  png(paste(plot.folder,name,".png",sep=""),
      res*7,res*5,res=res)
  par(xpd=T)
  par(mar=c(2,4,2,4))
  pie.cols=colorRampPalette(c(rgb(190,48,46,max=255),
                              rgb(247,247,236,max=255)))(length(3:(length(table(resp))+3)))
  pie(table(resp),col=pie.cols)
  dev.off()
}

# --------------------------
# create master data frame
# --------------------------
allvars=data.frame(email=sdat$Email)

# ------------------
# time to complete
# ------------------
ttc.qualt=function(stime,ftime,save.all=F) {
  t.start=matrix(as.numeric(unlist(strsplit(substr(stime,12,19),":"))),length(stime),3,byrow=T)
  t.end=matrix(as.numeric(unlist(strsplit(substr(ftime,12,19),":"))),length(ftime),3,byrow=T)
  ttc=(t.end%*%c(3600,60,1)-t.start%*%c(3600,60,1))/60
  d.start=matrix(as.numeric(unlist(strsplit(substr(stime,1,10),"-"))),length(stime),3,byrow=T)
  d.end=matrix(as.numeric(unlist(strsplit(substr(ftime,1,10),"-"))),length(ftime),3,byrow=T)
  dtc=d.end%*%c(0,0,1)-d.start%*%c(0,0,1)
  dt.end=t.end[ttc<0,]%*%c(3600,60,1)+24*dtc[ttc<0]*60*60
  dt.start=t.start[ttc<0,]%*%c(3600,60,1)
  ttc[ttc<0]=(dt.end-dt.start)/60
  if (save.all==T) {
    return(list(ttc=ttc,
                dt.start=dt.start,
                dt.end=dt.end))
  }
  else {
    return(ttc)
  }
}
ttc=ttc.qualt(sdat$StartDate,sdat$EndDate)
ttc=ceiling(ttc)
allvars=cbind(allvars,ttc)
# lump all over 30 minutes together
ttc[ttc>30]=31

res=200
png(paste(plot.folder,"time to complete",".png",sep=""),
    res*10,res*5,res=res)
par(xpd=T)
hist(ttc,breaks=1:31,main="Time to complete",xlab="minutes")
dev.off()

# job roles
mcmr("roles",
     "Which.of.the.following.best.describe.s..your.primary.job.responsibilities..Select.all.that.apply..",
     c("Dev","Dev Manager","Design/UX","IT","C-level","Owner","Admin","Finance","Marketing","Sales/PR","Other","Other text"))

# IT decision maker?
mcsr("IT.DM",
     "How.would.you.describe.your.level.of.influence.in.deciding.which.hosting.service.provider.your.co...",
     c("sole","shared","no","DK"))

# industry
ind.codes=read.csv("/Users/eric8226/Box Sync/Cloud user base survey/analyses/other/industry coding.csv",header=F)
colnames(ind.codes)=c("code","industry")
mcsr("industry",
     "Please.select.your.company.s.primary.industry.",
     ind.codes$industry)

# size
csize.codes=read.csv("/Users/eric8226/Box Sync/Cloud user base survey/analyses/other/company size coding.csv",header=F)
colnames(csize.codes)=c("code","csize")
mcsr("company.size",
     "About.how.many.employees.does.your.company.have.at.all.locations.worldwide..Your.best.estimate.is...",
     csize.codes$csize)

# revenue
crev.codes=read.csv("/Users/eric8226/Box Sync/Cloud user base survey/analyses/other/company revenue coding.csv",header=F)
colnames(crev.codes)=c("code","crev")
opts=rep("",max(crev.codes$code))
opts[crev.codes$code]=as.character(crev.codes$crev)
mcsr("company.revenue",
     "About.how.much.annual.revenue.does.your.company.generate",
     opts)

# IT budget platforms breakdown
mcmr("IT.platforms",
     "Estimate.how.much.of.your.company.s.IT.budget.is.allocated.to.each.of.these.IT.management.deploym....",
     c("IH ded","IH privcl","IH privcl man","3p privcl man","3p pubcl","3p colo","3p ded"))

# cloud feature importance today
mcmr("cloud_features",
     "As.of.today..how.important.are.the.following.aspects.of.hosting.service.providers.for.your.company..",
     c("Management","Performance","Hybrid","Ease of use","Price","Global infr.","Reliability","Support","Tech expertise","Other","Other text","Prof. services","Security","Compliance"))

# hybrid feature importance today
mcmr("hybrid_features",
     "How.important.are.the.following.aspects.when.evaluating.a.hybrid.service.provider..one.that.offer....",
     c("Other","Other text","Combined UI","Combined invoice","Combined tickets","Performance single","Performance across"))

# use other providers?
mcsr("other_clouds",
     "Does.your.company.use.any.cloud.providers.other.than.Rackspace.",
     c("yes","no"))

# which other clouds?
mcmr("other_clouds_which",
     "Which.other.cloud.providers.does.your.company.use",
     c("AWS","Google","Microsoft","IBM","DigitalOcean","Other","Other text"))

allvars$other_clouds_percent_Rax=sdat$What.percentage.of.your.company.s.total.IT
others=sdat$Which.other.cloud.providers.does.your.company.use..Select.all.that.apply..Other..TEXT
others=as.character(others)
allvars$other_clouds_which.Linode=grepl("Linode",others)*1
# then reverse "other" responses with just Linode
allvars$other_clouds_which.Other[others=="Linode"]=0

# which other clouds?
mcsr("other_clouds_next_workload",
     "Where.do.you.plan.to.run.your.company.s.next.workload.",
     c("Rackspace","other","DK"))

# Why choose Rackspace?
mcmr("whyR",
     "Why.did.your.company.originally.choose.the.Rackspace.cloud.over.other.clouds..Select.all.that.app....",
     c("Performance","Hybrid","Ease of use","Global infr.",
       "Don't know","Other","Other text","Price","Recommended",
       "Already using Rax","All services needed","Reliability",
       "Didn't help decide","Management","Support"))

# workloads
mcmr("workload",
     "What.workloads.are.running.on.your.company.s.Rackspace.cloud",
     c("Corporate website","Ecommerce","Apps or mobile","Dev and test",
       "Other","Other text","Web services","Databases"))

# looking for managed
mcsr("looking_for_managed",
     "When.your.company.started.with.Rackspace.were.they.specifically.looking.for",
     c("yes","no","unsure","DK"))

# services used
mcmr("services_used",
     "Which.of.the.following.Rackspace.managed.cloud.services.have.been.used.by.your.company",
     c("Support","Architecture advice","Launch assist","Security","Backup",
       "App services manage","Platform maintenance","Monitoring","Named AM",
       "Config management","Scaling","Logs","DevOps"))

# product awareness
mcmr("prod_awareness",
     "Which.of.the.following.Rackspace.cloud.products.are.you.aware.of",
     c("Load Balancers","Block Storage","CDN","OnMetal","Orchestration","Monitoring",
       "Databases","Big Data","Auto Scale","Cloud.Files"))

# support ratings
mcmr("support_ratings",
     "To.what.extent.do.you.agree.with.the.following.statements\\.\\.",
     c("Value","Utilize","Reactive","Proactive","Competent","Findable","Documentation"))

# support frequency
mcsr("support_frequency",
     "How.frequently.does.your.company.contact.Rackspace.for.support.",
     c("daily","weekly","monthly","rarely","never"))

# issues
mcmr("issues",
     "To.what.extent.have.the.following.issues.at.Rackspace.impacted.your.company",
     c("Data loss","M and S downtime","Other downtime","Network","Reboots"))

# pricing ratings
mcmr("pricing_ratings",
     "To.what.extent.do.you.agree.with.the.following.statements.related.to.Rackspace.pricing..",
     c("confusing","spend estimate","billing","value"))

# satisfaction overall
mcsr("satisfaction_overall",
     "Overall..how.satisfied.is.your.company.with.your.Rackspace.cloud.services.",
     1:6)

# -----------------------------------
# re-code reverse wording responses
# -----------------------------------
allvars$support_ratings.Findable=
  7-as.numeric(as.character(allvars$support_ratings.Findable))

allvars$pricing_ratings.confusing=
  7-as.numeric(as.character(allvars$pricing_ratings.confusing))

allvars$pricing_ratings.spend.estimate=
  7-as.numeric(as.character(allvars$pricing_ratings.spend.estimate))

allvars$satisfaction_overall=
  7-as.numeric(as.character(allvars$satisfaction_overall))

# ---------------------------------------------------------------------------
# remove people who did not take survey via email (i.e., are not traceable)
# ---------------------------------------------------------------------------
allvars=allvars[allvars$email!="",]

# -----------------------------------------------
# format numbers as numbers, factors as factors
# -----------------------------------------------
for (i in 1:ncol(allvars)) {
  allvars[,i]=as.character(allvars[,i])
  allvars.i.no.na=allvars[,i][!is.na(allvars[,i])]
  if (sum(is.na(as.numeric(allvars.i.no.na)))>0) {
    allvars[,i]=as.factor(allvars[,i])
  }
  else {
    allvars[,i]=as.numeric(as.character((allvars[,i])))
  }
}

