.makeNewFactorAnalysisDialog<-function(){

#make dialog
	dialog <- new(SimpleRDialog)
	dialog$setSize(500L,400L)
	dialog$setTitle("Factor Analysis")
	
	#add variable selector
	variableSelector <- new(VariableSelectorWidget)
	variableSelector$setTitle("data")
	addComponent(dialog,variableSelector,10,400,850,10)
	
	#add a list for the variables
	variableList<- new(VariableListWidget,variableSelector)
	variableList$setTitle("variables")
	addComponent(dialog, variableList,100,900,450, 420)
	
#Add an 'Options' button
	JButton <- J("javax.swing.JButton")
	button <- new(JButton,"Options")
	addComponent(dialog,button,600,825,700,625)

#make Options Dialog
	subDialog <- new(SimpleRSubDialog,dialog,"Factor Analysis: Options")
	setSize(subDialog,350,500)


combo <- new(J("org.rosuda.deducer.widgets.ComboBoxWidget"),"Extraction",c("principal components analysis", "maximum likelood","principal axis factoring","minimum residual (OLS)","weighted least squares (WLS)","generalized weighted least squares (GLS)"))
addComponent(subDialog, combo,1,900,200, 100)	
combo$setDefaultModel("maximum likelood")


combo1 <-  new(J("org.rosuda.deducer.widgets.ComboBoxWidget"),"Rotation",c("none", "varimax", "quartimax", "bentlerT", "geominT", "promax", "oblimin", "simplimax", "bentlerQ",  "geominQ", "cluster"))
	addComponent(subDialog, combo1,250,900,450, 100)
	combo1$setDefaultModel(c("promax"))
	
	# n of factors 
textArea <- new(TextAreaWidget,"Number of factors")
	addComponent(dialog, textArea,725,860,875, 590)
textArea$setDefaultModel(c("2"))

#options for sorting factor loadings
	transBoxesp <- new(CheckBoxesWidget,"Loadings",c("Sorted by size","Cut loadings less than:"))
	addComponent(subDialog, transBoxesp,500,640,760, 100)

	# value of loadings suppressed
textArea0 <- new(TextAreaWidget)
	addComponent(subDialog, textArea0,670,760,730, 650)
textArea0$setDefaultModel(c("0.2"))

#Listen for the button to be pressed
	ActionListener <- J("org.rosuda.deducer.widgets.event.RActionListener")
	actionFunction <- function(cmd,ActionEvent){
		subDialog$setLocationRelativeTo(button)
		subDialog$run()
	}
	listener <- new(ActionListener)
	listener$setFunction(toJava(actionFunction))
	button$addActionListener(listener)

#Add an 'Plots' button
	JButton1 <- J("javax.swing.JButton")
	button1 <- new(JButton1,"Plots")
	addComponent(dialog,button1,475,825,575,625)

#make Plots Dialog
	subDialog1 <- new(SimpleRSubDialog,dialog,"Factor Analysis: Plots")
	setSize(subDialog1,280,200)


#options for plotting the variables
	transBoxes <- new(CheckBoxesWidget,"Plots",c("path diagram","parallel analysis"))
	addComponent(subDialog1, transBoxes,1,900,480, 100)
	

#Listen for the button1 to be pressed
	ActionListener1 <- J("org.rosuda.deducer.widgets.event.RActionListener")
	actionFunction1 <- function(cmd,ActionEvent){
		subDialog1$setLocationRelativeTo(button1)
		subDialog1$run()
	}
	listener1 <- new(ActionListener1)
	listener1$setFunction(toJava(actionFunction1))
	button1$addActionListener(listener1)


#required library ###############################################
status<-.deducer$requirePackage("GPArotation")
if(status=="installed"){
	execute("library('GPArotation')")
	}else if(status=="not installed"){
		stop("package GPArotation required")}
				
status1<-.deducer$requirePackage("psych")					
if(status1=="installed"){
	execute("library('psych')")
	}else if(status1=="not installed"){
		stop("package psych required")}
		
status2<-.deducer$requirePackage("mvnormtest")					
if(status2=="installed"){
	execute("library('mvnormtest')")
	}else if(status2=="not installed"){
		stop("package mvnormtest required")}
###################################################################




#make sure at least two variables are selected###################			
.factorAnalysisCheckFunction <- function(state){
	if(length(state$variables)<2)
		return("Please select at least two variables")
	return("")
}
###################################################################
####################required package#######
status1<-.deducer$requirePackage("psych")					
if(status1=="installed"){
	execute("library('psych')")
	}else if(status1=="not installed"){
		stop("package psych required")}


.factorAnalysisRunFunction<- function(state) {
 #print(state) #a print statement is useful for debugging




# data frame ###################	
ddf<-state$variables
ddga<-state$data
ddfa<-paste(ddga, "$", sep="")	 
ddf<-paste(ddfa, ddf, sep="")
form <- paste(ddf," ")
form<-paste(form)
forma<-paste(form[1])
for( var in form[-1])
forma<-paste(forma,", ", var)
cmd<- paste("pr.model<-data.frame(", forma) 
cmd<-paste(cmd, ")")
cmd <- paste (cmd)


# state rotation ###################
if("none" %in%state$Rotation)
		rota <- "\"none\""
if("varimax" %in%state$Rotation)
		rota <- "\"varimax\""
if("quartimax" %in%state$Rotation)
		rota <- "\"quartimax\""
if("bentlerT" %in%state$Rotation)
		rota <- "\"bentlerT\""
if("geominT" %in%state$Rotation)
		rota <- "\"geominT\""
if("promax" %in%state$Rotation)
		rota <- "\"promax\""
if("oblimin" %in%state$Rotation)
		rota <- "\"oblimin\""
if("simplimax" %in%state$Rotation)
		rota <- "\"simplimax\""
if("bentlerQ" %in%state$Rotation)
		rota <- "\"bentlerQ\""
if("geominQ" %in%state$Rotation)
		rota <- "\"geominQ\""
if("cluster" %in%state$Rotation)
		rota <- "\"cluster\""

# state extration ###################
if("maximum likelood" %in%state$Extraction)
		extra <- "\"ml\""
if("principal axis factoring" %in%state$Extraction)
		extra <- "\"pa\""
if("minimum residual (OLS)" %in%state$Extraction)
		extra <- "\"minres\""
if("weighted least squares (WLS)" %in%state$Extraction)
		extra <- "\"wls\""
if("generalized weighted least squares (GLS)" %in%state$Extraction)
		extra <- "\"gls\""
if("principal components analysis" %in%state$Extraction)
		extra <- "\"gls\""

# PCA ###################
cmd7<- {cmd7<- paste("pr.model1<-principal(pr.model,") 
cmd7<-paste(cmd7, "nfactors=", state$`Number of factors`, ",")
cmd7<-paste(cmd7, "rotate=", rota)
cmd7<-paste(cmd7, ")")}

# Factor analysis ###################
cmd8<- {cmd8<- paste("pr.model1<-fa(pr.model,") 
cmd8<-paste(cmd8, "nfactors=", state$`Number of factors`, ",")
cmd8<-paste(cmd8, "rotate=", rota, ",")
cmd8<-paste(cmd8, "fm=", extra)
cmd8<-paste(cmd8, ")")}


# print function ###################
ifelse("principal components analysis" %in%state$Extraction, cmd11<- paste (cmd7),cmd11 <- paste (cmd8))

# Loadings options ###################
if(sum(nchar(state$Loadings))== 0)
{cmd12<-paste("print(pr.model1)")}

if(sum(nchar(state$Loadings))== 14)
{cmd12<-paste("print.psych(pr.model1,sort=T)")}

if(sum(nchar(state$Loadings))== 23)
{cmd12<-paste("print.psych(pr.model1,cut=",state$null,")")}

if(sum(nchar(state$Loadings))== 37)
{cmd12<-paste("print.psych(pr.model1,sort=T, cut=",state$null,")")}

# Plot options ###################
if(sum(nchar(state$Plots))== 29)		
{plot12 <- paste("JavaGD(width=800, height=600, ps=12)")
plot22 <- paste("par(mfrow=c(2,1))")
cmd02<-paste("pr.model3<- fa.parallel(pr.model)")
cmd01<-paste("pr.model2<- fa.diagram(pr.model1)")}

if (sum(nchar(state$Plots))== 12)	
{plot1 <- paste("JavaGD(width=800, height=600, ps=12)")
cmd1<-paste("pr.model2<- fa.diagram(pr.model1)")}

if (sum(nchar(state$Plots))== 17)	
{plot1 <- paste("JavaGD(width=800, height=600, ps=12)")
cmd2<-paste("pr.model3<- fa.parallel(pr.model)")}



cmd000<-paste("print(mshapiro.test(t(na.omit(as.matrix(pr.model)))))")


# remove data frame ###################
cmd13<-paste("rm(pr.model)")

# execute sequence ###################
if(sum(nchar(state$Plots))== 29)
comm<-paste(cmd,"\n", cmd11,"\n",cmd12,"\n",plot12,"\n",plot22,"\n",cmd01,"\n",cmd02,"\n",cmd000,"\n",cmd13)

if (sum(nchar(state$Plots))== 12)
comm<-paste(cmd,"\n",cmd11,"\n",cmd12,"\n",plot1,"\n",cmd1,"\n",cmd000,"\n",cmd13)

if (sum(nchar(state$Plots))== 17)
comm<-paste(cmd,"\n",cmd11,"\n",cmd12,"\n",plot1,"\n",cmd2,"\n",cmd000,"\n",cmd13)

if (sum(nchar(state$Plots))== 0)
comm<-paste(cmd,"\n",cmd11,"\n",cmd12,"\n",cmd000,"\n",cmd13)

execute(comm)
}

dialog$setCheckFunction(toJava(.factorAnalysisCheckFunction))
	dialog$setRunFunction(toJava(.factorAnalysisRunFunction))
	dialog
}

.getNewFactorAnalysisDialog <- function(){
	if(!exists(".newFactorAnalysisDialog")){
		ex <- globalenv()
		#make factor analysis dialog
		.newFactorAnalysisDialog <- .makeNewFactorAnalysisDialog()
		assign(".newFactorAnalysisDialog",.newFactorAnalysisDialog,ex)		
	}
	return(.newFactorAnalysisDialog)
}

###################################### 

#            Reliabiltity 	         #

######################################	


.makeNewReliabilityDialog<-function(){

#make dialog
dialog <- new(SimpleRDialog)
dialog$setSize(500L,400L)
dialog$setTitle("Cronbach Alpha")

#add variable selector
variableSelector <- new(VariableSelectorWidget)
variableSelector$setTitle("data")
addComponent(dialog,variableSelector,10,400,850,10)

#add a list for the variables
variableList<- new(VariableListWidget,variableSelector)
variableList$setTitle("variables")
addComponent(dialog, variableList,100,900,450, 420)
 


#make sure at least two variables are selected###################			
.ReliabilityCheckFunction <- function(state){
	if(length(state$variables)<2)
		return("Please select at least two variables")
	return("")
}



.ReliabilityRunFunction <- function(state){

### data.frame ########
ddf<-state$variables
ddga<-state$data
ddfa<-paste(ddga, "$", sep="")		 
ddf<-paste(ddfa, ddf, sep="")
form <- paste(ddf," ")
form<-paste(form)
forma<-paste(form[1])
for( var in form[-1])
forma<-paste(forma,", ", var)

cmd<- paste("pr.model<-data.frame(", forma) 

cmd<-paste(cmd, ")")

cmd <- paste (cmd)



## model ########
cmd1<- paste("pr.model1<-alpha(pr.model") 

cmd1<-paste(cmd1, ")")

cmd1 <- paste (cmd1,"\n","print(pr.model1)")

# remove data frame ###################
cmd13<-paste("rm(pr.model)")

comm<-paste(cmd,"\n",cmd1,"\n",cmd13)

execute(comm)
}

dialog$setCheckFunction(toJava(.ReliabilityCheckFunction))
	dialog$setRunFunction(toJava(.ReliabilityRunFunction ))
	dialog
}

.getNewReliabilityDialog <- function(){
	if(!exists(".newReliabilityDialog")){
		ex <- globalenv()
		#make factor analysis dialog
		.newReliabilityDialog <- .makeNewReliabilityDialog()
		assign(".newReliabilityDialog",.newReliabilityDialog,ex)		
	}
	return(.newReliabilityDialog)
}

.First.lib <- function(libname, pkgname) { 
deducer.addMenu("Psych")
	deducer.addMenuItem("Factor analysis",,".getNewFactorAnalysisDialog()$run()","Psych")
	if(.windowsGUI){
		winMenuAdd("Psych")
		winMenuAddItem("Psych", "Factor analysis", "deducer('Factor analysis')")
	}else if(.jgr){
		jgr.addMenu("Psych")
		jgr.addMenuItem("Psych", "Factor analysis", "deducer('Factor analysis')")
	}
deducer.addMenuItem("Reliability",,".getNewReliabilityDialog()$run()","Psych")
	if(.windowsGUI){
		winMenuAdd("Psych")
		winMenuAddItem("Psych", "Reliability", "deducer('Reliability')")
	}else if(.jgr){
		jgr.addMenu("Psych")
		jgr.addMenuItem("Psych", "Reliability", "deducer('Reliability')")
	}
}

