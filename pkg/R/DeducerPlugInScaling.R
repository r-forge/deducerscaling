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
	setSize(subDialog,350,350)
	subDialog$setResizable(FALSE)

	combo <- new(J("org.rosuda.deducer.widgets.ComboBoxWidget"),
					c("principal components analysis", "maximum likelood",
					"principal axis factoring","minimum residual (OLS)",
					"weighted least squares (WLS)","generalized weighted least squares (GLS)"))
	combo$setTitle("Extraction",TRUE)
	addComponent(subDialog, combo,50,900,225, 100)	
	combo$setDefaultModel("maximum likelood")


	combo1 <-  new(J("org.rosuda.deducer.widgets.ComboBoxWidget"),"Rotation",
					c("none", "varimax", "quartimax", "bentlerT", "geominT", 
					"promax", "oblimin", "simplimax", "bentlerQ",  "geominQ", "cluster"))
	addComponent(subDialog, combo1,275,900,450, 100)
	combo1$setDefaultModel(c("promax"))
	
	# n of factors 
	textArea <- new(TextFieldWidget,"# of factors")
	addComponent(dialog, textArea,725,825,850, 625)
	textArea$setDefaultModel(c("2"))
	textArea$setInteger(TRUE)
	textArea$setLowerBound(1)

	#options for sorting factor loadings
	transBoxesp <- new(CheckBoxesWidget,c("Sorted by size","Cut loadings less than:"))
	transBoxesp$setTitle("Loadings")
	addComponent(subDialog, transBoxesp,550,625,760, 100)

	# value of loadings suppressed
	textArea0 <- new(TextFieldWidget)
	addComponent(subDialog, textArea0,670,740,755, 600)
	textArea0$setDefaultModel(c("0.2"))
	textArea0$setNumeric(TRUE)
	textArea0$setLowerBound(0)
	
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
	setSize(subDialog1,280,150)
	subDialog1$setResizable(FALSE)

	#options for plotting the variables
	transBoxes <- new(CheckBoxesWidget,"Plots",c("path diagram","parallel analysis"))
	addComponent(subDialog1, transBoxes,50,800,650, 200)
	

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
		cmd8<-paste(cmd8, "nfactors=", textArea$getModel(), ",")
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
			{cmd12<-paste("print.psych(pr.model1,cut=",textArea0$getModel(),")")}
		
		if(sum(nchar(state$Loadings))== 37)
			{cmd12<-paste("print.psych(pr.model1,sort=T, cut=",textArea0$getModel(),")")}
		
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



########################################################################
#
#				Intraclass correlation coefficient
#
########################################################################
.makeICCDialog <- function(){
	
	
	RFunction <- J("org.rosuda.deducer.widgets.param.RFunction")
	RFunctionDialog <- J("org.rosuda.deducer.widgets.param.RFunctionDialog")
	ParamCharacter <- J("org.rosuda.deducer.widgets.param.ParamCharacter")
	ParamNumeric <- J("org.rosuda.deducer.widgets.param.ParamNumeric")
	ParamMultipleVariables <- J("org.rosuda.deducer.widgets.param.ParamMultipleVariables")
	
	iccFunc <- new(RFunction,"icc")
	
	ratingsParam <- new(ParamMultipleVariables,"ratings")
	iccFunc$add(ratingsParam)
	
	modelParam <- new(ParamCharacter,"model","oneway")
	modelParam$setOptions(c("oneway", "twoway"))
	modelParam$setViewType(modelParam$VIEW_COMBO)
	iccFunc$add(modelParam)
	
	typeParam <- new(ParamCharacter,"type","consistency")
	typeParam$setOptions(c("consistency", "agreement"))
	typeParam$setViewType(typeParam$VIEW_COMBO)
	iccFunc$add(typeParam)
	
	unitParam <- new(ParamCharacter,"unit","single")
	unitParam$setOptions(c("single", "average"))
	unitParam$setViewType(unitParam$VIEW_COMBO)
	iccFunc$add(unitParam)
	
	nullParam <- new(ParamNumeric,"r0")
	nullParam$setTitle("H0")
	nullParam$setValue(0)
	nullParam$setLowerBound(0)
	iccFunc$add(nullParam)
	
	conf <- new(ParamNumeric,"conf.level",.95)
	conf$setLowerBound(0)
	conf$setUpperBound(1)
	iccFunc$add(conf)
	
	rfd <- new(RFunctionDialog, iccFunc)
	rfd$setSize(425L,500L)
	rfd$setLocationRelativeTo(.jnull())
	rfd
}

.getICCDialog <- function(){
	if(!exists(".iccDialog")){
		ex <- globalenv()
		#make factor analysis dialog
		.iccDialog <- .makeICCDialog()
		assign(".iccDialog",.iccDialog,ex)		
	}
	return(.iccDialog)
}

.First.lib <- function(libname, pkgname) { 

	deducer.addMenu("Psych")
	deducer.addMenuItem("Factor analysis",,".getNewFactorAnalysisDialog()$run()","Psych")
	deducer.addMenuItem("Reliability",,".getNewReliabilityDialog()$run()","Psych")	
	deducer.addMenuItem("Intraclass correlation",,".getICCDialog()$run()","Psych")	
	if(.windowsGUI){
		winMenuAdd("Psych")
		winMenuAddItem("Psych", "Factor analysis", "deducer('Factor analysis')")
		winMenuAddItem("Psych", "Reliability", "deducer('Reliability')")
		winMenuAddItem("Psych", "Intraclass correlation", "deducer('Intraclass correlation')")
	}else if(.jgr){
		if(exists("jgr.getMenuNames") && exists("jgr.insertMenu")){
			menus <- jgr.getMenuNames()
			index <- which(menus=="Packages & Data")
			if(length(index)==0) 
				index <- 1
			jgr.insertMenu("Psych",index)
		}else
			jgr.addMenu("Psych")
		
		jgr.addMenuItem("Psych", "Factor analysis", "deducer('Factor analysis')")
		jgr.addMenuItem("Psych", "Reliability", "deducer('Reliability')")
		jgr.addMenuItem("Psych", "Intraclass correlation", "deducer('Intraclass correlation')")
	}

}

