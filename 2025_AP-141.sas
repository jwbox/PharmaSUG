/* Code for PharmaSUG 2025 Paper AP-141  */

/* Check R Packages */
proc iml;
    submit / R;
      ver = R.version.string
      vdf = data.frame(version = ver)
      pkg = installed.packages()[,c(1,3)]
    endsubmit;
call ImportDataSetFromR("R_pkg", "pkg");
call ImportDataSetFromR("R_ver", "vdf");
quit;

data _null_;
set R_ver;
CALL SYMPUTX("RVer",version);
RUN;


TITLE "R Version:  &Rver";
proc print data = R_pkg; run;
/****************************************************************/

/*Python Package Listing */

proc python;
submit;
 
import importlib.metadata
import pandas as pd
import sys

package_list = [(dist.metadata["Name"], dist.metadata["Version"]) for dist in importlib.metadata.distributions()]

df = pd.DataFrame(package_list)
df.columns = ["Package","Version"]


ds = SAS.df2sd(df.sort_values(by='Package'), 'work.PythonPackages')

pver = sys.version
SAS.symput('PythonVer',pver)

endsubmit;
run;


TITLE "Python Version: &PythonVer";
PROC SQL;
SELECT Package, Version
FROM WORK.PYTHONPACKAGES
ORDER BY Upcase(Package);
QUIT;
/****************************************************************/

/* Code Preset in the model.r program    */

var <- gw$args
print(var)

caslib <- paste(var['caslib'])
castbl <- paste(var['castbl'])

df <- read_table(list(name= castbl,caslib = caslib))
print(head(df))

model = eval(parse(text = var['mod']))

print(summary(model))
/****************************************************************/
/* PROC CAS call to the model.r program */
%LET caslib = PUBLIC;
%LET castbl = HEART;
%let modelCode = glm(as.factor(Status) ~ AgeAtStart + Systolic + Diastolic + Weight + Cholesterol + Sex, data=df,family = binomial);

proc cas;
	filename rcode '/nfsshare/sashls2/home/jimbox/model.r';
	code=readfile('rcode');
	action gateway.runLang / 
		args={caslib="&caslib", castbl = "&castbl", mod="&modelCode"}, 
		code=code, 
		lang="R", 
		log_level="INFO",
		single=true;
run;

quit;
/****************************************************************/


/* PROC CAS with code in a submit block  */

PROC CAS;
externalsource Rprog1;

df <- read_table(list(name= "HEART",caslib = "PUBLIC"))

model <- glm(as.factor(Status) ~ AgeAtStart + Systolic + Diastolic + Weight + Cholesterol + Sex, 
             data=df, 
             family = binomial)

print(summary(model))
mdf = as.data.frame(summary(model)$coefficients)
mdf$Vars = rownames(mdf)

mdf <- mdf[, c("Vars", names(mdf)[-ncol(mdf)])]
mdf

write_table(mdf, list(name = 'Heart_Model', caslib = 'casuser', replace = TRUE))

endexternalsource;

	action gateway.runLang / 
		 
		code=Rprog1, 
		lang="R", 
		log_level="INFO",
		single=true;
run;

PROC PRINT data = casuser.heart_model; run;
proc sql; select * from casuser.heart_model; quit;
/****************************************************************/


/**  Submitting the code Via Proc Gateway */

PROC GATEWAY single lang = R;
submit;

df <- read_table(list(name= "HEART",caslib = "PUBLIC"))

model <- glm(as.factor(Status) ~ AgeAtStart + Systolic + Diastolic + Weight + Cholesterol + Sex, 
             data=df, 
             family = binomial)

mdf = as.data.frame(summary(model)$coefficients)
mdf$Vars = rownames(mdf)

mdf <- mdf[, c("Vars", names(mdf)[-ncol(mdf)])]

write_table(mdf, list(name = 'Heart_Model', caslib = 'casuser', replace = TRUE))

endsubmit;
run;




/****************************************************************/


/* PROC CAS with code in a submit block to be multi threaded*/

PROC CAS;
externalsource Rprog1;
	df <- read_table(list(caslib= 'public', name= 'heart', groupby='Sex', groupbyMode = 'redistribute'))
	print(gw$num_threads)
	if(nrow(df)>0){
	cat("Thread", gw$thread_id)
	model <- glm(as.factor(Status) ~ AgeAtStart + Systolic + Diastolic + Weight + Cholesterol,
		data=df,
		family = binomial)
	assign(paste("model_",df$Sex[1], sep = ""),model)
	mdf = as.data.frame(summary(model)$coefficients)
		mdf$Vars = rownames(mdf)
		mdf <- mdf[, c("Vars", names(mdf)[-ncol(mdf)])]
	assign(paste("mdf_",df$Sex[1], sep = ""),mdf)
	print(df$Sex[1])
	head(assign(paste("mdf_",df$Sex[1], sep = ""),mdf), 10)	
	}
endexternalsource;
	action gateway.runLang / 		 
		code=Rprog1, 
		lang="R", 
		log_level="INFO",
		NTHREADS=2;
run;


/****************************************************************/


/**  Submitting the code to be multi threaded Via Proc Gateway */

PROC GATEWAY NTHREADS=2 lang=R;
submit;
	
	df <- read_table(list(caslib= 'public', name= 'heart', groupby='Sex', groupbyMode = 'redistribute'))
	
	print(gw$num_threads)
	
	if(nrow(df)>0){
	
	cat("Thread", gw$thread_id)
	
	model <- glm(as.factor(Status) ~ AgeAtStart + Systolic + Diastolic + Weight + Cholesterol,
		data=df,
		family = binomial)
	
	assign(paste("model_",df$Sex[1], sep = ""),model)
	
	mdf = as.data.frame(summary(assign(paste("model_",df$Sex[1], sep = ""),model))$coefficients)
		mdf$Vars = rownames(mdf)
		mdf <- mdf[, c("Vars", names(mdf)[-ncol(mdf)])]
	
	assign(paste("mdf_",df$Sex[1], sep = ""),mdf)
	
	print(df$Sex[1])
	head(assign(paste("mdf_",df$Sex[1], sep = ""),mdf), 10)
	}
	
endsubmit;
RUN;
