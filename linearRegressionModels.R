library(lmtest)
library(fmsb)
library(usdm)
library(leaps)
library(dplyr)

# INSTRUCTIONS -----------------------------------------------------------
# Step 1. Import the data set that you want to use.
# Step 2. Determine the predictor variables of the data set that you would like to use as well as your dependent variable, y.
# Step 3. Assign your predictor variables and independent variable to variables in R that you are initializing to them. 
# Step 4. Create a data frame of the imported data set
# Step 5. Assign the variable named, "numOfVariables" the value of the number of predictor variables that you are using.
# Step 6. Highlight all the code and run the program.
#  Adjust the size of the console window as needed to view all the models, p, Cp, AdjR^2, SSE, AIC, and PRESS values. 
# NOTE: the value of width for "model[i]" on the current line of 261 must be increased for larger numbers of
# predictor variables. 
# WARNING: This program is not dynamic and currently only works for a number of predictor variables less than or equal to twelve. 


# Step 3:
# Assign values to y, x1, x2, ... , xn  from the data set that you want to use. 
# In this case, we are using the APPENC03 data set with the following values. 
y = APPENC03$V2# market share of product
x1 = APPENC03$V3 # merchandise price
x2 = APPENC03$V4# advertising exposure
x3 = APPENC03$V5# presence or absence of wholesale pricing discount aka 1 or 0
x4 = APPENC03$V6# presence or absence of package promotion during the period aka 1 or 0
x5 = APPENC03$V8# year


# Step 4: 
# create a data frame of dataset
mrktShare <- APPENC03[c("V2","V3","V4","V5","V6","V8")] # y & all predictor variables
cpObj <- leaps(x=mrktShare[,2:6], y=mrktShare[,1], method="Cp") # There is probably a better way to do this. 


# Step 5: 
numOfvariables = 5 # number of predictor variables <<< THIS MUST BE CHANGED
# FOR EACH MODEL OR IT WILL GIVE ERROR

allModels <-"" # all the models as a String with + in between
models <-"" # all models as vectors
pValues <- ""
modelsDF <- data.frame(matrix(NA, nrow = length(APPENC03[1]$V1)))
adjR2vals <-""
SSEvals <-""
AICvals <-""
PRESSvals <-""

row=1
for(row in 1:length(cpObj$Cp)){ # looping from first to last row
  #allModels <- paste(tmpModels)
  tmpModels<-""
  tmpPs <-"" 
  tmpVecModels <-""
  
  tmpModelsDF <- data.frame(matrix(NA, nrow = length(APPENC03[1]$V1)))
  
  count= 0 # number of predictor variables in the current model (row)
  
  
  for(column in 1:numOfvariables){ # for each column
    if(cpObj$which[row,column]==TRUE){
      if(column==1){
        tmpModels <- paste(tmpModels, "x1", sep="") # x1
        tmpModelsDF$x1 <- x1
        tmpModelsDF <- select(tmpModelsDF, x1)
        
        
      } 
      if(column==2){
        if(count!=0){
          tmpModels <- paste(tmpModels, "x2", sep="+") # x2
          tmpModelsDF$x2 <- x2
        }
        if(count==0){
          tmpModels <- paste(tmpModels, "x2", sep="") # x2
          tmpModelsDF$x2 <- x2
          tmpModelsDF <- select(tmpModelsDF, x2)
          
        }
      } 
      if(column==3){
        if(count!=0){
          tmpModels <- paste(tmpModels, "x3", sep="+") # x3
          tmpModelsDF$x3 <- x3
        }
        if(count==0){
          tmpModels <- paste(tmpModels, "x3", sep="") # x3
          tmpModelsDF$x3 <- x3
          tmpModelsDF <- select(tmpModelsDF, x3)
          
        }
      }
      if(column==4){
        if(count!=0){
          tmpModels <- paste(tmpModels, "x4", sep="+") # x4
          tmpModelsDF$x4 <- x4
        }
        if(count==0){
          tmpModels <- paste(tmpModels, "x4", sep="") # x4
          tmpModelsDF$x4 <- x4
          tmpModelsDF <- select(tmpModelsDF, x4)
          
        }
      } 
      if(column==5){
        if(count!=0){
          tmpModels <- paste(tmpModels, "x5", sep="+") # x5
          tmpModelsDF$x5 <- x5
        }
        if(count==0){
          tmpModels <- paste(tmpModels, "x5", sep="") # x5
          tmpModelsDF$x5 <- x5
          tmpModelsDF <- select(tmpModelsDF, x5)
          
        }
      } 
      if(column==6){
        if(count!=0){
          tmpModels <- paste(tmpModels, "x6", sep="+") # x6
          tmpModelsDF$x6 <- x6
        }
        if(count==0){
          tmpModels <- paste(tmpModels, "x6", sep="") # x6
          tmpModelsDF$x6 <- x6
          tmpModelsDF <- select(tmpModelsDF, x6)
          
        }
      } 
      if(column==7){
        if(count!=0){
          tmpModels <- paste(tmpModels, "x7", sep="+") # x7
          tmpModelsDF$x7 <- x7
        }
        if(count==0){
          tmpModels <- paste(tmpModels, "x7", sep="") # x7
          tmpModelsDF$x7 <- x7
          tmpModelsDF <- select(tmpModelsDF, x7)
          
        }
      } 
      if(column==8){
        if(count!=0){
          tmpModels <- paste(tmpModels, "x8", sep="+") # x8
          tmpModelsDF$x8 <- x8
        }
        if(count==0){
          tmpModels <- paste(tmpModels, "x8", sep="") # x8
          tmpModelsDF$x8 <- x8
          tmpModelsDF <- select(tmpModelsDF, x8)
        }
      } 
      if(column==9){
        if(count!=0){
          tmpModels <- paste(tmpModels, "x9", sep="+") # x9
          tmpModelsDF$x9 <- x9
        }
        if(count==0){
          tmpModels <- paste(tmpModels, "x9", sep="") # x9
          tmpModelsDF$x9 <- x9
          tmpModelsDF <- select(tmpModelsDF, x9)
          
        }
      } 
      if(column==10){
        if(count!=0){
          tmpModels <- paste(tmpModels, "x10", sep="+") # x10
          tmpModelsDF$x10 <- x10
        }
        if(count==0){
          tmpModels <- paste(tmpModels, "x10", sep="") # x10
          tmpModelsDF$x10 <- x10
          tmpModelsDF <- select(tmpModelsDF, x10)
          
        }
      } 
      if(column==11){
        if(count!=0){
          tmpModels <- paste(tmpModels, "x11", sep="+") # x11
          tmpModelsDF$x11 <- x11
        }
        if(count==0){
          tmpModels <- paste(tmpModels, "x11", sep="") # x11
          tmpModelsDF$x11 <- x11
          tmpModelsDF <- select(tmpModelsDF, x11)
          
        }
      } 
      if(column==12){
        if(count!=0){
          tmpModels <- paste(tmpModels, "x12", sep="+") # x12
          tmpModelsDF$x12 <- x12
        }
        if(count==0){
          tmpModels <- paste(tmpModels, "x12", sep="") # x12
          tmpModelsDF$x12 <- x12
          tmpModelsDF <- select(tmpModelsDF, x12)
        }
        x12T = TRUE
      } 
      count=count+1
      tmpPs <-  paste(as.character(count + 1))
      
    } # end of if
    
    
    if(column%%numOfvariables==0){ # if it is the last column in the row
      
      if(row==1){
        allModels <- paste(allModels, tmpModels,sep="")
        pValues <- paste(pValues, tmpPs, sep = "")
      }
      if(row!=1){
        allModels <- paste(allModels, tmpModels,sep=",")
        pValues <- paste(pValues, tmpPs, sep = ",")
      }
    }
  } # end of nested for loop
  
  # for(predVar in 1:tmpModelsDF){ # for each predictor variable column in the model
  #   
  #   
  # } # end of for loop
  reg=lm(as.formula(paste("y~", paste(tmpModelsDF,collapse="+"))))
  
  adjR2vals <- paste(adjR2vals,round(summary(reg)$adj.r.squared, digits = 6),sep = ",")
  SSEvals <- paste(SSEvals, round(anova(reg)$"Sum Sq"[length(anova(reg)$"Sum Sq")],digits = 6),sep=",")
  AICvals <- paste(AICvals, round(AIC(reg),digits = 6),sep = ",")
  pr=resid(reg)/(1-lm.influence(reg)$hat)
  PRESS = sum(pr^2)
  PRESSvals <- paste(PRESSvals, round(PRESS, digits = 6),sep = ",")
  
  
} # end outer for loop
#################################

models <- as.list(strsplit(allModels, ",")[[1]])

pValues <- as.list(strsplit(pValues, ",")[[1]])

# NOTE: I didn't create temporary variables for adjR2val, SSEvals, AICvals,
# and PRESSvals in the loop above. That is why I have to remove the first
# row as R considers it "" aka an empty space. (I'm too lazy to modify the code.)
adjR2vals <- as.list(strsplit(adjR2vals, ",")[[1]])
adjR2vals <- adjR2vals[-c(1)] # remove the empty first row from adjR2vals

SSEvals <- as.list(strsplit(SSEvals, ",")[[1]])
SSEvals <- SSEvals[-c(1)]

AICvals <- as.list(strsplit(AICvals, ",")[[1]])
AICvals <- AICvals[-c(1)]

PRESSvals <- as.list(strsplit(PRESSvals, ",")[[1]])
PRESSvals <- PRESSvals[-c(1)]

#leaps(x=CH06PR09[,2:4], y=CH06PR09[,1], method="Cp") # see Cp values below
#print(paste(format("model", width = 24), format("| p", width = 5), format("| Cp", width = 16), format("| AdjR^2", width = 17), format("| SSE", width = 16), format("| AIC", width = 16),format("| PRESS", width = 16) ))
for(i in 1:length(cpObj$Cp)) {
  print(paste("model= ", format(models[i], width = 24), "|p = ", format(pValues[i],width = 5), "|Cp = ", format(round(cpObj$Cp[i],digits = 6),
                                                                                                                width = 12), "| AdjR^2 = ", format(adjR2vals[i],width =12 ), "|SSE = ", format(SSEvals[i],width = 10), "|AIC = ", 
              format(AICvals[i],width = 10), "|PRESS = ", format(PRESSvals[i],width=10)))
  # NOTE: the value of width for "model" must be increased for larger numbers of
  # predictor variables. 
}