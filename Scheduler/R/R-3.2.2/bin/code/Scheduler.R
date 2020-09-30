start = proc.time()
setwd("C:/Users/Erik/Documents/R Studio/OR Project Data")
# By Erik Nyquist and Kyle Stengel 2015
# Operations Research Term Project
# Student Ambassadors
# 
# This program was designed for the Concordia College Student Ambassador program.
# 
# It incorporates client data of student availability, work preference, and student attributes
# such as major and interests.
# 
# Taking in all of the data, the program distributes the students over the work week in such a way as to maximize the minimum
# number of attributes represented in a time bracket (ie - Monday Morning).
# 
# This way, a "snapshot" of Concordia is present at any given time and available to give tours to prospective students.
# 
# What follows is a TABLE OF CONTENTS:
# 
#   lines 039 - 050 ------------- Easywriter.R
#   lines 051 - 113 ------------- Pull attribute data from the client
#   lines 114 - 151 ------------- Pull availability data from the client
#   lines 152 - 188 ------------- Creation of the Shift Groups
#   lines 189 - 215 ------------- Objective function from people's preferences
#   lines 216 - 247 ------------- CONSTRAINT 1: Attributes must be at least 0
#   lines 248 - 279 ------------- CONSTRAINT 2: Attributes must be no more than 1
#   lines 280 - 305 ------------- CONSTRAINT 3: Each person must work at least 1 shift
#   lines 306 - 331 ------------- CONSTRAINT 4: Each person must work no more than 6 shifts
#   lines 332 - 369 ------------- CONSTRAINT 5: Attributes over Shift Groups must be at least 0
#   lines 370 - 407 ------------- CONSTRAINT 6: Attributes over Shift Groups must be no more than 1
#   lines 408 - 439 ------------- CONSTRAINT 7: Z is at least the sum of all Attributes over all shifts
#   lines 440 - 469 ------------- CONSTRAINT 8: Filling High Demand Shifts
#   lines 470 - 496 ------------- CONSTRAINT 9: Placing High Demand Students in High Demand Shifts
#   lines 497 - 533 ------------- CONSTRAINT 10: Students cannot work shifts that coincide
#   lines 534 - 559 ------------- CONSTRAINT 11: Students get no more than the number of shifts designated
#   lines 560 - 589 ------------- CONSTRAINT 12: Filling the Lunch Time Slots
#   lines 590 - 598 ------------- Build the master Constriant Matrix
#   lines 599 - 624 ------------- Create the LP
#   lines 625 - END ------------- Interpret the Results


##############
# EasyWriter.R
##############

easyWriter = function(vectorToAddCoefficientsTo,fullVariableNameList,regularExpressionToSearchFor,coefficientToGoWithNamesYouWant){
  #regular expressions provide an incredibly robust way of searching through text files.  Seriously, google them.
  indicesToModify = grep(regularExpressionToSearchFor,fullVariableNameList, perl=TRUE, value=FALSE) #grep is the function that executes the search for things that match the regular expression.
  vectorToAddCoefficientsTo[indicesToModify] = coefficientToGoWithNamesYouWant #over-writes the locations that match the regular expression.
  return(vectorToAddCoefficientsTo)
}


###################################
#Pull attribute data from the User
###################################

rawData = read.csv("Attributes.csv",header=TRUE,stringsAsFactors=FALSE,row.names = 1)
Attributes = rawData

HighDemand = Attributes[,ncol(Attributes)]# Grab the last column of Attributes
HighDemand = rownames(Attributes)[HighDemand != ""]# Eliminate any empty strings

Attributes = Attributes[,-ncol(Attributes)]# Remove the last column of Attributes

# Undecided = Attributes[,ncol(Attributes)]# Grab the last column of Attributes
# Undecided = rownames(Attributes)[Undecided != ""]# Eliminate any empty strings
# 
# Attributes = Attributes[,-ncol(Attributes)]# Remove the last column of Attributes

Education = Attributes[,ncol(Attributes)]# Grab the last column of Attributes
Education = rownames(Attributes)[Education != ""]# Eliminate any empty strings

Attributes = Attributes[,-ncol(Attributes)]# Remove the last column of Attributes

EnglishORComm = Attributes[,ncol(Attributes)]# Grab the last column of Attributes
EnglishORComm = rownames(Attributes)[EnglishORComm != ""]# Eliminate any empty strings

Attributes = Attributes[,-ncol(Attributes)]# Remove the last column of Attributes

Sports = Attributes[,ncol(Attributes)]# Grab the last column of Attributes
Sports = rownames(Attributes)[Sports != ""]# Eliminate any empty strings

Attributes = Attributes[,-ncol(Attributes)]# Remove the last column of Attributes

Health = Attributes[,ncol(Attributes)]# Grab the last column of Attributes
Health = rownames(Attributes)[Health != ""]# Eliminate any empty strings

Attributes = Attributes[,-ncol(Attributes)]

Humanities = Attributes[,ncol(Attributes)]# Grab the last column of Attributes
Humanities = rownames(Attributes)[Humanities != ""]# Eliminate any empty strings

Attributes = Attributes[,-ncol(Attributes)]# Remove the last column of Attributes

Music = Attributes[,ncol(Attributes)]# Grab the new last column of Attributes
Music = rownames(Attributes)[Music != ""]# Eliminate any empty strings

Attributes = Attributes[,-ncol(Attributes)]# Remove the last column of Attributes

Art = Attributes[,ncol(Attributes)]# Grab the new last column of Attributes
Art = rownames(Attributes)[Art != ""]# Eliminate any empty strings

Attributes = Attributes[,-ncol(Attributes)]# Remove the last column of Attributes

Science = Attributes[,ncol(Attributes)]# Grab the new last column of Attributes
Science = rownames(Attributes)[Science != ""]# Eliminate any empty strings

Attributes = Attributes[,-ncol(Attributes)]# Remove the last column of Attributes

Business = Attributes[,ncol(Attributes)]# Grab the new last column of Attributes
Business = rownames(Attributes)[Business != ""]# Eliminate any empty strings

attributeSets = list(Business, Science, Art, Music, Humanities, Health,Sports, EnglishORComm, Education, HighDemand)# Create the AttributeSet


#####################################################
# Pull availability and preference data from the User
#####################################################

rawData = read.csv("Availability.csv",header=TRUE,row.names=1,stringsAsFactors=FALSE)# header=TRUE grabs column labels, row.names=1 uses the first column for (surprise!) row names, and stringAsFactors=FALSE prevents R from thinking a 1 or 13 is a string that says "1" or "13" instead of the actual numbers 1 and 13.  Oh, the shenanigans...
availabilityAndPreferenceMatrix = rawData[,-ncol(rawData)]#removes last column
numberOfShiftsWanted = rawData[,ncol(rawData)]#grabs last column
names(numberOfShiftsWanted) = rownames(availabilityAndPreferenceMatrix) #lists don't have rownames or colnames... theyjust have names.

listOfPriorityLevels = c("1","2","3","4","5")
symbolsForForbiddenTimeSlot = c("x","X") #I put both capital and lower case x's, to help with typos on the data entry end.
mediumM = 0.5 #available but not ranked
bigM = -10000 #Students are not available at these times

listOfPersons = rownames(availabilityAndPreferenceMatrix)
listOfTimeSlots = colnames(availabilityAndPreferenceMatrix)[1:length(availabilityAndPreferenceMatrix)]

listOfTimeSlotsNew = list(Monday = c("Monday.1","Monday.2","Monday.3","Monday.4","Monday.5","Monday.6","Monday.7","Monday.8","Monday.9","Monday.10","Monday.11","Monday.12","Monday.13","Monday.14"),
                          Tuesday = c("Tuesday.1","Tuesday.2","Tuesday.3","Tuesday.4","Tuesday.5","Tuesday.6","Tuesday.7","Tuesday.8","Tuesday.9","Tuesday.10","Tuesday.11","Tuesday.12","Tuesday.13","Tuesday.14"),
                          Wednesday = c("Wednesday.1","Wednesday.2","Wednesday.3","Wednesday.4","Wednesday.5","Wednesday.6","Wednesday.7","Wednesday.8","Wednesday.9","Wednesday.10","Wednesday.11","Wednesday.12","Wednesday.13","Wednesday.14"),
                          Thursday = c("Thursday.1","Thursday.2","Thursday.3","Thursday.4","Thursday.5","Thursday.6","Thursday.7","Thursday.8","Thursday.9","Thursday.10","Thursday.11","Thursday.12","Thursday.13","Thursday.14"),
                          Friday = c("Friday.1","Friday.2","Friday.3","Friday.4","Friday.5","Friday.6","Friday.7","Friday.8","Friday.9","Friday.10","Friday.11","Friday.12","Friday.13","Friday.14"))

listOfDays = c("Monday.1","Monday.2","Monday.3","Monday.4","Monday.5","Monday.6","Monday.7","Monday.8","Monday.9","Monday.10","Monday.11","Monday.12","Monday.13","Monday.14",
               "Tuesday.1","Tuesday.2","Tuesday.3","Tuesday.4","Tuesday.5","Tuesday.6","Tuesday.7","Tuesday.8","Tuesday.9","Tuesday.10","Tuesday.11","Tuesday.12","Tuesday.13","Tuesday.14",
               "Wednesday.1","Wednesday.2","Wednesday.3","Wednesday.4","Wednesday.5","Wednesday.6","Wednesday.7","Wednesday.8","Wednesday.9","Wednesday.10","Wednesday.11","Wednesday.12","Wednesday.13","Wednesday.14",
               "Thursday.1","Thursday.2","Thursday.3","Thursday.4","Thursday.5","Thursday.6","Thursday.7","Thursday.8","Thursday.9","Thursday.10","Thursday.11","Thursday.12","Thursday.13","Thursday.14",
               "Friday.1","Friday.2","Friday.3","Friday.4","Friday.5","Friday.6","Friday.7","Friday.8","Friday.9","Friday.10","Friday.11","Friday.12","Friday.13","Friday.14")

namesOfVariables = c() #initialize with an empty list
for(person in listOfPersons){ #just keep hitting enter after these open curly brackets...it'll advance to the next line and wait to execute until all brackets are closed off. 
 for(time in listOfTimeSlots){      
   namesOfVariables = c(namesOfVariables,paste0(as.character(person),".",as.character(time)))    
 }
}

ExtraNamesOfVariables = c()
for(pp in 1:length(attributeSets)){
 for(yy in listOfTimeSlots){
   ExtraNamesOfVariables = c(ExtraNamesOfVariables,paste0("Attribute",as.character(pp),".",as.character(yy)))
 }
}

MasterListOfShifts = c()
for(aa in listOfDays){
    MasterListOfShifts = c(MasterListOfShifts, paste0(as.character(aa)))
}

###########################################################################
# Generating the larger time slots (i.e. morning, LunchTime, afternoon)
###########################################################################

listOfDays = c("Monday","Tuesday","Wednesday","Thursday","Friday")
listOfTimesOfDay = c("Morning","LunchTime","Afternoon")

listOfMorningSlots = list()
listOfMorningSlots = list(Monday = c("Monday.1","Monday.2","Monday.3","Monday.4","Monday.5"), Tuesday = c("Tuesday.1","Tuesday.2","Tuesday.3","Tuesday.4","Tuesday.5"), Wednesday = c("Wednesday.1","Wednesday.2","Wednesday.3","Wednesday.4","Wednesday.5"), Thursday = c("Thursday.1","Thursday.2","Thursday.3","Thursday.4","Thursday.5"), Friday = c("Friday.1","Friday.2","Friday.3","Friday.4","Friday.5"))

listOfLunchTimeSlots = list()
listOfLunchTimeSlots = list(Monday = c("Monday.6","Monday.7","Monday.8"), Tuesday = c("Tuesday.6","Tuesday.7","Tuesday.8"), Wednesday = c("Wednesday.6","Wednesday.7","Wednesday.8"), Thursday = c("Thursday.6","Thursday.7","Thursday.8"), Friday = c("Friday.6","Friday.7","Friday.8"))

listOfAfternoonSlots = list()
listOfAfternoonSlots = list(Monday = c("Monday.9","Monday.10","Monday.11","Monday.12","Monday.13","Monday.14"), Tuesday = c("Tuesday.9","Tuesday.10","Tuesday.11","Tuesday.12","Tuesday.13","Tuesday.14"), Wednesday = c("Wednesday.9","Wednesday.10","Wednesday.11","Wednesday.12","Wednesday.13","Wednesday.14"), Thursday = c("Thursday.9","Thursday.10","Thursday.11","Thursday.12","Thursday.13","Thursday.14"), Friday = c("Friday.9","Friday.10","Friday.11","Friday.12","Friday.13","Friday.14"))

listOfLargerTimeSlots = list(Mornings = listOfMorningSlots, LunchTimes = listOfLunchTimeSlots, Afternoons = listOfAfternoonSlots)

#Axvig: get a better name than listOfLargerTimeSlots1
listOfLargerTimeSlots1 = list(Mornings = list("MondayMorning","TuesdayMorning","WednesdayMorning","ThursdayMorning","FridayMorning"), LunchTimes = list("MondayLunchTime","TuesdayLunchTime","WednesdayLunchTime","ThursdayLunchTime","FridayLunchTime"), Afternoons = list("MondayAfternoon","TuesdayAfternoon","WednesdayAfternoon","ThursdayAfternoon","FridayAfternoon"))

MoreNamesOfVariables = c()
for(pp in 1:length(attributeSets)){
  for(ff in listOfDays){
    for(dd in listOfTimesOfDay){
      MoreNamesOfVariables = c(MoreNamesOfVariables,paste0("Attribute",as.character(pp),".",as.character(ff),as.character(dd)))#Axvig: have you considered putting a dot between ff and dd?  You'd have to update all your regular expressions, so don't do it unless there's a good reason.
    }
  }
}

z = c("Z")
MoreNamesOfVariables1 = c(MoreNamesOfVariables,z)

namesOfVariables = c(namesOfVariables, ExtraNamesOfVariables, MoreNamesOfVariables1)


##################################################################################
#Now let's get the objective function set up - we'll include people's preferences.
##################################################################################

objectiveFunction = matrix(0,1,length(namesOfVariables)) 
colnames(objectiveFunction) = namesOfVariables           #initialization

for(person in listOfPersons){
  for(time in listOfTimeSlots){
    regularExpressionToSearchFor = paste0(as.character(person),".",as.character(time))
    valueInTable = availabilityAndPreferenceMatrix[person,time]
    if(is.element(valueInTable,listOfPriorityLevels)){#if they can work and have it ranked, assign the objective value that ranking
      objectiveFunction = easyWriter(objectiveFunction,colnames(objectiveFunction),regularExpressionToSearchFor,5/as.numeric(valueInTable))#Axvig: what a stylish flip of the preference into something to maximize!  
    }
    if(!is.element(valueInTable,symbolsForForbiddenTimeSlot) & !is.element(valueInTable,listOfPriorityLevels)){#if they didn't mark that they can work, we could schedule them in that slot but it doesn't get them any "points"
      objectiveFunction = easyWriter(objectiveFunction,colnames(objectiveFunction),regularExpressionToSearchFor,bigM)
    }
    if(is.element(valueInTable,symbolsForForbiddenTimeSlot)){#If they can work, but it's not one of their top preferences, we can schedule them there, but it doesn't get them as many "points"
      objectiveFunction = easyWriter(objectiveFunction,colnames(objectiveFunction),regularExpressionToSearchFor,mediumM)
    }
  }
}

weightOfZRelativeToPreference = 100
objectiveFunction = easyWriter(objectiveFunction,colnames(objectiveFunction),"^Z$",weightOfZRelativeToPreference)


##################################
#          FIRST CONSTRAINT
# attributes must be larger than 0
##################################

#preallocate space for speed...
newCounter = 1
numberOfNewRows = length(attributeSets)*length(listOfTimeSlots)
newBlankRows = matrix(0,numberOfNewRows,length(namesOfVariables))
colnames(newBlankRows) = namesOfVariables

FirstConstraintMatrix = newBlankRows
FirstInequalities = matrix("",0,1)
FirstRightHandSide = matrix(0,0,1)

for (ll in 1:length(attributeSets)){
  for(dd in listOfTimeSlots){
    FirstNewConstraint = matrix(0,1,length(namesOfVariables)) #initialize
    colnames(FirstNewConstraint) = namesOfVariables #name locations for easy finding of stuff
    FirstNewInequality = matrix(">=",1,1)
    FirstNewRightHandSide = matrix(0,1,1)
    #now add the new stuff onto the ends of the overall vectors:
    FirstInequalities = rbind(FirstInequalities,FirstNewInequality)
    FirstRightHandSide = rbind(FirstRightHandSide, FirstNewRightHandSide)
    for(bb in attributeSets[[ll]]){
      FirstRegularExpressionToSearchFor = paste0(bb,"\\.",dd,"$") # if we don't include the x\\. at the beginning, we pick up all places where locationI is mentioned, whether first or second!  We just want first. 
      FirstNewConstraint = easyWriter(FirstNewConstraint,namesOfVariables,FirstRegularExpressionToSearchFor,-0.001)
      FirstRegularExpressionToSearchFor2 = paste0("Attribute",as.character(ll),"\\.",dd,"$")
      FirstNewConstraint = easyWriter(FirstNewConstraint,namesOfVariables,FirstRegularExpressionToSearchFor2,1)
    }
    FirstConstraintMatrix[newCounter,] = FirstNewConstraint
    
    newCounter = newCounter + 1
  }
}

#CheckFirstConstraint = cbind(FirstConstraintMatrix,FirstInequalities,FirstRightHandSide)


#####################################
#           SECOND CONSTRAINT
# attributes must be no larger than 1
#####################################

#preallocate space for speed...
newCounter = 1
numberOfNewRows = length(attributeSets)*length(listOfTimeSlots)
newBlankRows = matrix(0,numberOfNewRows,length(namesOfVariables))
colnames(newBlankRows) = namesOfVariables

SecondConstraintMatrix = newBlankRows
SecondInequalities = matrix("",0,1)
SecondRightHandSide = matrix(0,0,1)

for (ll in 1:length(attributeSets)){
  for(dd in listOfTimeSlots){
    SecondNewConstraint = matrix(0,1,length(namesOfVariables)) #initialize
    colnames(SecondNewConstraint) = namesOfVariables #name locations for easy finding of stuff
    SecondNewInequality = matrix("<=",1,1)
    SecondNewRightHandSide = matrix(0,1,1)
    #now add the new stuff onto the ends of the overall vectors:
    SecondInequalities = rbind(SecondInequalities,SecondNewInequality)
    SecondRightHandSide = rbind(SecondRightHandSide, SecondNewRightHandSide)
    for(bb in attributeSets[[ll]]){
      SecondRegularExpressionToSearchFor = paste0(bb,"\\.",dd,"$") # if we don't include the x\\. at the beginning, we pick up all places where locationI is mentioned, whether first or second!  We just want first. 
      SecondNewConstraint = easyWriter(SecondNewConstraint,namesOfVariables,SecondRegularExpressionToSearchFor,-1)
      FirstRegularExpressionToSearchFor2 = paste0("Attribute",as.character(ll),"\\.",dd,"$")
      SecondNewConstraint = easyWriter(SecondNewConstraint,namesOfVariables,FirstRegularExpressionToSearchFor2,1)
    }
    SecondConstraintMatrix[newCounter,] = SecondNewConstraint
    
    newCounter = newCounter + 1
  }
}

#CheckSecondConstraint = cbind(SecondConstraintMatrix,SecondInequalities,SecondRightHandSide)


###########################################
#             THIRD CONSTRAINT
# Each person must work at least 1 shift
###########################################

#preallocate space for speed...
newCounter = 1
numberOfNewRows = length(listOfPersons)
newBlankRows = matrix(0,numberOfNewRows,length(namesOfVariables))
colnames(newBlankRows) = namesOfVariables

ThirdConstraintMatrix = newBlankRows
ThirdInequalities = matrix("",0,1)
ThirdRightHandSide = matrix(0,0,1)

for(tt in listOfPersons){
  ThirdNewConstraint = matrix(0,1,length(namesOfVariables)) #initialize
  colnames(ThirdNewConstraint) = namesOfVariables #name locations for easy finding of stuff
  ThirdNewInequality = matrix(">=",1,1)
  ThirdNewRightHandSide = matrix(1,1,1)
  #now add the new stuff onto the ends of the overall vectors:
  ThirdInequalities = rbind(ThirdInequalities,ThirdNewInequality)
  ThirdRightHandSide = rbind(ThirdRightHandSide, ThirdNewRightHandSide)
  ThirdRegularExpressionToSearchFor = paste0(as.character(tt),"\\.") # if we don't include the x\\. at the beginning, we pick up all places where locationI is mentioned, whether Third or second!  We just want Third. 
  ThirdNewConstraint = easyWriter(ThirdNewConstraint,namesOfVariables,ThirdRegularExpressionToSearchFor,1)
  ThirdConstraintMatrix[newCounter,] = ThirdNewConstraint
  
  newCounter = newCounter + 1
}

#CheckThirdConstraint = cbind(ThirdConstraintMatrix,ThirdInequalities,ThirdRightHandSide)


#############################################
#               FOURTH CONSTRAINT
# Each person must work no more than 6 shifts
#############################################

#preallocate space for speed...
newCounter = 1
numberOfNewRows = length(listOfPersons)
newBlankRows = matrix(0,numberOfNewRows,length(namesOfVariables))
colnames(newBlankRows) = namesOfVariables

FourthConstraintMatrix = newBlankRows
FourthInequalities = matrix("",0,1)
FourthRightHandSide = matrix(0,0,1)

for(tt in listOfPersons){
  FourthNewConstraint = matrix(0,1,length(namesOfVariables)) #initialize
  colnames(FourthNewConstraint) = namesOfVariables #name locations for easy finding of stuff
  FourthNewInequality = matrix("<=",1,1)
  FourthNewRightHandSide = matrix(6,1,1)
  #now add the new stuff onto the ends of the overall vectors:
  FourthInequalities = rbind(FourthInequalities,FourthNewInequality)
  FourthRightHandSide = rbind(FourthRightHandSide, FourthNewRightHandSide)
  FourthRegularExpressionToSearchFor = paste0(as.character(tt),"\\.") # if we don't include the x\\. at the beginning, we pick up all places where locationI is mentioned, whether Third or second!  We just want Third. 
  FourthNewConstraint = easyWriter(FourthNewConstraint,namesOfVariables,FourthRegularExpressionToSearchFor,1)
  FourthConstraintMatrix[newCounter,] = FourthNewConstraint
  newCounter = newCounter + 1
}

#CheckFourthConstraint = cbind(FourthConstraintMatrix,FourthInequalities,FourthRightHandSide)


######################################
#           FIFTH CONSTRAINT
# Attributes in Larger Time slots >= 0
######################################

#preallocate space for speed...
newCounter = 1
numberOfNewRows = length(attributeSets)*5*length(listOfLargerTimeSlots)# We multiply by three because that is how many larger slots there are
newBlankRows = matrix(0,numberOfNewRows,length(namesOfVariables))
colnames(newBlankRows) = namesOfVariables

FifthConstraintMatrix = newBlankRows
FifthInequalities = matrix("",0,1)
FifthRightHandSide = matrix(0,0,1)

for(rr in 1:length(attributeSets)){
  for(uu in 1:length(listOfLargerTimeSlots)){
    for(ww in 1:length(listOfLargerTimeSlots[[uu]])){
      FifthNewConstraint = matrix(0,1,length(namesOfVariables)) #initialize
      colnames(FifthNewConstraint) = namesOfVariables #name locations for easy finding of stuff
      FifthNewInequality = matrix(">=",1,1)
      FifthNewRightHandSide = matrix(0,1,1)
      #now add the new stuff onto the ends of the overall vectors:
      FifthInequalities = rbind(FifthInequalities,FifthNewInequality)
      FifthRightHandSide = rbind(FifthRightHandSide, FifthNewRightHandSide)
      for(kk in listOfLargerTimeSlots[[uu]][[ww]]){     #this will cycle over each day, each larger time slot (similar to our first/second constraints)
        #Look at listOfLargerTimeSlots, listOfLargerTimeSlots[[1]], listOfLargerTimeSlots[[1]][[1]]
        #to get a better idea of what listOfLargerTimeSlots is...(hint: it's a list of a list of lists...listception...)
        FifthRegularExpressionToSearchFor1 = paste0("^Attribute",as.character(rr),"\\.",as.character(kk),"$")  
        FifthNewConstraint = easyWriter(FifthNewConstraint,namesOfVariables,FifthRegularExpressionToSearchFor1,-0.001)
      }
      for(jj in listOfLargerTimeSlots1[[uu]][[ww]]){
        regexToSearchFor = paste0("^Attribute",as.character(rr),"\\.",as.character(jj),"$")
        FifthNewConstraint = easyWriter(FifthNewConstraint,namesOfVariables,regexToSearchFor,1)
      }
      FifthConstraintMatrix[newCounter,] = FifthNewConstraint
      
      newCounter = newCounter + 1
    }
  }
}

#CheckFifthConstraint = cbind(FifthConstraintMatrix,FifthInequalities,FifthRightHandSide)


########################################################
#                 SIXTH CONSTRAINT
# Attributes in larger time slots can be no bigger than 1
#########################################################

#preallocate space for speed...
newCounter = 1
numberOfNewRows = length(attributeSets)*5*length(listOfLargerTimeSlots)# We multiply by three because that is how many larger slots there are
newBlankRows = matrix(0,numberOfNewRows,length(namesOfVariables))
colnames(newBlankRows) = namesOfVariables

SixthConstraintMatrix = newBlankRows
SixthInequalities = matrix("",0,1)
SixthRightHandSide = matrix(0,0,1)

for(rr in 1:length(attributeSets)){
  for(uu in 1:length(listOfLargerTimeSlots)){
    for(ww in 1:length(listOfLargerTimeSlots[[uu]])){
      SixthNewConstraint = matrix(0,1,length(namesOfVariables)) #initialize
      colnames(SixthNewConstraint) = namesOfVariables #name locations for easy finding of stuff
      SixthNewInequality = matrix("<=",1,1)
      SixthNewRightHandSide = matrix(0,1,1)
      #now add the new stuff onto the ends of the overall vectors:
      SixthInequalities = rbind(SixthInequalities,SixthNewInequality)
      SixthRightHandSide = rbind(SixthRightHandSide, SixthNewRightHandSide)
      for(kk in listOfLargerTimeSlots[[uu]][[ww]]){     #this will cycle over each day, each larger time slot (similar to our first/second constraints)
        #Look at listOfLargerTimeSlots, listOfLargerTimeSlots[[1]], listOfLargerTimeSlots[[1]][[1]]
        #to get a better idea of what listOfLargerTimeSlots is...(hint: it's a list of a list of lists...listception...)
        SixthRegularExpressionToSearchFor1 = paste0("^Attribute",as.character(rr),"\\.",as.character(kk),"$")  
        SixthNewConstraint = easyWriter(SixthNewConstraint,namesOfVariables,SixthRegularExpressionToSearchFor1,-1)
      }
      for(jj in listOfLargerTimeSlots1[[uu]][[ww]]){
        regexToSearchFor = paste0("^Attribute",as.character(rr),"\\.",as.character(jj),"$")
        SixthNewConstraint = easyWriter(SixthNewConstraint,namesOfVariables,regexToSearchFor,1)
      }
      SixthConstraintMatrix[newCounter,] = SixthNewConstraint
      
      newCounter = newCounter + 1
    }
  }
}

#CheckSixthConstraint = cbind(SixthConstraintMatrix,SixthInequalities,SixthRightHandSide)


###############################################
#                 SEVENTH CONSTRAINT
# Z <= sum(all attributes in larger time slots)
###############################################

#preallocate space for speed...
newCounter = 1
numberOfNewRows = 5*length(listOfLargerTimeSlots1)# We multiply by five because that is how many larger slots1 there are
newBlankRows = matrix(0,numberOfNewRows,length(namesOfVariables))
colnames(newBlankRows) = namesOfVariables

SeventhConstraintMatrix = newBlankRows
SeventhInequalities = matrix("",0,1)
SeventhRightHandSide = matrix(0,0,1)

for(uu in 1:length(listOfLargerTimeSlots1)){
  for(ww in 1:length(listOfLargerTimeSlots1[[uu]])){
    SeventhNewConstraint = matrix(0,1,length(namesOfVariables)) #initialize
    colnames(SeventhNewConstraint) = namesOfVariables #name locations for easy finding of stuff
    SeventhNewInequality = matrix("<=",1,1)
    SeventhNewRightHandSide = matrix(0,1,1)
    #now add the new stuff onto the ends of the overall vectors:
    SeventhInequalities = rbind(SeventhInequalities,SeventhNewInequality)
    SeventhRightHandSide = rbind(SeventhRightHandSide, SeventhNewRightHandSide)
    for(jj in listOfLargerTimeSlots1[[uu]][[ww]]){ 
      SeventhRegularExpressionToSearchFor1= paste0("^Attribute",".*\\",".",as.character(jj),"$")
      SeventhNewConstraint = easyWriter(SeventhNewConstraint,namesOfVariables,SeventhRegularExpressionToSearchFor1,-1)
      regexToSearchFor = "Z" #Axvig: you could put this line and the next ouside of the jj loop.  See why?
      SeventhNewConstraint = easyWriter(SeventhNewConstraint,namesOfVariables,regexToSearchFor,1)
    }
    SeventhConstraintMatrix[newCounter,] = SeventhNewConstraint
      
    newCounter = newCounter + 1
  }
}

#CheckSeventhConstraint = cbind(SeventhConstraintMatrix,SeventhInequalities,SeventhRightHandSide)


###########################################################
#                 EIGHTH CONSTRAINT
# High importance time slots have at least 3 people working
###########################################################


HighImportanceTimeSlots = c("Monday.1","Wednesday.1","Friday.1")#Axvig:  wouldn't Monday.2, Wednesday.2, and Friday.2, Monday.3, Wedensday.3, and Friday.3 also work?  They'd still be staffing higher on Monday/Friday mornings, and it might give you more flexibility in the model.  Just a thought...

#preallocate space for speed...
newCounter = 1
numberOfNewRows = length(HighImportanceTimeSlots)
newBlankRows = matrix(0,numberOfNewRows,length(namesOfVariables))
colnames(newBlankRows) = namesOfVariables

EighthConstraintMatrix = newBlankRows
EighthInequalities = matrix("",0,1)
EighthRightHandSide = matrix(0,0,1)

for(hh in HighImportanceTimeSlots){
  EighthNewConstraint = matrix(0,1,length(namesOfVariables)) #initialize
  colnames(EighthNewConstraint) = namesOfVariables #name locations for easy finding of stuff
  EighthNewInequality = matrix(">=",1,1)
  EighthNewRightHandSide = matrix(3,1,1)
  #now add the new stuff onto the ends of the overall vectors:
  EighthInequalities = rbind(EighthInequalities,EighthNewInequality)
  EighthRightHandSide = rbind(EighthRightHandSide, EighthNewRightHandSide)
  EighthRegularExpressionToSearchFor1= paste0(as.character(hh),"$")
  EighthNewConstraint = easyWriter(EighthNewConstraint,namesOfVariables,EighthRegularExpressionToSearchFor1,1)
  regexToSearchFor = paste0("^Attribute",".*","\\.",as.character(hh),"$")
  EighthNewConstraint = easyWriter(EighthNewConstraint,namesOfVariables,regexToSearchFor,0)
  EighthConstraintMatrix[newCounter,] = EighthNewConstraint
  
  newCounter = newCounter + 1
}

#CheckEighthConstraint = cbind(EighthConstraintMatrix,EighthInequalities,EighthRightHandSide)


##################################################################################
#                               NINTH CONSTRAINT
# We want at least one of our High Demand People in our High Importance Time Slots
##################################################################################

#preallocate space for speed...
newCounter = 1
numberOfNewRows = length(HighImportanceTimeSlots)
newBlankRows = matrix(0,numberOfNewRows,length(namesOfVariables))
colnames(newBlankRows) = namesOfVariables

NinthConstraintMatrix = newBlankRows
NinthInequalities = matrix("",0,1)
NinthRightHandSide = matrix(0,0,1)

for(bb in HighImportanceTimeSlots){
  NinthNewConstraint = matrix(0,1,length(namesOfVariables)) #initialize
  colnames(NinthNewConstraint) = namesOfVariables #name locations for easy finding of stuff
  NinthNewInequality = matrix(">=",1,1)
  NinthNewRightHandSide = matrix(1,1,1)
  #now add the new stuff onto the ends of the overall vectors:
  NinthInequalities = rbind(NinthInequalities,NinthNewInequality)
  NinthRightHandSide = rbind(NinthRightHandSide, NinthNewRightHandSide)
  for(aa in HighDemand){
    NinthRegularExpressionToSearchFor1 = paste0(as.character(aa),"\\.",as.character(bb),"$")
    NinthNewConstraint = easyWriter(NinthNewConstraint,namesOfVariables,NinthRegularExpressionToSearchFor1,1)
  }
  NinthConstraintMatrix[newCounter,] = NinthNewConstraint
  
  newCounter = newCounter + 1
}

#CheckNinthConstraint = cbind(NinthConstraintMatrix,NinthInequalities,NinthRightHandSide)


########################################
#           TENTH CONSTRAINT
# People can't work more than once a day
########################################

ListOfTimeSlotsNew = list(Monday = c("Monday.1","Monday.2","Monday.3","Monday.4","Monday.5","Monday.6","Monday.7","Monday.8","Monday.9","Monday.10","Monday.11","Monday.12","Monday.13","Monday.14"),
                          Tuesday = c("Tuesday.1","Tuesday.2","Tuesday.3","Tuesday.4","Tuesday.5","Tuesday.6","Tuesday.7","Tuesday.8","Tuesday.9","Tuesday.10","Tuesday.11","Tuesday.12","Tuesday.13","Tuesday.14"),
                          Wednesday = c("Wednesday.1","Wednesday.2","Wednesday.3","Wednesday.4","Wednesday.5","Wednesday.6","Wednesday.7","Wednesday.8","Wednesday.9","Wednesday.10","Wednesday.11","Wednesday.12","Wednesday.13","Wednesday.14"),
                          Thursday = c("Thursday.1","Thursday.2","Thursday.3","Thursday.4","Thursday.5","Thursday.6","Thursday.7","Thursday.8","Thursday.9","Thursday.10","Thursday.11","Thursday.12","Thursday.13","Thursday.14"),
                          Friday = c("Friday.1","Friday.2","Friday.3","Friday.4","Friday.5","Friday.6","Friday.7","Friday.8","Friday.9","Friday.10","Friday.11","Friday.12","Friday.13","Friday.14"))


#preallocate space for speed...
newCounter = 1
numberOfNewRows = length(listOfPersons)*length(listOfTimeSlotsNew)
newBlankRows = matrix(0,numberOfNewRows,length(namesOfVariables))
colnames(newBlankRows) = namesOfVariables

TenthConstraintMatrix = newBlankRows
TenthInequalities = matrix("",0,1)
TenthRightHandSide = matrix(0,0,1)

for(pp in listOfPersons){
  for(aa in 1:length(listOfTimeSlotsNew)){
  TenthNewConstraint = matrix(0,1,length(namesOfVariables)) #initialize
  colnames(TenthNewConstraint) = namesOfVariables #name locations for easy finding of stuff
  TenthNewInequality = matrix("<=",1,1)
  TenthNewRightHandSide = matrix(1,1,1)
  #now add the new stuff onto the ends of the overall vectors:
  TenthInequalities = rbind(TenthInequalities,TenthNewInequality)
  TenthRightHandSide = rbind(TenthRightHandSide, TenthNewRightHandSide)
  
  for(bb in 1:length(ListOfTimeSlotsNew[[aa]])){
    
    TenthRegularExpressionToSearchFor = paste0(as.character(pp),"\\.",as.character(listOfTimeSlotsNew[[aa]][[bb]]),"$")
    TenthNewConstraint = easyWriter(TenthNewConstraint, namesOfVariables, TenthRegularExpressionToSearchFor,1)
    
    regexToSearchFor = paste0("^Attribute")
    TenthNewConstraint = easyWriter(TenthNewConstraint,namesOfVariables,regexToSearchFor,0)
  }

  TenthConstraintMatrix[newCounter,] = TenthNewConstraint
  
  newCounter = newCounter + 1
  }
}

#CheckTenthConstraint = cbind(TenthConstraintMatrix,TenthInequalities,TenthRightHandSide)


##########################################################
#                     ELEVENTH CONSTRAINT
# Give workers no more than the number of shifts requested
##########################################################

#preallocate space for speed...
newCounter = 1
numberOfNewRows = length(listOfPersons)
newBlankRows = matrix(0,numberOfNewRows,length(namesOfVariables))
colnames(newBlankRows) = namesOfVariables

EleventhConstraintMatrix = newBlankRows
EleventhInequalities = matrix("",0,1)
EleventhRightHandSide = matrix(0,0,1)

for(person in listOfPersons){
  EleventhNewConstraint = matrix(0,1,length(namesOfVariables))
  colnames(EleventhNewConstraint) = namesOfVariables
  regularExpressionToSearchFor = paste0(as.character(person),"\\.") 
  EleventhNewConstraint = easyWriter(EleventhNewConstraint,namesOfVariables,regularExpressionToSearchFor,1)
  EleventhNewInequality = matrix("<=",1,1)
  EleventhNewRightHandSide = matrix(numberOfShiftsWanted[[person]],1,1)#we use double hard brackets to gain access to just the information, not all the labels and such.
  #now add the new stuff onto the ends of the overall vectors:
  EleventhRightHandSide = rbind(EleventhRightHandSide, EleventhNewInequality) #rbind is the row-bind operation, concatenating matrix one on top of the other.
  EleventhInequalities = rbind(EleventhInequalities,EleventhNewInequality)
  EleventhConstraintMatrix[newCounter,] = EleventhNewConstraint
  
  newCounter = newCounter + 1
}

#CheckEleventhConstraint = cbind(EleventhConstraintMatrix,EleventhInequalities,EleventhRightHandSide)


#################################################################################
#                                    TWELFTH CONSTRAINT
# Make sure at least 1 person is working over the lunch hour (noon-1pm) each day.
#################################################################################

#preallocate space for speed...
newCounter = 1
numberOfNewRows = length(listOfLunchTimeSlots)
newBlankRows = matrix(0,numberOfNewRows,length(namesOfVariables))
colnames(newBlankRows) = namesOfVariables

TwelfthConstraintMatrix = newBlankRows
TwelfthInequalities = matrix("",0,1)
TwelfthRightHandSide = matrix(0,0,1)

for(hh in 1:length(listOfLunchTimeSlots)){
  TwelfthNewConstraint = matrix(0,1,length(namesOfVariables)) #initialize
  colnames(TwelfthNewConstraint) = namesOfVariables #name locations for easy finding of stuff
  TwelfthNewInequality = matrix(">=",1,1)
  TwelfthNewRightHandSide = matrix(1,1,1)
  #now add the new stuff onto the ends of the overall vectors:
  TwelfthInequalities = rbind(TwelfthInequalities,TwelfthNewInequality)
  TwelfthRightHandSide = rbind(TwelfthRightHandSide, TwelfthNewRightHandSide)
  for(yy in listOfLunchTimeSlots[[hh]]){
  TwelfthRegularExpressionToSearchFor1= paste0(as.character(yy),"$")
  TwelfthNewConstraint = easyWriter(TwelfthNewConstraint,namesOfVariables,TwelfthRegularExpressionToSearchFor1,1)
  regexToSearchFor = paste0("^Attribute",".*","\\.",as.character(yy),"$")
  TwelfthNewConstraint = easyWriter(TwelfthNewConstraint,namesOfVariables,regexToSearchFor,0)
  }
  TwelfthConstraintMatrix[newCounter,] = TwelfthNewConstraint
  
  newCounter = newCounter + 1
}

#CheckTwelfthConstraint = cbind(TwelfthConstraintMatrix,TwelfthInequalities,TwelfthRightHandSide)


################################################
#            THIRTEENTH CONSTRAINT
# Each shift has a max cap of students per shift
################################################

listOfDays = c("Monday.1","Monday.2","Monday.3","Monday.4","Monday.5","Monday.6","Monday.7","Monday.8","Monday.9","Monday.10","Monday.11","Monday.12","Monday.13","Monday.14",
               "Tuesday.1","Tuesday.2","Tuesday.3","Tuesday.4","Tuesday.5","Tuesday.6","Tuesday.7","Tuesday.8","Tuesday.9","Tuesday.10","Tuesday.11","Tuesday.12","Tuesday.13","Tuesday.14",
               "Wednesday.1","Wednesday.2","Wednesday.3","Wednesday.4","Wednesday.5","Wednesday.6","Wednesday.7","Wednesday.8","Wednesday.9","Wednesday.10","Wednesday.11","Wednesday.12","Wednesday.13","Wednesday.14",
               "Thursday.1","Thursday.2","Thursday.3","Thursday.4","Thursday.5","Thursday.6","Thursday.7","Thursday.8","Thursday.9","Thursday.10","Thursday.11","Thursday.12","Thursday.13","Thursday.14",
               "Friday.1","Friday.2","Friday.3","Friday.4","Friday.5","Friday.6","Friday.7","Friday.8","Friday.9","Friday.10","Friday.11","Friday.12","Friday.13","Friday.14")

listOfDays_iterate = listOfDays # REWORK THIS TO MAKE SURE

# There are 14 shifts per day with the max required students given below

data_Monday = c(4,3,3,3,2,2,2,2,2,2,2,2,2,2)
data_Wednesday = data_Monday
data_Friday = data_Monday

data_Tuesday = c(3,3,3,3,2,2,2,2,2,2,2,2,2,2)
data_Thursday = data_Tuesday

matrix_1 = matrix(data_Monday,14,1)
matrix_2 = matrix(data_Tuesday,14,1)

maxStudentsPerShift = rbind(matrix_1,matrix_2,matrix_2,matrix_2,matrix_1)

#preallocate space for speed...
newCounter = 1
numberOfNewRows = length(MasterListOfShifts)
newBlankRows = matrix(0,numberOfNewRows,length(namesOfVariables))
colnames(newBlankRows) = namesOfVariables

ThirteenthConstraintMatrix = newBlankRows
ThirteenthInequalities = matrix("",0,1)
ThirteenthRightHandSide = matrix(0,0,1)

for(aa in 1:length(MasterListOfShifts)){
  ThirteenthNewConstraint = matrix(0,1,length(namesOfVariables)) #initialize
  colnames(ThirteenthNewConstraint) = namesOfVariables #name locations for easy finding of stuff
  ThirteenthNewInequality = matrix("<=",1,1)
  ThirteenthNewRightHandSide = matrix(maxStudentsPerShift[[aa]],1,1)
  #now add the new stuff onto the ends of the overall vectors:
  ThirteenthInequalities = rbind(ThirteenthInequalities,ThirteenthNewInequality)
  ThirteenthRightHandSide = rbind(ThirteenthRightHandSide, ThirteenthNewRightHandSide)
  
  regularExpressionToSearchFor = paste0(listOfDays_iterate[aa],"$")
  
  ThirteenthNewConstraint = easyWriter(ThirteenthNewConstraint,namesOfVariables,regularExpressionToSearchFor,1)
  
  regex = "Attribute.*"
  
  ThirteenthNewConstraint = easyWriter(ThirteenthNewConstraint,namesOfVariables,regex,0)
  
  #now add the new stuff onto the ends of the overall vectors:
  ThirteenthConstraintMatrix[newCounter,] = ThirteenthNewConstraint
  
  newCounter = newCounter + 1
}


####################################################
# Now it's time to build our big constraint matrix!!
####################################################

ConstraintMatrix = rbind(FirstConstraintMatrix,SecondConstraintMatrix,ThirdConstraintMatrix,FourthConstraintMatrix,FifthConstraintMatrix,SixthConstraintMatrix,SeventhConstraintMatrix,EighthConstraintMatrix,NinthConstraintMatrix,TenthConstraintMatrix,EleventhConstraintMatrix,TwelfthConstraintMatrix,ThirteenthConstraintMatrix)
Inequalities = rbind(FirstInequalities,SecondInequalities,ThirdInequalities,FourthInequalities,FifthInequalities,SixthInequalities,SeventhInequalities,EighthInequalities,NinthInequalities,TenthInequalities,EleventhInequalities,TwelfthInequalities,ThirteenthInequalities)
RightHandSide = rbind(FirstRightHandSide,SecondRightHandSide,ThirdRightHandSide,FourthRightHandSide,FifthRightHandSide,SixthRightHandSide,SeventhRightHandSide,EighthRightHandSide,NinthRightHandSide,TenthRightHandSide,EleventhRightHandSide,TwelfthRightHandSide,ThirteenthRightHandSide)


###########################
# Let's build our LP now!!!
###########################

#load the appropriate libraries:
library(lpSolveAPI)
library(lpSolve)

#Initialize the LP
MyFinalTestLP = make.lp(NROW(ConstraintMatrix),NCOL(ConstraintMatrix))
set.type(MyFinalTestLP,seq(1,NCOL(ConstraintMatrix)-1),type=c("binary")) #this is sort of optional, but if you wanted these variables to be constrained to be either 0 or 1, with no halfsies, this is the way to do it.

#set objective
set.objfn(MyFinalTestLP,objectiveFunction)
#set control for a maximization problem
lp.control(MyFinalTestLP,sense='max')

#write constraints:
for(rowCounter in 1:NROW(ConstraintMatrix)){
  set.row(MyFinalTestLP,rowCounter,ConstraintMatrix[rowCounter,])
  set.constr.type(MyFinalTestLP,Inequalities[rowCounter],rowCounter)
  set.rhs(MyFinalTestLP, RightHandSide[rowCounter], rowCounter)
}

write.lp(MyFinalTestLP,'MyFinalTestLP.mps',type='mps')

#################################
# Solving using COIN-OR-CBC in R:
#################################

system("/Scheduler/CoinOR/bin/cbc.exe", input='import MyFinalTestLP.mps solve solution myFinalTestSolution.txt', wait=TRUE)

results = data.frame(read.table("myFinalTestSolution.txt", skip=1, header=FALSE)) #Line one is the objective value - skip it
results$V3 = NULL #all ones in our case, so we drop it
results$V1 = results$V1 + 1 #This helped us match the index to the variable number (e.g. 20 | C21 becomes 21 | C21)
colnames(results) = c('var_num', 'var', 'var_val')
optimalObjectiveValue = sum(results$var_val) #hey, our objective value is back

whereItsAt = namesOfVariables[results$var_num] #Take that helpful index, go back to the list of names you fed COINOR, and pull the names by the index

find = grep("Attribute1.Monday",whereItsAt,value = TRUE)#Find all Monday positions that could potentially be after Student data

position = pmatch(find[[1]],whereItsAt)# Find the position of the first Non-Student entry
whereItsAt = whereItsAt[1:position - 1]# Remove all Non-Student entries

write.csv(whereItsAt, file = "Temp.csv")# Write the vector answer to a .csv file

shell.exec("Temp.csv")# Open the file in Excel

shell.exec("Answer.xlsx")# Open the answer file in Excel

proc.time()-start