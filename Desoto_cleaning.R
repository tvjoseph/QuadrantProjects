# JMJPFU
# 19-Sept-2016

# In this script let us make a large function to consolidate the unique information of Students and School according to month wise consolidation


# Task 1 : Get all the unique Student IDs together over which we will run the For loop

# Student list we will get from the Student_School_Association file.

library(dplyr)
cou = 0

for(i in 1:nrow(Student_school_Association)){
  
 
  tempid <- paste(Student_school_Association[i,1]) # Getting the Id of the student
  
  #tempred <- Student_school_Association[i,] # Get all the records for that student
  
  # Data 1 ##  First getting the Discipline data for this student
  
  temp_discipline <- Stud_Disc_InAsc_Master %>% filter(StudentUniqueId==tempid)
  
  if(nrow(temp_discipline) > 0){
    
    tab2 <- Stud_Disc_Act_Master %>% filter(StudentUniqueId==tempid) # Taking the info from the Discipline action data
    tab3 <- merge(temp_discipline,Discipline_Indicator_Master,by="IncidentIdentifier",all.x=TRUE) # getting all information
    tab3$IncidentDate <- as.Date(as.character(tab3$IncidentDate)) # Converting the date to a charachter object and then making a new date format
    
    tab3$Month <- format(tab3$IncidentDate,"%B") # Making a new variable for Month
    
    tab3$Day <- format(tab3$IncidentDate,"%a") # Making a new variable for day
    
    tab3 <- tab3 %>% select(SchoolName,BehaviorDetailedDescription,IncidentDate)
    
    colnames(tab3)[3] <- "Date"
    
    temp_discipline <- merge(tab3,Discipline_template,by="BehaviorDetailedDescription",all.x=TRUE) 
    
    colnames(temp_discipline)[4] <- "Discore"
    
    
  } # If condition for checking whether there are any discipline data for the considered student
  
  
  # Data 2 ## Getting the Attendence data like the discipline data
  
  temp_attendence <- attend_trans %>% filter(StudentUniqueStateId==tempid) # Not a single incidence of absenteeism
  
  # Take only relevant columns in temp_attendence
  
  temp_attendence <- temp_attendence %>% select(StudentUniqueStateId,AttendanceEventReason,StateOrganizationId,LocalCourseCode,ClassPeriodName,Location,Date,Month,Day,Week,reasons,counts,HispanicLatinoEthnicity,Race,EconomicDisadvantaged,SchoolFoodServicesEligibility,normrace,normpop)
  
  # Data 3 ## Getting the grade information of the particular student
  
  temp_grade <- stud_grade %>% filter(ns1.StudentUniqueStateId==tempid)
  
  # Data 4 ## Getting the temp timeline
  
  temp_timeline <- samp_dates
  
  colnames(temp_timeline)[1] <- "Date" # Renaming the temp_timeline date column
  
  # Data Merge 1 # Attendence data Merge
  
  if(nrow(temp_attendence) > 0){
    
    temp_timeline <- merge(temp_timeline,temp_attendence,by="Date",all=TRUE) # merged all records of attendence
    
  }else {
    
    temp_timeline[,3:19] <- NA
    colnames(temp_timeline)[3:19] <- c("StudentUniqueStateId","AttendanceEventReason","StateOrganizationId","LocalCourseCode","ClassPeriodName","Location","Month","Day","Week","reasons","counts","HispanicLatinoEthnicity","Race","EconomicDisadvantaged","SchoolFoodServicesEligibility","normrace","normpop")
    
    
  } # End of If loop for temp_attendence merge
  
  # JMJPFU
  # 20-Sept-2016
  
  # Data Merge 2 - Discipline data
  
  # Cleaning up the discipline data
  
  if(nrow(temp_discipline) > 0) {
    
    # Merging the data sets
    
    temp_timeline <- merge(temp_timeline,temp_discipline,by="Date",all=TRUE)
    
    
  }else{
    
    temp_timeline[,20:22] <- NA 
    colnames(temp_timeline)[20:22] <- c("BehaviorDetailedDescription","SchoolName","Discore" )
    
    
  } # End of the If loop for discipline merge
  
  
  # Data Merge 3 - Getting the grade data
  
  # Getting the basic dates
  
  grad_dates <- data.frame(Dates=c("2015-10-13","2015-11-10","2015-12-16","2016-02-04","2016-03-07","2016-04-07","2016-06-10"))
  
  grad_dates[,2:28] <- NA # Defining the rest 27 variables pertaining to grades
  
  colnames(grad_dates)[1] <- "Date"
  
  colnames(grad_dates)[2:28] <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","23","26","28","29","RA", "RB","EA","EB","MA","MB","AS","BS","SA","SB") 
  
  # Calling the function to fix the grade details
  
  if(nrow(temp_grade) > 0){
    
    school_id <- unique(temp_grade %>% select(ns1.StateOrganizationId))
    samp_grade <- data.frame(matrix(nrow=0,ncol=29))
    names(samp_grade) <- c(names(grad_dates),"school")
    
    if(nrow(school_id) > 1){
      
      for(l in 1:nrow(school_id)){
        
        temp_school <- paste(school_id[l,1])
        
        filt_grade <- temp_grade %>% filter(ns1.StateOrganizationId == temp_school)
        temsamp_grade <- get_grade(filt_grade,grad_dates)
        
        temsamp_grade$school <- temp_school
        samp_grade <- rbind(samp_grade,temsamp_grade)
        
      } # End of for loop to loop over all schools
      
      
      
    }else {
      
      temsamp_grade <- get_grade(temp_grade,grad_dates) # Getting all the grade information in samp_grade 
      temsamp_grade$school <- paste(school_id[1,1])
      samp_grade <- rbind(samp_grade,temsamp_grade)
      
    } # If loop for looping over the number of grades in school
    
    samp_grade$StudID <- paste(temp_grade$ns1.StudentUniqueStateId[1])
    
  }else{samp_grade <- grad_dates 
  
  samp_grade$school <- NA
  
  samp_grade$StudID <- paste(temp_grade$ns1.StudentUniqueStateId[1])
  
  }
  
 
  
  # Merging the grade info with the temp_timline
  
  temp_timeline <- merge(temp_timeline,samp_grade,by="Date",all=TRUE) 
  
  # Data Operation 3 - Cleaning up all the merged data and creating a final data frame
  
  Final_timeline <- data.frame(matrix(nrow=0,ncol=51))
  colnames(Final_timeline) <- names(temp_timeline)
  
  # Now to eliminate all those rows where there is no data.
  
 
  
  for(i in 1:nrow(temp_timeline)){
    
      
      if(sum(as.numeric(temp_timeline[i,3:49]),na.rm = TRUE) !=0){ 
       # cou = cou+1
        
        #Final_timeline[cou,1:50] <- paste(as.character(temp_timeline[i,1:50]))
        Final_timeline <- rbind(Final_timeline,temp_timeline[i,])
        
      } # End of the if loop
    
   
    
  } # End of the rows For loop
 
  
 
  
  # Converting all characters to numeric
  
  for(i in c(18,19,22:49)){
    Final_timeline[,i] <- as.numeric(Final_timeline[,i]) 
  }
  
  
 if(nrow(Final_timeline) > 0){
   
   # Creating a consolidated grade
   for(i in 1:nrow(Final_timeline)){
     
     Final_timeline$Congrade[i] <- sum(Final_timeline[i,23:49],na.rm=TRUE)
   }
   
   Final_timeline$StudID <- paste(tempid)
   
   # Creating the final data set for all the students
   
   New_Stud_timeline <- rbind(New_Stud_timeline,Final_timeline)
   
 } # End of IF loop for nrow(Final_timeline) > 0
  
  
 
  
} # End of the file where we iterate over the list of students

# Tomorrow, get the other variables also as part of this table and construct the total data frame to work with


  

New_Stud_timeline <- data.frame(matrix(nrow=0,ncol=52))
colnames(New_Stud_timeline) <- names(Final_timeline)



# JMJPFU
# 22 Sept 2016

# Back up of Student data

All_Stud_timeline_back <- All_Stud_timeline

# JMJPFU
# 23 Sept 2016

# Let us now, try to fill in the missing details of Student Unique ID

All_Stud_timeline_back2 <- All_Stud_timeline

for(i in 1:nrow(All_Stud_timeline)){
  
  tempid <- paste(All_Stud_timeline$StudentUniqueStateId[i])
  
  if(tempid == "NA"){
    
    All_Stud_timeline$StudentUniqueStateId[i] <- paste(All_Stud_timeline$StudID[i])
    
    
  }
  
  
  
} # End of first for loop

# For inserting for discipline score


for(i in 1:nrow(All_Stud_timeline)){
  
  tempid <- paste(All_Stud_timeline$StudentUniqueStateId[i])
  
  
  if(tempid == "NA"){
    
    temp1 = All_Stud_timeline$StudentUniqueStateId[(i-1)]
    temp2 = All_Stud_timeline$StudentUniqueStateId[(i+1)]
    
    if(temp1==temp2){
    
    All_Stud_timeline$StudentUniqueStateId[i] <- paste(All_Stud_timeline$StudentUniqueStateId[(i-1)])
    }
    
  }
  
}

81874

All_Stud_timeline$NewDate <- as.Date(as.numeric(All_Stud_timeline$Date),origin = "1970-01-01")


test <- All_Stud_timeline[All_Stud_timeline$StudentUniqueStateId== "NA" & !is.na(All_Stud_timeline$Discore),]

test <- test %>% filter(!is.na(Discore))

test1 <- All_Stud_timeline[661691:661702,]

# Month wise plan


for(i in 1:nrow(All_Stud_timeline)){
  
  tempid <- paste(All_Stud_timeline$StudentUniqueStateId[i])
  
  
  if(tempid == "NA"){
    
    tempdate <- format(All_Stud_timeline$NewDate[i],"%B")
    
    
    
    if(!is.na(tempdate)& tempdate=="September"){
      
      All_Stud_timeline$StudentUniqueStateId[i] <- paste(All_Stud_timeline$StudentUniqueStateId[(i+1)])
    }
    
  }
  
}

nrow(All_Stud_timeline[All_Stud_timeline$StudentUniqueStateId== "NA",])

# Imputing

All_Stud_timeline$StudentUniqueStateId[355964] <- paste(All_Stud_timeline$StudentUniqueStateId[355963])
# 23 Sept - Taking a back up again before rerunning the code again

All_Stud_timeline_back3 <- All_Stud_timeline

# JMJPFU
# 26-Sept-2016

# Need to remove duplicates from the data set

# So all the potential duplicates were summarised in the Adl_stud_timline DF.

temp <- unique(Newadl_Stud_timeline$StudID) # 435 unique students had duplicates

# Lets remove those ids from the main file

New_Stud_timeline_back <- New_Stud_timeline

New_Stud_timeline$Dup <- NA # Creating a new variable call duplicate

for(i in 1:length(temp)){
  
  tempid <- paste(temp[i])
  print(i)
  
  for(j in 1:nrow(New_Stud_timeline)){
    
    if(New_Stud_timeline$StudID[j] == tempid ){
      
      New_Stud_timeline$Dup[j] <- "Duplicate"
      
      
    }
    
  }
  
  
  
}

# Now to filter those items with Duplicate and check them up

temp2 <- New_Stud_timeline %>% filter(Dup == "Duplicate")

length(unique(temp2$StudID))

# The records with Duplicate have to be removed

New_Stud_timeline_new <- New_Stud_timeline %>% filter(is.na(Dup))

New_Stud_timeline_new <- New_Stud_timeline_new[,1:52] # Removing the duplicate column

New_Stud_timeline <- rbind(New_Stud_timeline_new,Newadl_Stud_timeline)

# Removing all unwanted data frames

All_Stud_timeline_new <- NULL

# Now to add the missing School Ids for each records

All_Stud_timeline_back3 <- All_Stud_timeline

# Adding missing school IDs 

for(i in 1:nrow(New_Stud_timeline)){
  
  
  if(New_Stud_timeline$StateOrganizationId[i] == "NA"){
    
    tempname <- paste(New_Stud_timeline$SchoolName[i])
    New_Stud_timeline$StateOrganizationId[i]<- paste(school_id %>% filter(schoolname == tempname) %>% select(SchoolId))
  
    # tempid <- paste(All_Stud_timeline$StudID[i])
    # 
    # temp_sch <- unique(All_Stud_timeline %>% filter(StudID == tempid) %>% select(StateOrganizationId))
    # 
    # ifelse(temp_sch[1,1] == "NA", All_Stud_timeline$StateOrganizationId[i] <- paste(temp_sch[2,1]),All_Stud_timeline$StateOrganizationId[i] <- paste(temp_sch[1,1]))
    
    
  }
  
  
}



temp <- All_Stud_timeline_back3 %>% filter(StudID == "9746172283")


tempred <- Student_school_Association %>% filter(StudentUniqueStateId == "9746172283" )
# Testing when a student has more than one school

tempid <- "4077801881"

temp_grade <- stud_grade %>% filter(ns1.StudentUniqueStateId==tempid)

First Nine Weeks  First Semester    Second Nine Weeks Fourth Nine Weeks
[5] Second Semester   Third Nine Weeks 

temp_seme <- temp_grade %>% filter(ns1.GradingPeriod == "Second Semester" )

temp_grade <- temp_grade %>% arrange(desc(ns1.ClassPeriodName))

# JMJPFU
# 27 - Sept 2016

# To check for grade info for some students

temp <- stud_grade %>% filter(ns1.StudentUniqueStateId == "3885717684")

temp1 <- Student_school_Association %>% filter(StudentUniqueStateId == "3885717684" )

# Need to get a unique mapping of School ID and School Names

school_id <- data.frame(unique(Student_school_Association$SchoolId))
school_name<- data.frame(unique(Student_school_Association$NameOfInstitution))
colnames(school_name) <- colnames(Student_school_Association$NameOfInstitution)
school_id$SchoolId <- as.factor(school_id$SchoolId)

colnames(school_id) <- colnames(mapping)[1]

mapping <- Student_school_Association %>% select(SchoolId,NameOfInstitution)

mapping$SchoolId <- as.factor(mapping$SchoolId)

School_map <- merge(school_id,mapping, by = "SchoolId")

School_map <- semi_join(school_id,mapping)

# Merge is not working. Let me make a for loop

mapping$NameOfInstitution <- as.character(mapping$NameOfInstitution)

school_id$schoolname <- NA

for(i in 1:nrow(school_id)){
  tempid <- paste(school_id$SchoolId[i])
  
  school_id$schoolname[i] <- unique(mapping %>% filter(SchoolId == tempid)%>% select(NameOfInstitution))
  
  
  
}

############################################# New format for Data #########################

# JMJPFU
# 29-Sept-2016

Desoto_Master <- data.frame(matrix(nrow = 0,ncol=12))

colnames(Desoto_Master) <- c("Date","StudentID","SchoolID","Schoolname","Variable","FirstTier","SecondTier","ThirdTier","FourthTier","FifthTier","Race","Economic")

students <- unique(Student_school_Association$StudentUniqueStateId)

for(i in 1:length(students)){
  
  tempdf <- data.frame(matrix(nrow = 0,ncol=10))
  colnames(tempdf) <- c("Date","StudentID","SchoolID","Schoolname","Variable","FirstTier","SecondTier","ThirdTier","FourthTier","FifthTier")
  
  tempid <- paste(students[i])
  
  
  # Getting Discipline Data
  
  temp_discipline <- Stud_Disc_InAsc_Master %>% filter(StudentUniqueId==tempid)
  
  if(nrow(temp_discipline) > 0){
    
    tab2 <- Stud_Disc_Act_Master %>% filter(StudentUniqueId==tempid) # Taking the info from the Discipline action data
    tab3 <- merge(temp_discipline,Discipline_Indicator_Master,by="IncidentIdentifier",all.x=TRUE) # getting all information
    
    tab3 <- tab3 %>% select(IncidentDate,StudentUniqueId,SchoolId,SchoolName,BehaviorDetailedDescription)
    
    colnames(tab3) <- c("Date","StudentID","SchoolID","Schoolname","ThirdTier")
    
    tab3$Variable <- 1
    tab3$FirstTier <- "Discipline"
    tab3$SecondTier <- "Discipline"
    
    tab3 <- tab3 %>% select(Date,StudentID,SchoolID,Schoolname,Variable,FirstTier,SecondTier,ThirdTier)
    
    tab3$FourthTier <- tab3$ThirdTier
    tab3$FifthTier <- tab3$ThirdTier
    
    tempdf <- rbind(tempdf,tab3) # Combining the data with the tempdf
    
    tempdf$Date <- as.character(tempdf$Date) # Just converting into charachter for later rbind
    
  } # If condition to check if student discipline data exists
  
  ##############################################################################
  
  # Getting Grade Data
  
  temp_grade <- stud_grade %>% filter(ns1.StudentUniqueStateId==tempid)
  
  if(nrow(temp_grade) > 0){
    
    tab3 <- temp_grade %>% select(ns1.GradingPeriod,ns1.StudentUniqueStateId,ns1.StateOrganizationId,ns1.NumericGradeEarned,ns1.LocalCourseCode,ns1.ClassPeriodName)
    colnames(tab3) <- c("Date","StudentID","SchoolID","Variable","ThirdTier","SecondTier")
    tab3$FirstTier <- "Grade"
    tab3$Schoolname <- "NA"
    tab3 <- tab3 %>% select(Date,StudentID,SchoolID,Schoolname,Variable,FirstTier,SecondTier,ThirdTier)
    tab3$FourthTier <- tab3$ThirdTier
    tab3$FifthTier <- tab3$ThirdTier
    tempdf <- rbind(tempdf,tab3) # Combining the data with the tempdf
    
    
  } # End of If loop if the grade info exists
  
  ##################################################################################################
  
  # Getting Attendence data
  
  temp_attendence <- attend_trans %>% filter(StudentUniqueStateId==tempid)
  
  if(nrow(temp_attendence) > 0){
    
    tab3 <- temp_attendence %>% select(Date,StudentUniqueStateId,StateOrganizationId,counts,reasons,AttendanceEventReason,LocalCourseCode,ClassPeriodName)
    
    tab3$FirstTier <- "Attendence"
    tab3$Schoolname <- "NA"
    tab3 <- tab3 %>% select(Date,StudentUniqueStateId,StateOrganizationId,Schoolname,counts,FirstTier,reasons,AttendanceEventReason,LocalCourseCode,ClassPeriodName)
    colnames(tab3) <- c("Date","StudentID","SchoolID","Schoolname","Variable","FirstTier","SecondTier","ThirdTier","FourthTier","FifthTier")
    tab3$Date <- as.character(tab3$Date)
    
    tempdf <- rbind(tempdf,tab3)
    
  }
  
  
  
  tempaddl <- Std_demographics %>% filter(StudentUniqueStateId == tempid) %>% select(Race,EconomicDisadvantaged)
  
  if(nrow(tempdf) > 0){
    tempdf$Race <- paste(tempaddl$Race[1])
    tempdf$Economic <- paste(tempaddl$EconomicDisadvantaged[1])
    Desoto_Master <- rbind(Desoto_Master,tempdf)
    
    
  }
  
  print(i)
  
} # End of the for loop for looping over all students

# Let us do some testing with the current DF

school_agg1 <- Desoto_Master %>% group_by(SchoolID,FirstTier) %>% summarise(sum(as.numeric(Variable)))
student_agg1 <- Desoto_Master %>% group_by(StudentID,FirstTier) %>% summarise(sum(as.numeric(Variable)))

# JMJPFU
# 30-Sept-2016

# Cleaning the Desoto Master further - 

Desoto_backup <- Desoto_Master

# Step 1 : Including dates for the grades

unique(Desoto_Master %>% filter(FirstTier == "Grade") %>% select(Date))

grad_dates$Date


for(i in 1:nrow(Desoto_Master)){
  print(i)
  
  if(Desoto_Master$Date[i] == "First Nine Weeks"){Desoto_Master$Date[i] <- paste(grad_dates$Date[1]) }
  else if(Desoto_Master$Date[i] == "Second Nine Weeks"){Desoto_Master$Date[i] <- paste(grad_dates$Date[2]) }
  else if(Desoto_Master$Date[i] == "First Semester"){Desoto_Master$Date[i] <- paste(grad_dates$Date[3]) }
  else if(Desoto_Master$Date[i] == "Third Nine Weeks"){Desoto_Master$Date[i] <- paste(grad_dates$Date[4]) }
  else if(Desoto_Master$Date[i] == "Second Semester"){Desoto_Master$Date[i] <- paste(grad_dates$Date[5]) }
  else if(Desoto_Master$Date[i] == "Fourth Nine Weeks"){Desoto_Master$Date[i] <- paste(grad_dates$Date[6]) }
  else if(Desoto_Master$Date[i] == "End of Year"){Desoto_Master$Date[i] <- paste(grad_dates$Date[7]) }
  
}

# Getting school names correct

temp_school <- unique(Desoto_Master %>% select(SchoolID,Schoolname))

temp_school <- unique(Student_school_Association %>% select(SchoolId,NameOfInstitution))

temp_school <- temp_school[temp_school$Schoolname != "NA",]


for(i in 7920:nrow(Desoto_Master)){
  print(i)
  
  tempid <- paste(Desoto_Master$SchoolID[i])
  schoolname <- temp_school %>% filter(SchoolId == tempid) %>% select(NameOfInstitution)
  Desoto_Master$Schoolname[i] <- paste(schoolname[1,1])
  
}

# Now to create the Normalising variable



Desoto_New <- data.frame(matrix(nrow=0,ncol=13))
colnames(Desoto_New) <- names(studrec1)

# Creating some school records
Student_school_Association$SchoolId <- as.factor(Student_school_Association$SchoolId)
Student_school_Association$StudentUniqueStateId <- as.character(Student_school_Association$StudentUniqueStateId)
school_count <- Student_school_Association %>% group_by(SchoolId) %>% summarise(length(StudentUniqueStateId))



for(i in 6:length(students)){
  print(i)
  
  tempid <- paste(students[i])
  temprec <- Desoto_Master %>% filter(StudentID == tempid)
  temptier <- unique(temprec$FirstTier)
  tempschool <- unique(temprec$SchoolID)
  
  if(length(tempschool)==1){
    
    for(j in 1:length(temptier)){
      
      tier <- temptier[j]
      
      # For Grade information
      
      if(tier == "Grade"){
        
        graderec <- temprec %>% filter(FirstTier==tier)
        
        sectier <- unique(graderec$SecondTier) # Finding all subjects
        
        secnorm <- length(sectier) # Getting length of subjects
        
         for(k in 1:length(sectier)){
           
           tempsubj <- sectier[k]
           
           studrec1 <-  temprec %>% filter(SecondTier == tempsubj) 
           thirnorm <- nrow(studrec1)
           
           studrec1$Normvar <- as.numeric(studrec1$Variable)/(100 * secnorm * thirnorm)
           
           Desoto_New <- rbind(Desoto_New,studrec1)
           
           
         } # Looping over all subjects
        
        
      } # End of loop if the tier is grade
      else if(tier == "Attendence"){
        
       attrec <-  temprec %>% filter(FirstTier==tier)
       
       schnorm <- school_count %>% filter(SchoolId == tempschool) %>% select(`length(StudentUniqueStateId)`)
        
       divis <- as.numeric(paste(schnorm[1,1]))
        
       attrec$Normvar <- as.numeric(attrec$Variable)/divis
        
       Desoto_New <- rbind(Desoto_New,attrec)
        
      }
      else if(tier == "Discipline"){
        
        discrec <-  temprec %>% filter(FirstTier==tier)
        
        schnorm <- school_count %>% filter(SchoolId == tempschool) %>% select(`length(StudentUniqueStateId)`)
        
        divis <- as.numeric(paste(schnorm[1,1]))
        
        discrec$Normvar <- as.numeric(discrec$Variable)/divis
        
        Desoto_New <- rbind(Desoto_New,discrec)
        
      }
      
      
    } # Looping over all the First tiers ( grade, Discipline, Attendence)
    
    
  } # End of IF loop when number of school is only one
  
  else if(length(tempschool) > 1){
    
    for(l in 1:length(tempschool)){
      
      school <- paste(tempschool[l])
      new_temprec <- temprec %>% filter(StudentID == tempid & SchoolID == school)
      temptier <- unique(new_temprec$FirstTier)
      
      for(j in 1:length(temptier)){
        
        tier <- temptier[j]
        
        # For Grade information
        
        if(tier == "Grade"){
          
          graderec <- new_temprec %>% filter(FirstTier==tier)
          
          sectier <- unique(graderec$SecondTier) # Finding all subjects
          
          secnorm <- length(sectier) # Getting length of subjects
          
          for(k in 1:length(sectier)){
            
            tempsubj <- sectier[k]
            
            studrec1 <-  new_temprec %>% filter(SecondTier == tempsubj) 
            thirnorm <- nrow(studrec1)
            
            studrec1$Normvar <- as.numeric(studrec1$Variable)/(100 * secnorm * thirnorm)
            
            Desoto_New <- rbind(Desoto_New,studrec1)
            
            
          } # Looping over all subjects
          
          
        } # End of loop if the tier is grade
        else if(tier == "Attendence"){
          
          attrec <-  new_temprec %>% filter(FirstTier==tier)
          
          schnorm <- school_count %>% filter(SchoolId == school) %>% select(`length(StudentUniqueStateId)`)
          
          divis <- as.numeric(paste(schnorm[1,1]))
          
          attrec$Normvar <- as.numeric(attrec$Variable)/divis
          
          Desoto_New <- rbind(Desoto_New,attrec)
          
        }
        else if(tier == "Discipline"){
          
          discrec <-  new_temprec %>% filter(FirstTier==tier)
          
          schnorm <- school_count %>% filter(SchoolId == school) %>% select(`length(StudentUniqueStateId)`)
          
          divis <- as.numeric(paste(schnorm[1,1]))
          
          discrec$Normvar <- as.numeric(discrec$Variable)/divis
          
          Desoto_New <- rbind(Desoto_New,discrec)
          
        }
        
        
      } # Looping over all the First tiers ( grade, Discipline, Attendence)
      
      
      
    } # End of For loop to go over all schools in first loop
    
    
    
    
  } # End of if loop to go over the condition if there are more schools for a student
  
  
    
  } # Final loop to go over for all students


################### Once again the normalisation calculation ################

# JMJPFU
# 03-10-2016

# Getting the School count as part of the data

for(i in 1:nrow(Desoto_New)){
  print(i)
  
  tempid <- paste(Desoto_New$SchoolID[i])
  
  tempcount <- school_count %>% filter(SchoolId==tempid) %>% select(`length(StudentUniqueStateId)`)
  
  Desoto_New$Population[i] <- as.numeric(paste(tempcount[1,1]))
  
  
}

# JMJPFU
# 05-10-2016

# Dividing the normalised variable with the population

Desoto_New$Normpop <- Desoto_New$Normvar/Desoto_New$Population

# Multiplying by 1000 to enlarge the scale

Desoto_New$Normpop <- Desoto_New$Normpop/ 1000

# Getting a school name variable

school_name <- unique(School_map)

# Getting a copy of school agg

school_agg1_back <- school_agg1
colnames(school_name)[1] <- names(school_agg1)[1]

# Merging

school_agg1 <- merge(school_agg1,school_name,by="SchoolID",all.x = TRUE)

# Creating a new count of Economic advantaged students per school

stud_record <- Student_school_Association %>% select(StudentUniqueStateId,SchoolId,NameOfInstitution)
stud_record2 <- Std_demographics %>% select(StudentUniqueStateId,EconomicDisadvantaged)

stud_record <- merge(stud_record,stud_record2,by="StudentUniqueStateId",all.x = TRUE)

# Creating a table of both advantaged and disadvantaged students

eco_table <- stud_record %>% filter(SchoolId == "57906109")

table(eco_table$EconomicDisadvantaged)
# No Yes 
# 107 327

# No Yes 
# 256 605

stud_record3 <- Std_demographics %>% select(StudentUniqueStateId,Race)
stud_record <- merge(stud_record,stud_record3,by="StudentUniqueStateId",all.x = TRUE)

table(eco_table$Race)

# Let us look at the unique combination of SecondTier and ThirdTier of Cockrell

cockrell_unique <- Cockrell_grade %>% select(SecondTier,ThirdTier)
nrow(unique(cockrell_unique))

# So there are around 59 unique subjects
