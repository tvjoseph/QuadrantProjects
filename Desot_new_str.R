# JMJPFU
# 27-Sept-2016

######## For Finding the duplicate cases #####################

for(i in 1:nrow(temp)){
  
  
  tempid <- paste(temp[i,1]) # Getting the Id of the student
  
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
    
    Newadl_Stud_timeline <- rbind(Newadl_Stud_timeline,Final_timeline)
    
  } # End of IF loop for nrow(Final_timeline) > 0
  
  
  
  
} # End of the file where we iterate over the list of students


Newadl_Stud_timeline <- data.frame(matrix(nrow=0,ncol=52))
colnames(Newadl_Stud_timeline) <- names(Final_timeline)

# Inserting School ID based on the name of the school

for(i in 1:nrow(New_Stud_timeline)){
  
  #temp <- paste(All_Stud_timeline$StateOrganizationId[i])
  
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

###############################################################


