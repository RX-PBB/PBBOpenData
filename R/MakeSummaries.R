#**************************************************
#
#  Functions for making csv and SummaryAllData
#
#**************************************************
#' makeOpenPBBData_Summaries
#'
#' makes two summary files outputed as a list. csv is the treemap summary. summaryall is the data for the program level summary
#' @param db_name_new name of org database
#' @param db_host_new database host
#' @param db_user database user
#' @param db_pw database pw
#' @param BudgetID Budget ID we want to summarize
#' @param CostModelID Costmodel ID of the PBB accounting structure
#' @export
#' @examples
#' makeOpenPBBData_Summaries(db_name_new,db_host_new,BudgetID,CostModelID)

makeOpenPBBData_Summaries<-function(db_name_new,db_host_new,db_user,db_pw,BudgetID,CostModelID,ScoreReview='ScorePeer'){



  con <- dbConnect(MySQL(),
                   user=db_user,
                   password=db_pw,
                   host=db_host_new,
                   dbname=db_name_new)


  df<-RadSlicer_OpenData(con,BudgetID,CostModelID,ScoreReview)
  df<-df$AcctSummary
  df[,'FTE.Alloc']<-df[,'NumberOfItems']*df[,'PercentAppliedToProg']

  statement<-paste("SELECT * FROM BudgetInfo;",sep='')
  BudgetInfo<-dbGetQuery(conn=con,statement=statement)

  statement<-paste("SELECT * FROM ResultSetup;",sep='')
  ResultSetup<-dbGetQuery(conn=con,statement=statement)

  statement<-paste("SELECT * FROM CostModelInfo WHERE CostModelName='PBB';",sep='')
  CostModelInfo<-dbGetQuery(conn=con,statement=statement)

  statement<-paste("SELECT * FROM ProgInfo;",sep='')
  ProgInfo<-dbGetQuery(conn=con,statement=statement)

  statement<-paste("SELECT * FROM PBBComments;",sep='')
  PBBComments<-dbGetQuery(conn=con,statement=statement)

  statement<-paste("SELECT * FROM ProgBudgetInfo WHERE BudgetID = ",BudgetID,";",sep='')
  ProgBudgetInfo<-dbGetQuery(conn=con,statement=statement)

  dbDisconnect(con)

  Div1<-CostModelInfo$Div1Name
  Div2<-CostModelInfo$Div2Name

  colnames(df)[colnames(df)==Div1]<-'Acct_Department'
  colnames(df)[colnames(df)==Div2]<-'Acct_Division'

  #If Account structure still has existing Department or Division in Div 2 and beyond.
  if(is.element('Division',colnames(df)))(colnames(df)[colnames(df)=='Division']<-'Client_Div')
  if(is.element('Department',colnames(df)))(colnames(df)[colnames(df)=='Department']<-'Client_Dept')
  if(is.element('Description',colnames(df)))(colnames(df)[colnames(df)=='Description']<-'Acct_Description')

  #Make Div1/Div2 consistent with open data code for "Department" "Division"
  colnames(ProgInfo)[colnames(ProgInfo)=='ProgDept']<-'Department'
  colnames(ProgInfo)[colnames(ProgInfo)=='ProgDiv']<-'Division'
  colnames(ProgInfo)[colnames(ProgInfo)=='ProgDescription']<-'Description'

  #clean "none's"
  ProgInfo[ProgInfo$Division=='none','Division']<-ProgInfo[ProgInfo$Division=='none','Department']

  nrow(df)
  df<-merge(df,ProgInfo[,c('ProgID','Department','Division','Description','ProgGroup')],by='ProgID')
  nrow(df)

  bpas<-ResultSetup[ResultSetup$ResultType=='BPA','ResultAbbr']
  community<-ResultSetup[ResultSetup$ResultType=='Community','ResultAbbr']
  governance<-ResultSetup[ResultSetup$ResultType=='Governance','ResultAbbr']

  #The case where there are no allocations
  if(nrow(df)==0){

    col.names<-c('Fixed','Type','Fund','Department','Division','Group','Program','Final Score','Quartile','Prg#','Cost Type',
      'Acct_Fund','Acct_Department','Acct_Division','AcctNumber','Cost/Position','ID#','Cost','FTE','Allocation',bpas,community,governance,
      'AcctType','ItemMeta1','NumberOfItems','TotalCost','ProgNum','Scored','Year','RXCommentID','ProgDescription')
     summaryall<-create_empty_df(length(col.names),col.names)


      data<-list()
      data$csv<-df
      data$summaryall<-summaryall

      return(data)
  }



  insights<-merge(ProgBudgetInfo[,c('ProgID','ProgBudgetNote','PBBCommentID')],PBBComments[,c('PBBCommentID','PBBComment')])
  df<-merge(df,insights[,c('ProgID','PBBComment','ProgBudgetNote')],all.x=T)

  temp<-NULL
  Depts<-split(df,df$Department)
  for (i in 1:length(Depts)){
    Divs<-Depts[[i]]
    Divs<-split(Divs,Divs$Division)
    for (j in 1:length(Divs)){
      Programs<-Divs[[j]]
      Programs<-split(Programs,Programs$ProgID)
      for (k in 1:length(Programs)){
        prog<-Programs[[k]]


        row<-data.frame(
          Year=prog[1,'Year'],
          Type=prog[1,'ServiceType'],
          Department=prog[1,'Department'],
          Division=prog[1,'Division'],
          ProgID=prog[1,'ProgID'],
          Program=prog[1,'ProgName'],
          Quartile=prog[1,'Quartile'],
          Final_Score=prog[1,'FinalScore'],
          TotalCost=sum(prog[prog$AcctType=='Expense','ProgramCost'],na.rm = T),
          FTE=sum(prog[prog$`Cost Type`=='Personnel','FTE.Alloc'],na.rm = T),
          DirectCost=sum(prog[prog$AcctType=='Expense','ProgramCost'],na.rm = T),
          Personnel=sum(prog[prog$AcctType=='Expense' & prog$`Cost Type`=='Personnel','ProgramCost'],na.rm = T),
          NonPersonnel=sum(prog[prog$AcctType=='Expense' & prog$`Cost Type`=='NonPersonnel','ProgramCost'],na.rm = T),
          ProgramRevenue=sum(prog[prog$AcctType=='Revenue','ProgramCost'],na.rm = T),
          BudgetID=BudgetID,
          PBBComment=prog[1,'PBBComment'],
          Insight=prog[1,'ProgBudgetNote'],
          stringsAsFactors=F)

        if(!is.null(bpas)&& length(bpas)!=0){
          for(x in 1:length(bpas)){
            if(is.null(prog[1,bpas[x]]))(bpa.value<-NA)else(bpa.value<-prog[1,bpas[x]])
            row<-cbind(row,bpa.value)
            colnames(row)[length(row)]<-bpas[x]
          }
        }

        if(!is.null(community) && length(community)!=0){
          for(x in 1:length(community)){
            if(is.null(prog[1,community[x]]))(com.value<-NA)else(com.value<-prog[1,community[x]])
            row<-cbind(row,com.value)
            colnames(row)[length(row)]<-community[x]
          }
        }

        if(!is.null(governance) && length(governance)!=0){
          for(x in 1:length(governance)){
            if(is.null(prog[1,governance[x]]))(gov.value<-NA)else(gov.value<-prog[1,governance[x]])
            row<-cbind(row,gov.value)
            colnames(row)[length(row)]<-governance[x]
          }
        }

        row<-cbind(row,prog[1,'Description'])
        colnames(row)[length(row)]<-'Description'

        temp<-rbind(temp,row)
      }

    }
  }

  #**********************************************************
  # Calculate quartile rank of finacial data by department
  #**********************************************************
  Present_Ranking<-function(x,Q1,Q2,Q3){

    if(!is.na(x)){
      Present_Ranking<-1
      if(x>=Q3)(Present_Ranking<-2)
      if(x>=Q2)(Present_Ranking<-3)
      if(x>=Q1)(Present_Ranking<-4)

      #This means all values are zero so there is no ranking
      if(Q1==0)(Present_Ranking<-0)

      return(Present_Ranking)
    }else(return (NA))
  }

  Depts<-temp
  Depts<-split(Depts,Depts$Department)
  temp<-NULL
  for (i in 1:length(Depts)){

    #Get department totals for ranking the individual programs on.

    T_Q1<-quantile(Depts[[i]]$TotalCost,prob=1-25/100)
    T_Q2<-quantile(Depts[[i]]$TotalCost,prob=1-50/100)
    T_Q3<-quantile(Depts[[i]]$TotalCost,prob=1-75/100)

    P_Q1<-quantile(Depts[[i]]$Personnel,prob=1-25/100)
    P_Q2<-quantile(Depts[[i]]$Personnel,prob=1-50/100)
    P_Q3<-quantile(Depts[[i]]$Personnel,prob=1-75/100)

    NP_Q1<-quantile(Depts[[i]]$NonPersonnel,prob=1-25/100)
    NP_Q2<-quantile(Depts[[i]]$NonPersonnel,prob=1-50/100)
    NP_Q3<-quantile(Depts[[i]]$NonPersonnel,prob=1-75/100)

    R_Q1<-quantile(Depts[[i]]$ProgramRevenue,prob=1-25/100)
    R_Q2<-quantile(Depts[[i]]$ProgramRevenue,prob=1-50/100)
    R_Q3<-quantile(Depts[[i]]$ProgramRevenue,prob=1-75/100)

    FTE_Q1<-quantile(Depts[[i]]$FTE,prob=1-10/100)
    FTE_Q2<-quantile(Depts[[i]]$FTE,prob=1-20/100)
    FTE_Q3<-quantile(Depts[[i]]$FTE,prob=1-30/100)

    Divs<-Depts[[i]]
    Divs<-split(Divs,Divs$Division)
    for (j in 1:length(Divs)){
      Programs<-Divs[[j]]
      Programs<-split(Programs,Programs$ProgID)
      for (k in 1:length(Programs)){
        prog<-Programs[[k]]

        #Personnel or NonPersonnel Majority cost
        p_majority<-0
        np_majority<-0
        if(prog$Personnel>prog$NonPersonnel)(p_majority<-4)else(np_majority<-4)

        prog[,'PersonnelDriven']<-p_majority
        prog[,'NonPersonnelDriven']<-np_majority

        #Pertile Rank stats
        prog[,'Rank_Total']<-Present_Ranking(prog[1,'TotalCost'],T_Q1,T_Q2,T_Q3)
        prog[,'Rank_FTE']<-Present_Ranking(prog[1,'FTE'],FTE_Q1,FTE_Q2,FTE_Q3)
        prog[,'Rank_P']<-Present_Ranking(prog[1,'Personnel'],P_Q1,P_Q2,P_Q3)
        prog[,'Rank_NP']<-Present_Ranking(prog[1,'NonPersonnel'],NP_Q1,NP_Q2,NP_Q3)
        prog[,'Rank_R']<-Present_Ranking(prog[1,'ProgramRevenue'],R_Q1,R_Q2,R_Q3)



        temp<-rbind(temp,prog)

      }
    }
  }

  #include an overall column
  temp[,'Overall']<-0
  temp[,'Overall']<-5-as.numeric(temp$Quartile)
  temp[is.na(temp$Overall),'Overall']<-0


  #include governance average
  temp$Governance <- round(rowMeans(subset(temp, select = c(governance)), na.rm = TRUE),digits = 0)
  temp[is.nan(temp$Governance),'Governance']<-NA


  #Include Policy Questions
  #NOTE: 4 is the highest because it is most opportunity!
  temp[,'Policy0']<-0
  temp[temp$DirectCost>0 & temp$ProgramRevenue>0 & (temp$ProgramRevenue/temp$DirectCost>1),'Policy0']<-0
  temp[temp$DirectCost>0 & temp$ProgramRevenue>0 & (temp$ProgramRevenue/temp$DirectCost<=1),'Policy0']<-1
  temp[temp$DirectCost>0 & temp$ProgramRevenue>0 & (temp$ProgramRevenue/temp$DirectCost<=.75),'Policy0']<-2
  temp[temp$DirectCost>0 & temp$ProgramRevenue>0 & (temp$ProgramRevenue/temp$DirectCost<=.5),'Policy0']<-3
  temp[temp$DirectCost>0 & temp$ProgramRevenue>0 & (temp$ProgramRevenue/temp$DirectCost<=.25),'Policy0']<-4
  temp[(temp$ProgramRevenue==0),'Policy0']<-0

  temp[,'Policy1']<-0
  temp[temp$Quartile>2 & temp$Mandate==4 & !is.na(temp$Mandate),'Policy1']<-4

  temp[,'Policy2']<-0
  temp[temp$Quartile>2 & temp$Mandate==2 & !is.na(temp$Mandate),'Policy2']<-4

  temp[,'Policy3']<-0
  temp[temp$Quartile<3 & temp$Reliance>=3 & !is.na(temp$Reliance),'Policy3']<-4

  temp[,'Policy4']<-0
  temp[temp$Quartile>2 & temp$Reliance<3 & !is.na(temp$Reliance),'Policy4']<-4

  temp[,'Policy5']<-0
  temp[temp$Quartile>2 & temp$Reliance<2 & !is.na(temp$Reliance) & temp$Mandate<2 & !is.na(temp$Mandate),'Policy5']<-4



  #screen for :,;,!," our of Department, Division, and Program fields
  for (i in 3:6){
    temp[,i]<-gsub(":","-",temp[,i])
    temp[,i]<-gsub(";","-",temp[,i])
    temp[,i]<-gsub("!","",temp[,i])
    temp[,i]<-gsub("\"","'",temp[,i])
    temp[,i]<-gsub("$","",temp[,i])
    temp[,i]<-gsub("*","",temp[,i])
  }

  #update Community Results with PresentAbbr
  abbrs<-ResultSetup[!is.na(ResultSetup$PresentAbbr),]
  if (nrow(abbrs)>0){
    for(i in 1:nrow(abbrs)){
      colnames(temp)[colnames(temp)==abbrs[i,'ResultAbbr']]<-abbrs[i,'PresentAbbr']

    }
  }

  #sort
  temp<-temp[order(temp$Type,temp$Department,temp$Program),]


  #write.csv(temp,'data_treemap.csv')
  #write.csv(df[df$AcctType=='Expense',],'summaryall.csv')

  #summaryall<-df[df$AcctType=='Expense',]
  summaryall<-df
  summaryall[,'Fixed']<-0
  summaryall[summaryall$Scored=='Prioritized','Fixed']<-1
  summaryall[summaryall$Scored!='Prioritized','Fixed']<-0
  summaryall[,'Type']<-summaryall[,'ServiceType']

  summaryall[,'Group']<-summaryall[,'ProgGroup']
  summaryall[,'Program']<-summaryall[,'ProgName']
  summaryall[,'Final Score']<-summaryall[,'FinalScore']
  summaryall[,'Prg#']<-summaryall[,'ProgID']
  summaryall[,'Acct_Fund']<-summaryall[,'Fund']
  summaryall[,'AcctNumber']<-summaryall[,'AcctCode']

  summaryall[,'Cost/Position']<-summaryall[,'obj_level_01']
  summaryall[,'ID#']<-summaryall[,'obj_level_02']
  summaryall[,'Cost']<-summaryall[,'ProgramCost']
  summaryall[,'FTE']<-summaryall[,'FTE.Alloc']
  summaryall[,'Allocation']<-summaryall[,'PercentAppliedToProg']

  bpas<-bpas[(is.element(bpas,colnames(summaryall)))]
  community<-community[(is.element(community,colnames(summaryall)))]
  governance<-governance[(is.element(governance,colnames(summaryall)))]

  summaryall<-summaryall[c('Fixed','Type','Fund','Department','Division','Group','Program','Final Score','Quartile','Prg#','Cost Type',
                           'Acct_Fund','Acct_Department','Acct_Division','AcctNumber','Cost/Position','ID#','Cost','FTE','Allocation',bpas,community,governance,
                           'AcctType','ItemMeta1','NumberOfItems','TotalCost','ProgNum','Scored','Year','RXCommentID','ProgDescription')]


  data<-list()
  data$csv<-temp
  data$summaryall<-summaryall

  return(data)

}



#' RadSlicer_OpenData
#'
#' Helper Function
#' @param con RMySQL connection
#' @param BudgetID BudgetID to use
#' @param CostModelID PBB CostModel ID
#' @export


RadSlicer_OpenData<-function(con,BudgetID,CostModelID,ScoreReview='PeerScore'){

  #helper function
  colnameIDs_to_Names<-function(df.update, df.LinkID_Name,ID,Name){


    for(i in 1:nrow(df.LinkID_Name)){

      colnames(df.update)[colnames(df.update)==df.LinkID_Name[i,ID]]<-df.LinkID_Name[i,Name]
    }

    return(df.update)
  }


  AcctSummary<-PullAllocations_OpenData(con,BudgetID=BudgetID,CostModelID=CostModelID)


  #df<-PullFinalScores(conexists=con,All.Programs=T)
  df<-PullFinalScores_OpenData(con,BudgetID=BudgetID,CostModelID=CostModelID)
  FinalScores<-df

  statement<-paste("SELECT ResultType, Scored FROM ResultTypes;",sep='')
  ResultTypes<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM BudgetInfo;",sep='')
  BudgetInfo<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM Scores;",sep='')
  Scores<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM ResultSetup;",sep='')
  ResultSetup<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM ResultDefs;",sep='')
  ResultDefs<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM ProgBudgetInfo WHERE BudgetID = ",BudgetID,";",sep='')
  ProgBudgetInfo<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM CostModelInfo WHERE CostModelID = ",CostModelID,";",sep='')
  CostModelInfo<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM PBBComments;",sep='')
  PBBComments<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM RXComments;",sep='')
  RXComments<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM ProgInfo;",sep='')
  ProgInfo<-dbGetQuery(con,statement)

  ResultAbbr<-ResultSetup[which(is.element(ResultSetup$ResultType,ResultTypes[ResultTypes$Scored==1,"ResultType"])),] #Linker df


  if(nrow(Scores[which(is.element(Scores$ResultID,ResultAbbr[ResultAbbr$ResultType=='BPA','ResultID'])),])>0){
    BPAs<-Scores[which(is.element(Scores$ResultID,ResultAbbr[ResultAbbr$ResultType=='BPA','ResultID'])),]
    BPAs<-BPAs[c('ProgID','ResultID',ScoreReview)]
    BPAs<-dcast(BPAs,ProgID~ResultID,value.var = ScoreReview,fill = NA)
    #update colnames to BPAs names
    BPAs<-colnameIDs_to_Names(df.update=BPAs,df.LinkID_Name=ResultAbbr,ID='ResultID',Name='ResultAbbr')
    BPAs.names<-colnames(BPAs[which(!is.element(colnames(BPAs),'ProgID'))])
  }else{
    BPAs<-NULL
    BPAs.names<-NULL
  }

  if(nrow(Scores[which(is.element(Scores$ResultID,ResultAbbr[ResultAbbr$ResultType!='BPA','ResultID'])),])>0){
    Results<-Scores[which(is.element(Scores$ResultID,ResultAbbr[ResultAbbr$ResultType!='BPA','ResultID'])),]
    Results<-Results[c('ProgID','ResultID',ScoreReview)]
    Results<-dcast(Results,ProgID~ResultID,value.var = ScoreReview,fill = NA)
    Results<-colnameIDs_to_Names(df.update=Results,df.LinkID_Name=ResultAbbr,ID='ResultID',Name='ResultAbbr')
    Results.names<-colnames(Results[which(!is.element(colnames(Results),'ProgID'))])
  }else{
    Results<-NULL
    Results.names<-NULL
  }
  #browser()
  #Some merging and combining to get Budget ID's and Q's
  AcctSummary[,'ProgramCost']<-AcctSummary[,'TotalCost']*AcctSummary[,'PercentAppliedToProg']
  AcctSummary<-merge(AcctSummary,ResultTypes[c('ResultType','Scored')],by.x='ServiceType',by.y='ResultType')
  AcctSummary<-merge(AcctSummary,BudgetInfo[c('BudgetID','Year','BudgetName')],by='BudgetID')

  if(nrow(AcctSummary)>0){

      if(nrow(AcctSummary[AcctSummary$Scored==1,])>0){
      AcctSummary[AcctSummary$Scored==1,'Scored']<-'Prioritized'
      }
      if(nrow(AcctSummary[AcctSummary$Scored==0,])>0){
      AcctSummary[AcctSummary$Scored==0,'Scored']<-'Non-Prioritized'
      }
  }

  df<-df$FinalScores
  df<-df[df$Score=='Peer',]
  if(!is.null(df)){
    AcctSummary<-merge(AcctSummary,df[c('ProgID','Quartile','FinalScore')],by='ProgID',all.x = T)
    AcctSummary[is.na(AcctSummary$Quartile),'Quartile']<-'Non-Prioritized'
  }else{

    if(nrow(AcctSummary)>0){
    AcctSummary[,'Quartile']<-'Non-Prioritized'
    AcctSummary[,'FinalScore']<-NA
    }else{
      #add columns for empty data frame case
      AcctSummary<-cbind(AcctSummary,create_empty_df(2,c('Quartile','FinalScore')))

    }

  }


  colnames(AcctSummary)[colnames(AcctSummary)=='Div1']<-CostModelInfo$Div1Name
  colnames(AcctSummary)[colnames(AcctSummary)=='Div2']<-CostModelInfo$Div2Name
  colnames(AcctSummary)[colnames(AcctSummary)=='Div3']<-CostModelInfo$Div3Name
  colnames(AcctSummary)[colnames(AcctSummary)=='Div4']<-CostModelInfo$Div4Name
  colnames(AcctSummary)[colnames(AcctSummary)=='Div5']<-CostModelInfo$Div5Name
  colnames(AcctSummary)[colnames(AcctSummary)=='Div6']<-CostModelInfo$Div6Name
  colnames(AcctSummary)[colnames(AcctSummary)=='Div7']<-CostModelInfo$Div7Name
  colnames(AcctSummary)[colnames(AcctSummary)=='Div8']<-CostModelInfo$Div8Name

  #Add a Year - Budget Column
  if(nrow(AcctSummary)>0){
  AcctSummary[,'BudgetYearName']<-paste(AcctSummary[,'Year']," - ",AcctSummary[,'BudgetName'],sep='')
  }else{
      #add columns for empty data frame case
      AcctSummary<-cbind(AcctSummary,create_empty_df(1,c('BudgetYearName')))

  }
  #********************************
  # Update with Scores
  #*********************************
  if(!is.null(BPAs)){
    AcctSummary<-merge(AcctSummary,BPAs,by='ProgID',all.x=T)
  }
  if(!is.null(Results)){
    AcctSummary<-merge(AcctSummary,Results,by='ProgID',all.x=T)
  }
  #**********************************************************************
  #Update with Comments - use RXComment ID to style by standard basis set
  #**********************************************************************
  CommentIDs<-merge(ProgBudgetInfo[c('ProgID','BudgetID','PBBCommentID')],PBBComments,by='PBBCommentID')
  AcctSummary<-merge(AcctSummary,CommentIDs[c('ProgID','BudgetID','RXCommentID')],by=c('ProgID','BudgetID'),all.x=T)
  colnames(AcctSummary)[colnames(AcctSummary)=='RXCommentID.y']<-'RXCommentID'

  #**********************************************************************
  #Update with ProgDescription
  #**********************************************************************
  AcctSummary<-merge(AcctSummary,ProgInfo[c('ProgID','ProgDescription')],by=c('ProgID'),all.x=T)




  #browser()
  df<-list()
  df$AcctSummary<-AcctSummary
  df$ResultSetup<-ResultSetup
  df$ResultTypes<-ResultTypes
  df$BPA.names<-BPAs.names
  df$Result.names<-Results.names
  df$ResultDefs<-ResultDefs
  df$ProgBudgetInfo<-ProgBudgetInfo
  df$ProgInfo<-ProgInfo
  df$PBBComments<-PBBComments
  df$Scores<-Scores
  df$FinalScores<-FinalScores


  return(df)

}


#' ProgInfo_OpenPBB
#'
#' Helper Function
#' @param con RMySQL connection
#' @export

ProgInfo_OpenPBB<-function(con,BudgetID){

  statement<-paste("SELECT * FROM ProgInfo;",sep='')
  ProgInfo<-dbGetQuery(con,statement)

  #Filter Budget by active programs - NEED to ADD this back AND fix NULL CASE for start-up
  statement<-paste("SELECT ProgID FROM ProgBudgetInfo WHERE BudgetID = ",BudgetID,";",sep='')
  BudgetProgIDs<-dbGetQuery(con,statement)

  ProgInfo<-ProgInfo[which(is.element(ProgInfo$ProgID,BudgetProgIDs$ProgID)),]
  if(nrow(ProgInfo)==0){

    ProgInfo<-NULL

  }
  return(ProgInfo)
}



#' PullAllocations_OpenData
#'
#' Helper Function
#' @param con RMySQL connection
#' @export

PullAllocations_OpenData<-function(con,BudgetID,CostModelID){

  #helper function for here only
  create_IDstring<-function(numeric_vector){

    if (length(numeric_vector)==0)(return(NULL))

    out<-NULL
    out<-paste("('",numeric_vector[1],sep='')
    if (length(numeric_vector)>1){
      for (i in 2:length(numeric_vector)){
        out<-paste(out,"','",numeric_vector[i],sep='')

      }
    }
    out<-paste(out,"')",sep='')

  }
  #print('here 2')

  statement<-paste("SELECT Obj1ID, ObjType,Obj1,Obj1Code FROM Obj1Info WHERE CostModelID='",CostModelID,"';",sep='')
  Obj1<-dbGetQuery(con,statement)
  if(nrow(Obj1)>0)(colnames(Obj1)[2]<-'Cost Type')

  statement<-paste("SELECT Obj2ID, Obj2,Obj2Code FROM Obj2Info WHERE CostModelID='",CostModelID,"';",sep='')
  Obj2<-dbGetQuery(con,statement)

  statement<-paste("SELECT Obj3ID, Obj3,Obj3Code FROM Obj3Info WHERE CostModelID='",CostModelID,"';",sep='')
  Obj3<-dbGetQuery(con,statement)

  statement<-paste("SELECT Obj4ID, Obj4,Obj4Code FROM Obj4Info WHERE CostModelID='",CostModelID,"';",sep='')
  Obj4<-dbGetQuery(con,statement)

  statement<-paste("SELECT Div1ID, Div1,Div1Code FROM Div1Info WHERE CostModelID='",CostModelID,"';",sep='')
  Div1<-dbGetQuery(con,statement)

  statement<-paste("SELECT Div2ID, Div2,Div2Code FROM Div2Info WHERE CostModelID='",CostModelID,"';",sep='')
  Div2<-dbGetQuery(con,statement)

  statement<-paste("SELECT Div3ID, Div3,Div3Code FROM Div3Info WHERE CostModelID='",CostModelID,"';",sep='')
  Div3<-dbGetQuery(con,statement)

  statement<-paste("SELECT Div4ID, Div4,Div4Code FROM Div4Info WHERE CostModelID='",CostModelID,"';",sep='')
  Div4<-dbGetQuery(con,statement)

  statement<-paste("SELECT Div5ID, Div5,Div5Code FROM Div5Info WHERE CostModelID='",CostModelID,"';",sep='')
  Div5<-dbGetQuery(con,statement)

  statement<-paste("SELECT Div6ID, Div6,Div6Code FROM Div6Info WHERE CostModelID='",CostModelID,"';",sep='')
  Div6<-dbGetQuery(con,statement)

  statement<-paste("SELECT Div7ID, Div7,Div7Code FROM Div7Info WHERE CostModelID='",CostModelID,"';",sep='')
  Div7<-dbGetQuery(con,statement)

  statement<-paste("SELECT Div8ID, Div8,Div8Code FROM Div8Info WHERE CostModelID='",CostModelID,"';",sep='')
  Div8<-dbGetQuery(con,statement)


  #Programs
  ProgInfo<-ProgInfo_OpenPBB(con=con,BudgetID=BudgetID)
  #Get Programs and Program Alloations by BudgetID:


  #Get the Item and Acct Info Make it faster by subsetting by user group pull
  #if(UserGroup=='All Available'){
  statement<-paste("SELECT ItemID, AcctID FROM ItemInfo WHERE CostModelID = ",CostModelID," AND BudgetID = ",BudgetID,";",sep='')
  #}else{
  #statement<-paste("SELECT ItemID, AcctID FROM ItemInfo WHERE CostModelID = ",CostModelID," AND BudgetID = ",BudgetID," AND ItemMeta1 = ",UserGroup,";",sep='')
  #}

  Items<-dbGetQuery(con,statement)


  #Make this on User group as well to
  statement<-paste("SELECT * FROM Alloc WHERE ItemID IN ",create_IDstring(Items$ItemID),";",sep='')
  Alloc<-dbGetQuery(con,statement)
  #browser()
  if(nrow(Alloc)==0){

    Alloc<-create_empty_df(n.cols=9,col.names=c('ItemID','PercentAppliedToProg','ProgID','AllocComments','AllocLastUpdated','AllocLastUser','byFTE','byFTEDivision','BudgetID'))

  }


  #Get the Fund Info
  statement<-paste("SELECT Fund, FundID FROM FundInfo WHERE CostModelID = ",CostModelID,";",sep='')
  Funds<-dbGetQuery(con,statement)

  #browser()
  #statement<-paste("SELECT * FROM AcctInfo WHERE CostModelID = ",CostModelID,";",sep='')
  statement<-paste("SELECT * FROM
                   ((SELECT * FROM ItemInfo WHERE CostModelID = ",CostModelID," AND BudgetID= ",BudgetID,") items
                   INNER JOIN AcctInfo ON items.AcctID = AcctInfo.AcctID);",sep='')
  acctinfo<-dbGetQuery(con,statement)
  acctinfo<-acctinfo[ , -which(names(acctinfo) %in% c("CostModelID"))]


  if (nrow(acctinfo)==0)(return(NULL))


  statement<-paste("SELECT * FROM ProgBudgetInfo WHERE BudgetID = ",BudgetID,";",sep='')
  ProgBudgetInfo<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM PBBComments;",sep='')
  PBBComments<-dbGetQuery(con,statement)



  #browser()
  if(!is.null(Alloc)){
    #browser()
    allocations<-merge(acctinfo,Alloc[c('ItemID','PercentAppliedToProg','ProgID','AllocComments','AllocLastUpdated','AllocLastUser','byFTE','byFTEDivision')],by='ItemID',all.x=T)
    allocations[is.na(allocations$PercentAppliedToProg),'PercentAppliedToProg']<-0

    #Merge Objects
    allocations<-merge(allocations,Funds,by='FundID',all.x=T)
    allocations<-merge(allocations,Obj1,by='Obj1ID',all.x=T)
    allocations<-merge(allocations,Obj2,by='Obj2ID',all.x=T)
    allocations<-merge(allocations,Obj3,by='Obj3ID',all.x=T)
    allocations<-merge(allocations,Obj4,by='Obj4ID',all.x=T)

    #Merge Divisions
    #browser()
    allocations<-merge(allocations,Div1,by='Div1ID',all.x=T)
    allocations<-merge(allocations,Div2,by='Div2ID',all.x=T)
    allocations<-merge(allocations,Div3,by='Div3ID',all.x=T)
    allocations<-merge(allocations,Div4,by='Div4ID',all.x=T)
    allocations<-merge(allocations,Div5,by='Div5ID',all.x=T)
    allocations<-merge(allocations,Div6,by='Div6ID',all.x=T)
    allocations<-merge(allocations,Div7,by='Div7ID',all.x=T)
    allocations<-merge(allocations,Div8,by='Div8ID',all.x=T)

    allocations<-merge(allocations,ProgInfo[c('ProgID','ProgNum','ProgName','ServiceType')],by='ProgID',all.x=T)

    allocations[,'obj_level_01']<-'Level1'
    allocations[,'obj_level_02']<-'Level2'
    allocations[,'obj_level_03']<-'Level3'
    allocations[,'obj_level_04']<-'Level4'

    #helper function to create and name some object levels
    obj_level_names<-function(allocations,obj='Obj1',objcode='Obj1Code',level='obj_level_01'){

      #Both Code and Name

      allocations[allocations[,obj]!=obj & allocations[,objcode]!=objcode,level]<-paste("(",allocations[allocations[,obj]!=obj & allocations[,objcode]!=objcode,objcode],") ",
                                                                                        allocations[allocations[,obj]!=obj & allocations[,objcode]!=objcode,obj],sep='')
      #Clean back for any blank codes
       allocations[allocations[,obj]!=obj & allocations[,objcode]!=objcode,level]<-allocations[,level]<-gsub("[)]\\s","",allocations[allocations[,obj]!=obj & allocations[,objcode]!=objcode,level])
       allocations[allocations[,obj]!=obj & allocations[,objcode]!=objcode,level]<-allocations[,level]<-gsub("[(]","",allocations[allocations[,obj]!=obj & allocations[,objcode]!=objcode,level])

      # Obj1 Only Used
      allocations[allocations[,obj]!=obj & allocations[,objcode]==objcode,level]<-paste(allocations[allocations[,obj]!=obj & allocations[,objcode]==objcode,obj],sep='')

      # Obj1Code Only Used
      allocations[allocations[,obj]==obj & allocations[,objcode]!=objcode,level]<-paste(allocations[allocations[,obj]==obj & allocations[,objcode]!=objcode,objcode],sep='')

      # This Obj Level not Used
      allocations[allocations[,obj]==obj & allocations[,objcode]==objcode,level]<-'Classifiction Level Not Used'

      return(allocations)
    }


    allocations<-obj_level_names(allocations,obj='Obj1',objcode='Obj1Code',level='obj_level_01')
    allocations<-obj_level_names(allocations,obj='Obj2',objcode='Obj2Code',level='obj_level_02')
    allocations<-obj_level_names(allocations,obj='Obj3',objcode='Obj3Code',level='obj_level_03')
    allocations<-obj_level_names(allocations,obj='Obj4',objcode='Obj4Code',level='obj_level_04')

    #Add comments
    CommentIDs<-merge(ProgBudgetInfo[c('ProgID','BudgetID','PBBCommentID')],PBBComments,by='PBBCommentID')
    allocations<-merge(allocations,CommentIDs[c('ProgID','BudgetID','RXCommentID')],by=c('ProgID','BudgetID'),all.x=T)


    allocations<-allocations[c('ItemID','AcctID','Cost Type','AcctType','ItemMeta1','Obj1','Obj1Code','PercentAppliedToProg','NumberOfItems','TotalCost','ProgID','ProgNum','ProgName','AllocComments','AllocLastUpdated','AllocLastUser','byFTE','byFTEDivision','NameMeta','Fund','AcctCode','obj_level_01','obj_level_02','obj_level_03','obj_level_04','Obj2','Obj3','Obj4','Div1','Div2','Div3','Div4','Div5','Div6','Div7','Div8','ServiceType','BudgetID','RXCommentID')]
    allocations[,'RXCommentID_cp']<- allocations[,'RXCommentID']

    allocations$TotalCost<-as.numeric(allocations$TotalCost)
    allocations$PercentAppliedToProg<-as.numeric(allocations$PercentAppliedToProg)

  }else{allocations<-NULL}

  return(allocations)
}


#' PullAllocations_OpenData
#'
#' Helper Function
#' @param con RMySQL connection
#' @export

PullFinalScores_OpenData<-function(con,BudgetID,CostModelID){


  #browser()
  statement<-paste("SELECT * FROM Scores WHERE CostModelID =",CostModelID,";",sep='')
  Scores<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM ResultSetup WHERE CostModelID =",CostModelID,";",sep='')
  AllResults<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM ResultTypes;",sep='')
  ResultTypes<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM ResultDefs;",sep='')
  Definitions<-dbGetQuery(con,statement)

  statement<-paste("SELECT * FROM ProgInfo;",sep='')
  ProgInfo<-dbGetQuery(con,statement)


  ProgInfo<-ProgInfo_OpenPBB(con,BudgetID)


  #browser()

  #If we have no scores, return NULL, this is unlikely if we are moving through the process properly
  if(nrow(Scores)==0){

    return(NULL)

  }


  #Combine Scores with Results - update the scores with ResultType, Abbr
  PeerReview<-merge(Scores,AllResults[c('ResultID','ResultAbbr','ResultType','Weight')],by='ResultID')

  #Add Programs - Force All programs in case there are missing dept
  PeerReview<-merge(ProgInfo[c('ProgID','ProgDept','ProgName', 'ProgDescription','ServiceType','ItemMeta1','ProgNum')],PeerReview,by='ProgID',all.x=T)

  #Exclude non scored service Types
  Scored.ResultTypes<-ResultTypes[ResultTypes$Scored==1,'ResultType']
  PeerReview<-PeerReview[which(is.element(PeerReview$ServiceType,Scored.ResultTypes)),]

  #We have to include only the scores for the current designation of the program
  PeerReview.results<-PeerReview[PeerReview$ServiceType==PeerReview$ResultType,]
  #But the line above we will lose the BPAs, go back and add those.
  PeerReview.bpas<-PeerReview[PeerReview$ResultType=='BPA',]

  #Combine back
  PeerReview<-rbind(PeerReview.bpas,PeerReview.results)

  #************************************************
  # Same up to here as peer review function
  # Now do the extra stuff for Final Scores
  #************************************************

  #For Final Scores calculated total weighted
  df<-PeerReview[c('ProgDept','ItemMeta1','ProgName','ProgDescription','ScoreDept','ScorePeer','DeptComments','PeerComments','ProgID','ServiceType', 'ResultID','ResultAbbr','ResultType','ScoreID','Weight')]
  #clean this for all result id's not NA's
  df<-df[!is.na(df$ResultID),]

  df[,'ScoreDept']<-df[,'ScoreDept']*df[,'Weight']
  df[,'ScorePeer']<-df[,'ScorePeer']*df[,'Weight']
  #browser()
  Dept<-df[c('ProgID','ResultID','ScoreDept')]
  Peer<-df[c('ProgID','ResultID','ScorePeer')]

  Dept<-dcast(Dept,ProgID ~ ResultID,value.var='ScoreDept')
  Peer<-dcast(Peer,ProgID ~ ResultID,value.var='ScorePeer')

  Dept[,'FinalScore']<-rowSums(Dept[,c(2:ncol(Dept))],na.rm = T)
  Peer[,'FinalScore']<-rowSums(Peer[,c(2:ncol(Peer))],na.rm = T)

  Dept<-Dept[c('ProgID','FinalScore')]
  Peer<-Peer[c('ProgID','FinalScore')]

  #Just get the Prog Info data
  Final_Peer<-PeerReview[!duplicated(PeerReview$ProgID),c('ProgDept','ItemMeta1','ProgNum','ProgName','ProgDescription','DeptComments','PeerComments','ProgID','ServiceType')]
  Final_Peer<-merge(Final_Peer,Peer,by='ProgID')
  Final_Peer<-merge(Final_Peer,ResultTypes[c('ResultType','Q1_Q2','Q2_Q3','Q3_Q4')],by.x='ServiceType',by.y='ResultType')

  Final_Dept<-PeerReview[!duplicated(PeerReview$ProgID),c('ProgDept','ItemMeta1','ProgNum','ProgName','ProgDescription','DeptComments','PeerComments','ProgID','ServiceType')]
  Final_Dept<-merge(Final_Dept,Dept,by='ProgID')
  Final_Dept<-merge(Final_Dept,ResultTypes[c('ResultType','Q1_Q2','Q2_Q3','Q3_Q4')],by.x='ServiceType',by.y='ResultType')

  #*************************************
  # Compute Boundaries - Peer Review
  #*************************************

  #split across service types and calculate the boundaries based on average and stdev
  boundaries<-function(Scores){
    temp<-NULL
    resulttypes<-split(Scores,Scores$ServiceType)
    for(i in 1:length(resulttypes)){
      resulttype<-resulttypes[[i]][1,'ServiceType']

      # Caluculate Max Score and update the scores to out of max score
      score.max<-AllResults[AllResults$ResultType==resulttype,]
      score.max<-rbind(AllResults[AllResults$ResultType=='BPA',],score.max)
      score.max[,'Score']<-4
      score.max[,'Score']<-score.max[,'Score']*score.max[,'Weight']
      score.max<-sum(score.max[,'Score'],na.rm=T)

      rows<-Scores[Scores$ServiceType==resulttype,]
      rows[,'FinalScore']<-rows[,'FinalScore']*100/score.max

      Q2_Q3<-mean(rows[,'FinalScore'],na.rm=T)
      Q1_Q2<-sd(rows[,'FinalScore'],na.rm=T)+Q2_Q3
      Q3_Q4<-Q2_Q3-sd(rows[,'FinalScore'],na.rm=T)


      #if zero then upate to algortithm values, otherwise keep the storered boundaries
      rows[rows$Q1_Q2==0,'Q1_Q2']<-Q1_Q2
      rows[rows$Q2_Q3==0,'Q2_Q3']<-Q2_Q3
      rows[rows$Q3_Q4==0,'Q3_Q4']<-Q3_Q4


      rows[,'Quartile']<-4
      if(!is.na(Q3_Q4))(rows[rows$FinalScore>=rows$Q3_Q4,'Quartile']<-3)
      if(!is.na(Q2_Q3))(rows[rows$FinalScore>=rows$Q2_Q3,'Quartile']<-2)
      if(!is.na(Q1_Q2))(rows[rows$FinalScore>=rows$Q1_Q2,'Quartile']<-1)

      temp<-rbind(temp,rows)
    }
    Scores<-temp
    return(Scores)
  }

  Final_Dept<-boundaries(Final_Dept)
  Final_Peer<-boundaries(Final_Peer)

  Final_Dept[,'Score']<-'Dept'
  Final_Peer[,'Score']<-'Peer'

  Final_Dept<-Final_Dept[order(-Final_Dept$FinalScore),]
  Final_Peer<-Final_Peer[order(-Final_Peer$FinalScore),]

  #Lastly make a column of Quartle difference between department and peer
  Q_Diff<-Final_Peer[c('ProgID','Quartile')]
  colnames(Q_Diff)[2]<-'Peer_Quartile'

  Q_Diff<-merge(Q_Diff,Final_Dept[c('ProgID','Quartile')],by='ProgID')
  colnames(Q_Diff)[3]<-'Dept_Quartile'
  Q_Diff[,'Q_Diff']<-Q_Diff[,'Peer_Quartile']-Q_Diff[,'Dept_Quartile']

  FinalScores<-rbind(Final_Dept,Final_Peer)
  FinalScores<-merge(FinalScores,Q_Diff[c('ProgID','Q_Diff','Peer_Quartile','Dept_Quartile')],by='ProgID')
  #browser()
  data<-list()
  data$FinalScores<-FinalScores
  data$PeerReview<-PeerReview[c('ProgDept','ItemMeta1','ProgName','ProgDescription','ScoreDept','ScorePeer','DeptComments','PeerComments','ProgID','ServiceType', 'ResultID','ResultAbbr','ResultType','ScoreID','Weight')]
  data$ResultTypes<-ResultTypes
  data$AllResults<-AllResults
  return(data)
}

