#' @title TMSBrianApp data check
#' @description Check data for minimum requirement for TMSBrainApp analysis.
#' @param TMSdata A data frame to be checked for eligibility.
#' @return Returns a data frame if minimum requirement test is passed.
#' @export
#' @keywords
#' @examples
#' \dontrun{
#' TMSapp.check(TMSdata)
#' }
#'


TMSapp.check <- function(TMSdata){

  create_rep <- function(TMSdata){
    TMSdata <- TMSdata%>% as_tibble()%>%
      group_by(ID)%>%
      mutate(Rep=1)%>%
      mutate(Rep=cumsum(Rep))%>%
      ungroup()

    aux <- cbind(select(TMSdata,ID,Rep),select(TMSdata,-c(ID,Rep)))%>% as_tibble
    return(aux)
  }

  Aux <- list()
  if(is.data.frame(TMSdata)){
    TMSdata <- as_tibble(TMSdata)
  #
  re_name <- c("Assoc..Target","Loc..X","Loc..Y","Loc..Z","EMG.Data.1")
  #
  data.names <- names(TMSdata)
  data.names <- make.names(data.names)
  #
  re_names <- NULL
  for (k in 1:length(data.names)) {
    if(data.names[k]==re_name[1]){
    re_names[k]="ID"
    }else if(data.names[k]==re_name[2]){
      re_names[k]="x"
    }else if(data.names[k]==re_name[3]){
      re_names[k]="y"
    }else if(data.names[k]==re_name[4]){
      re_names[k]="z"
    }else if(data.names[k]==re_name[5]){
      re_names[k]="EMG_Data"
    }else{
      re_names[k]=data.names[k]
    }

  }
  #
  names(TMSdata) <- re_names
  #

  data.names <- names(TMSdata)

  compulsory_var <- c("ID","Rep","x","y","z","EMG_Data")
  Test.avail <- which((compulsory_var %in% data.names))
  Test <- Test.avail %>% length()
  if(Test==6){
    Aux$data <- TMSdata
    Aux$test_result <- "Data Passed Test"
  }else if(length(Test>0)){

    ##########
    Test.avail.aux <- NULL
      for (i in 1:6) {
        Test.avail.aux[i] <-  sum(Test.avail==i)==1
      }
    ##########
    Aux$test$ID <- ifelse(!Test.avail.aux[1],"'ID' not available","Available")
    Aux$test$Rep <- ifelse(!Test.avail.aux[2],"Rep not available","Available")
    Aux$test$x <- ifelse(!Test.avail.aux[3],"'x' Coordinate not available" ,"Available")
    Aux$test$y <- ifelse(!Test.avail.aux[4],"'y' Coordinate not available","Available")
    Aux$test$z <- ifelse(!Test.avail.aux[5],"'z' Coordinate not available","Available")
    Aux$test$EMG_Data <- ifelse(!Test.avail.aux[6],"'EMG_Data' not available","Available")


    #########
    if(Aux$test$Rep=="Rep not available" & Aux$test$ID=="Available"){
      Aux$data <- create_rep(TMSdata)
      Aux$test$Rep <- "Available"
    }
    ##########
    Aux$test_result<- if_else(
      Aux$test$ID =="Available"&
        Aux$test$Rep=="Available"&
        Aux$test$x=="Available"&
        Aux$test$y=="Available"&
        Aux$test$z=="Available"&
        Aux$test$EMG_Data=="Available",
      "Data Passed Test",
      "Data Failed Test. See 'test' for reason ")
    ##########
   }

  }else{
    Aux$test <- "Not a dataframe"
 }


  return(Aux)
}

