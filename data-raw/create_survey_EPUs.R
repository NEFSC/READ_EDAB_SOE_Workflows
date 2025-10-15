#' Designate survey strata to EPUs
#' 
#' This designation is used across several idicators
#' It will now be exported as part of this package

create_survey_EPUs <- function(save_data = FALSE) {
  
  MAB <- c(1010:1080, 1100:1120, 1600:1750, 3010:3450, 3470, 3500, 3510)
  GB  <- c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550)
  GOM <- c(1220, 1240, 1260:1290, 1360:1400, 3560:3830)
  SS  <- c(1300:1352, 3840:3990)
  
  df1 <- data.frame(EPU = rep("MAB",length(MAB)),STRATUM = MAB)
  df2 <- data.frame(EPU = rep("GB",length(GB)),STRATUM = GB)
  df3 <- data.frame(EPU = rep("GOM",length(GOM)),STRATUM = GOM)
  df4 <- data.frame(EPU = rep("SS",length(SS)),STRATUM = SS)
  epu_strata <- rbind(df1,df2,df3,df4)
 
  if (save_data) {
    usethis::use_data(epu_strata, overwrite = TRUE)
  } else {
    return(epu_strata) 
  }
  
}