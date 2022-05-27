#helper functions.

## module related function

create_ttf_table <- function(num_arm){
  df <- data.frame(ID   = seq(1:num_arm),
                   Treatment = "",
                   Subgroup = "",
                   Pathology = "",
                   N  = "",
                   No.Event = "",
                   Est.Median = "",
                   EM.95CIL = "",
                   EM.95CIU = "",
                   Fup.Median = "",
                   HR = "",
                   HR.95CIL = "",
                   HR.95CIU = "",
                   KM.Data = "",
                   IPD.Data = "",
                   stringsAsFactors=FALSE)
  df
}

create_cat_table <- function(num_arm){
  dfc <- data.frame(ID   = seq(1:num_arm),
                    Treatment = "",
                    Subgroup = "",
                    Pathology = "",
                    N = "",
                    stringsAsFactors=FALSE)
  dfc
}

create_num_table <- function(num_arm){
  dfn <- data.frame(ID   = seq(1:num_arm),
                    Treatment = "",
                    Subgroup = "",
                    Pathology = "",
                    N = "",
                    Mean = '',
                    Sd = '',
                    Median = '',
                    Range = '',
                    stringsAsFactors=FALSE)
  dfn
}

make_final_table <- function(dat,ns){
  if(!is.null(dat)){
  col_id = which(!colnames(dat) %in% c("Treatment","Subgroup","Pathology","ID"))
  colnames(dat)[col_id] <- ns(colnames(dat)[col_id])
  dat
  }
}

adjust_row <- function(dtable,n_arms){
  if (!is.na(n_arms)){
    current_row = nrow(dtable)
    if(current_row > n_arms){
      dtable <- dtable[seq(1:n_arms),]}
    else if(current_row  < n_arms){
      add_row = n_arms - current_row
      add_df <- matrix("",add_row,ncol(dtable))
      colnames(add_df) <- colnames(dtable)
      dtable <- rbind(dtable,add_df)
      dtable$ID <- seq(1:nrow(dtable))
    }
  }
  dtable
}

adjust_col <- function(dtable,cat_level_name){

  if (!is.na(cat_level_name)){
    current_names <- NULL
    if(ncol(dtable) > 5){
      current_names = colnames(dtable)[6:ncol(dtable)]
    }

    if(substr(cat_level_name, nchar(cat_level_name), nchar(cat_level_name)) %in% c(";", ",")){
      cat_level_name = substr(cat_level_name, 1, nchar(cat_level_name)-1)
    }
    level_name <-  stringr::str_split(cat_level_name ,";|\\,")[[1]]

    if(!setequal(current_names, level_name)){
      common_names = intersect(current_names, level_name) #keep
      add_names = setdiff(level_name,current_names)     #add
      remove_names = setdiff(current_names, level_name)   #remove
      # remove
      if(length(remove_names)> 0){
        dtable  <- dtable[,-which(colnames(dtable) %in% remove_names )]
      }
      #add
      for(s_name in add_names){
        if(s_name != ""){
        dtable[s_name] = rep("",nrow(dtable))
        }
      }
    }}
  dtable
}



### ttf util

table2survplot <- function(ttf_table, excel_input){

  if(!is.null(ttf_table)){
    if(use_km_data(excel_input, ttf_table$KM.Data)){
      try(make_survplot(make_df_kmdata(ttf_table)))
    }else if(use_median(excel_input, ttf_table$KM.Data, ttf_table$Est.Median)){
      make_survplot(make_df_median(ttf_table))
    }
  }

}

make_survplot <- function(plot_data){
  ggplot(plot_data, aes(timess, valuess))  + theme_minimal() +
    geom_line(aes(colour = factor(treatment)), size = 1)
}

use_median <- function(excel_input,excel_name, ttf_median){
  (is.null(excel_input) | all(excel_name == ""))& any(ttf_median != "")
}

make_df_median <- function(ttf_table){
  exponential_rates = log(2)/as.numeric(ttf_table$Est.Median[ttf_table$Est.Median != ""])
  time <- seq(from = 0, to = 45, by = 0.1)
  dff = NULL
  for (ii in seq(1,length(exponential_rates))){
    dff = rbind(dff,data.frame(treatment = paste0(ttf_table$Treatment[ii],"_",ttf_table$Subgroup[ii]), timess = time, valuess = 1-pexp(time,exponential_rates[ii])))
  }
  dff
}

use_km_data <- function(excel_input,excel_name){
  !is.null(excel_input) & any(excel_name != "")
}

make_df_kmdata <- function(ttf_table){
  plot_id <- which(ttf_table$KM.Data != "")
  dff = NULL
  for (iid in plot_id){
    dff = rbind(dff,data.frame(treatment = paste0(ttf_table$Treatment[iid],"_",ttf_table$Subgroup[iid]), timess = ttf_table$km_data[[iid]]$time, valuess = ttf_table$km_data[[iid]]$surv))
  }
  dff
}


add_km <- function(ttf_table,km_input_files,unit_time = "month"){
  ttf_table <- ttf_table[-which(colnames(ttf_table) %in% c("km_data"))]
  if(!is.null(km_input_files)){
    excl <- km_input_files
    excl$km_data = sapply(excl$datapath,function(x) list(km_clean_month(read.csv(x),unit_time)))
    ttf_table <- dplyr::left_join(ttf_table, excl[c("name","km_data")], by = c("KM.Data" = "name"))
  }
  ttf_table
}

km_clean_month <- function(sdat,unit_time = "month"){
  sdat <- km_clean(sdat)
  sdat$time = km_clean_month_vector(sdat$time, unit_time)
  sdat
}

km_clean<-function(sdat){
  sdat<-sdat[sdat$time>0,]
  sdat<-sdat[sdat$surv<1,]
  time<-c(0,sdat$time)
  surv<-c(1,sdat$surv)
  surv<-surv[order(time)]
  time<-time[order(time)]
  while(sum(diff(surv)>0)>0){
    surv[c(-1,diff(surv))>0]<-surv[(c(-1,diff(surv))>0)[-1]]
  }
  return(data.frame(time=time,surv=surv))
}

km_clean_month_vector <- function(timev,unit_time = "month"){
  time_dic <- data.frame(unit  = c("month","year","week","day"),
                         scale = c(1, 12, 0.23, 0.03289))

  timev*time_dic$scale[time_dic$unit == unit_time]
}

update_median <- function(ttf_table){
  if(!is.null(ttf_table)){
    ttf_table %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Est.Median = ifelse(KM.Data != "" & Est.Median == "",
                                              get_ttf_median(km_data),
                                              Est.Median
      ))}}

get_ttf_median<- function(km_data){
  try(if(!is.null(km_data)){
    minid = max(which(km_data$surv - 0.5 > 0),rm.na = TRUE)
    as.character(round(km_data$time[minid],2))
  }else{
    ""
  })
}

make_risk_table <- function(img_input){

  if (!is.null(img_input)){
    risk_table <- get_risktable_from_fig(img_input)
  }else{
    risk_table <- as.data.frame("Can't get the risk table")
  }

  if(ncol(risk_table) < 2){
    colnames(risk_table) <- "Value (Separate numbers by blank space)"
    risk_table$Treatment <- ""
    risk_table$Subgroup <- ""
    risk_table$Pathology <- ""
    risk_table <- risk_table[c("Treatment","Subgroup","Pathology","Value (Separate numbers by blank space)")]
    risk_table$Treatment[1] = "Time (in original unit)"
  }
  risk_table
}

#### risk table

get_risktable_from_fig <- function(img_input){
  data <- magick::image_read(img_input) %>%
    # image_transparent("white", fuzz=50) %>%
    # image_background("white") %>%
    # image_negate() %>%
    # image_morphology(method = "Thinning", kernel = "Rectangle:20x1+0+0^<") %>%
    # image_negate() %>%
    magick::image_ocr()

  # some wrangling
  datalist <- data %>%
    stringi::stri_split(fixed = "\n") %>%
    purrr::map(~ stringi::stri_split(str = ., fixed = "‘")) %>%
    .[[1]]

  datalist2 <- sapply(datalist, function(x) stringr::str_split(x," ")[[1]])

  trisk_id <- which(sapply(datalist2, is.sequential))  ## what if trisk_id > 1 . use other rulse, the seq should start from 0.

  if(length(trisk_id) == 1){
    trisk <- as.numeric(datalist2[[trisk_id]])  #trisk <- seq(0,max(as.numeric(datalist2[[trisk_id]])),1)
    trisk_len <- length(trisk)  ## the number of time points.
    nrisk_ids <- which(sapply(datalist2[c(trisk_id+1:length(datalist2))], function(x) length(x)>5)) + trisk_id
    risk_table  <- as.data.frame(rbind(paste0(c("time",trisk),collapse = " "),  do.call(rbind, datalist[nrisk_ids])))

  }else{
    risk_table <- as.data.frame("Can't get the risk table")
    }
  risk_table
}

is.sequential <- function(x){
  if(length(x)< 3){
    return(FALSE)
  }
  x <- suppressWarnings(as.numeric(x))
  all(diff(x) == diff(x)[1])
}

####  IPD
clean_risktable <- function(risk_table){
  risk_table <- risk_table[which(risk_table$`Value (Separate numbers by blank space)` != ""),]
  risk_table$value =lapply(risk_table$`Value (Separate numbers by blank space)`,clean_risktable_vector )
  risk_table
}

clean_risktable_vector <- function(xv){
  sep_xv <- as.numeric(trimws(stringr::str_split(xv," ")[[1]]))
  sep_xv[which(!is.na(sep_xv))]
}

add_ipd_upload <- function(final_data,ipd_input_files){
  final_data <- final_data[-which(colnames(final_data) %in% c("ipd","ipd_type"))]
  
  if(!is.null(ipd_input_files)){
    excl <- ipd_input_files
    excl$ipd = sapply(excl$datapath,function(x) list(read.csv(x)))
    final_data <- dplyr::left_join(final_data, excl[c("name","ipd")], by = c("IPD.Data" = "name"))
  }
  final_data$ipd_type <- ""
  final_data$ipd_type[which((final_data$IPD.Data != ""))] <- "Upload"
  final_data
}

add_ipd_risktable <- function(final_data ,risk_table, unit_time){
  #colnames(final_data) <- c(colnames(create_ttf_table(1)),"km_data")

  final_data$ipd <- vector(mode = "list", length = nrow(final_data))
  if(!is.null(risk_table)){
    risk_table <- clean_risktable(risk_table)
    risk_time <- km_clean_month_vector(risk_table$value[1][[1]], unit_time)
    risk_table <- risk_table[2:nrow(risk_table),]
    final_data <- dplyr::left_join(final_data, risk_table, by = c("Treatment", "Subgroup","Pathology"))

    seq_id <- which(!sapply(final_data$value, is.null))

    final_data$ipd[seq_id] =  lapply(seq_id, function(x) {
      get_ipd_risktable(risk_time, final_data[x,]$value[[1]], final_data[x,]$km_data[[1]], arm.id= final_data[x,]$Treatment, subgroup.id = final_data[x,]$Subgroup, Pathology.id = final_data[x,]$Pathology)
    })
    final_data$ipd_type[which(!sapply(final_data$ipd,is.null))] <- "risk_table"
    final_data <- final_data[-which(colnames(final_data) %in% c("Value (Separate numbers by blank space)", "value"  ))]
  }

  seq_median_id <- which(sapply(final_data$ipd,is.null))
  final_data$ipd[seq_median_id]  = lapply(seq_median_id, function(x) {
    get_ipd_median(final_data[x,]$Est.Median, final_data[x,]$No.Event, final_data[x,]$N, arm.id= final_data[x,]$Treatment, subgroup.id = final_data[x,]$Subgroup)
  })
  final_data$ipd_type[seq_median_id] <- "median"
  final_data
}

get_ipd_median <- function(median_surv, num_event, num_patient, arm.id = 1, subgroup.id  = 1, Pathology.id = 1){
  median_surv <- as.numeric(median_surv)
  num_event <- as.numeric(num_event)
  num_patient <- as.numeric(num_patient)
  if(!is.na(median_surv) & !is.na(num_patient)){
    if(is.na(num_event)){ num_event <- num_patient * 0.7}

  exponential_rates = log(2)/as.numeric(median_surv)

  lifetimes <- rexp(num_patient, rate = exponential_rates)
  maxEnrollTime <- 0.85 / exponential_rates
  enroll    <- (1:num_patient) / num_patient * maxEnrollTime
  endTime   <- lifetimes + enroll
  censtime  <- sort(endTime)[num_event]
  time      <- pmin(endTime, censtime) - enroll
  status    <- as.numeric(censtime >= endTime) # 0 censor 1 death
  dfData   <- data.frame(time = time, status = status, arm = arm.id, subgroup = subgroup.id, Pathology = Pathology.id)
  return(dfData)
  }
  else{
    return(NULL)
  }
}

get_ipd_risktable <- function(risk_time, risk_number, km_data, arm.id = 1, subgroup.id  = 1, Pathology.id= 1){
  digizeit <- data.frame(k=1:nrow(km_data),Tk=km_data$time,Sk=km_data$surv)
  pub.risk <- riskdat(risk_time, risk_number,digizeit)
  trial_arm <- guyot_ipd(digizeit,pub.risk,tot.events="NA", arm.id=arm.id, subgroup.id = subgroup.id, Pathology = Pathology.id)
  trial_arm$IPD
}

riskdat<-function(trisk,nrisk,digizeit){
  ### the last number can't be 0
  if(nrisk[length(nrisk)] ==  0){
    nrisk = nrisk[1:length(nrisk)-1]
    trisk = trisk[1:length(trisk)-1]
  }

  pub.risk<-data.frame(i=1:length(nrisk),trisk=trisk,nrisk=nrisk)
  mat1<-matrix(digizeit$Tk,nrow=length(digizeit$Tk),ncol=length(nrisk))
  mat2<-matrix(trisk,nrow=length(digizeit$Tk),ncol=length(nrisk),byrow=TRUE)
  pub.risk$lower<-colSums(mat1<mat2)+1
  pub.risk$upper<-c(colSums(mat1<mat2)[-1],nrow(digizeit))
  return(pub.risk)
}

guyot_ipd<-function(digizeit,pub.risk,tot.events="NA",arm.id=1, subgroup.id  = 1, Pathology.id = 1){
  #Read in survival times read by digizeit
  t.S<-digizeit$Tk
  S<-digizeit$Sk
  #Read in published numbers at risk, n.risk, at time, t.risk, lower and upper
  # indexes for time interval
  t.risk<-pub.risk$trisk
  lower<-pub.risk$lower
  upper<-pub.risk$upper
  n.risk<-pub.risk$nrisk
  n.int<-length(n.risk)
  n.t<- upper[n.int]
  #Initialise vectors
  arm<-rep(arm.id,n.risk[1])
  subgroup <-rep(subgroup.id,n.risk[1])
  Pathology <- rep(Pathology.id,n.risk[1])
  n.censor<- rep(0,(n.int-1))
  n.hat<-rep(n.risk[1]+1,n.t)
  cen<-rep(0,n.t)
  d<-rep(0,n.t)
  KM.hat<-rep(1,n.t)
  last.i<-rep(1,n.int)
  sumdL<-0
  if (n.int > 1){
    #Time intervals 1,...,(n.int-1)
    for (i in 1:(n.int-1)){
      #First approximation of no. censored on interval i
      n.censor[i]<- round(n.risk[i]*S[lower[i+1]]/S[lower[i]]- n.risk[i+1])
      #Adjust tot. no. censored until n.hat = n.risk at start of interval (i+1)
      while((n.hat[lower[i+1]]>n.risk[i+1])||((n.hat[lower[i+1]]<n.risk[i+1])&&(n.censor[i]>0))){
        if (n.censor[i]<=0){
          cen[lower[i]:upper[i]]<-0
          n.censor[i]<-0
        }
        if (n.censor[i]>0){
          cen.t<-rep(0,n.censor[i])
          for (j in 1:n.censor[i]){
            cen.t[j]<- t.S[lower[i]] +
              j*(t.S[lower[(i+1)]]-t.S[lower[i]])/(n.censor[i]+1)
          }
          #Distribute censored observations evenly over time. Find no. censored on each time interval.
          cen[lower[i]:upper[i]]<-hist(cen.t,breaks=t.S[lower[i]:lower[(i+1)]],
                                       plot=F)$counts
        }
        #Find no. events and no. at risk on each interval to agree with K-M estimates read from curves
        n.hat[lower[i]]<-n.risk[i]
        last<-last.i[i]
        for (k in lower[i]:upper[i]){
          if (i==1 & k==lower[i]){
            d[k]<-0
            KM.hat[k]<-1
          }
          else {
            d[k]<-round(n.hat[k]*(1-(S[k]/KM.hat[last])))
            KM.hat[k]<-KM.hat[last]*(1-(d[k]/n.hat[k]))
          }
          n.hat[k+1]<-n.hat[k]-d[k]-cen[k]
          if (d[k] != 0) last<-k
        }
        n.censor[i]<- n.censor[i]+(n.hat[lower[i+1]]-n.risk[i+1])
      }
      if (n.hat[lower[i+1]]<n.risk[i+1]) n.risk[i+1]<-n.hat[lower[i+1]]
      last.i[(i+1)]<-last
    }
  }

  #Time interval n.int.
  if (n.int>1){
    #Assume same censor rate as average over previous time intervals.
    n.censor[n.int]<- min(round(sum(n.censor[1:(n.int-1)])*(t.S[upper[n.int]]-
                                                              t.S[lower[n.int]])/(t.S[upper[(n.int-1)]]-t.S[lower[1]])), n.risk[n.int])
  }
  if (n.int==1){n.censor[n.int]<-0}
  if (n.censor[n.int] <= 0){
    cen[lower[n.int]:(upper[n.int]-1)]<-0
    n.censor[n.int]<-0
  }
  if (n.censor[n.int]>0){
    cen.t<-rep(0,n.censor[n.int])
    for (j in 1:n.censor[n.int]){
      cen.t[j]<- t.S[lower[n.int]] +
        j*(t.S[upper[n.int]]-t.S[lower[n.int]])/(n.censor[n.int]+1)
    }
    cen[lower[n.int]:(upper[n.int]-1)]<-hist(cen.t,breaks=t.S[lower[n.int]:upper[n.int]],
                                             plot=F)$counts
  }
  #Find no. events and no. at risk on each interval to agree with K-M estimates read from curves
  n.hat[lower[n.int]]<-n.risk[n.int]
  last<-last.i[n.int]
  for (k in lower[n.int]:upper[n.int]){
    if(KM.hat[last] !=0){
      d[k]<-round(n.hat[k]*(1-(S[k]/KM.hat[last])))} else {d[k]<-0}
    KM.hat[k]<-KM.hat[last]*(1-(d[k]/n.hat[k]))
    n.hat[k+1]<-n.hat[k]-d[k]-cen[k]
    #No. at risk cannot be negative
    if (n.hat[k+1] < 0) {
      n.hat[k+1]<-0
      cen[k]<-n.hat[k] - d[k]
    }
    if (d[k] != 0) last<-k
  }
  #If total no. of events reported, adjust no. censored so that total no. of events agrees.
  if (tot.events != "NA"){
    if (n.int>1){
      sumdL<-sum(d[1:upper[(n.int-1)]])
      #If total no. events already too big, then set events and censoring = 0 on all further time intervals
      if (sumdL >= tot.events){
        d[lower[n.int]:upper[n.int]]<- rep(0,(upper[n.int]-lower[n.int]+1))
        cen[lower[n.int]:(upper[n.int]-1)]<- rep(0,(upper[n.int]-lower[n.int]))
        n.hat[(lower[n.int]+1):(upper[n.int]+1)]<- rep(n.risk[n.int],(upper[n.int]+1-lower[n.int]))
      }
    }
    #Otherwise adjust no. censored to give correct total no. events
    if ((sumdL < tot.events)|| (n.int==1)){
      sumd<-sum(d[1:upper[n.int]])
      while ((sumd > tot.events)||((sumd< tot.events)&&(n.censor[n.int]>0))){
        n.censor[n.int]<- n.censor[n.int] + (sumd - tot.events)
        if (n.censor[n.int]<=0){
          cen[lower[n.int]:(upper[n.int]-1)]<-0
          n.censor[n.int]<-0
        }
        if (n.censor[n.int]>0){
          cen.t<-rep(0,n.censor[n.int])
          for (j in 1:n.censor[n.int]){
            cen.t[j]<- t.S[lower[n.int]] +
              j*(t.S[upper[n.int]]-t.S[lower[n.int]])/(n.censor[n.int]+1)
          }
          cen[lower[n.int]:(upper[n.int]-1)]<-hist(cen.t,breaks=t.S[lower[n.int]:upper[n.int]],
                                                   plot=F)$counts
        }
        n.hat[lower[n.int]]<-n.risk[n.int]
        last<-last.i[n.int]
        for (k in lower[n.int]:upper[n.int]){
          d[k]<-round(n.hat[k]*(1-(S[k]/KM.hat[last])))
          KM.hat[k]<-KM.hat[last]*(1-(d[k]/n.hat[k]))
          if (k != upper[n.int]){
            n.hat[k+1]<-n.hat[k]-d[k]-cen[k]
            #No. at risk cannot be negative
            if (n.hat[k+1] < 0) {
              n.hat[k+1]<-0
              cen[k]<-n.hat[k] - d[k]
            }
          }
          if (d[k] != 0) last<-k
        }
        sumd<- sum(d[1:upper[n.int]])
      }
    }
  }
  KMdata<-matrix(c(t.S,n.hat[1:n.t],d,cen),ncol=4,byrow=F)
  ### Now form IPD ###
  #Initialise vectors
  t.IPD<-rep(t.S[n.t],n.risk[1])
  event.IPD<-rep(0,n.risk[1])
  #Write event time and event indicator (=1) for each event, as separate row in t.IPD and event.IPD
  k=1
  for (j in 1:n.t){
    if(d[j]!=0){
      t.IPD[k:(k+d[j]-1)]<- rep(t.S[j],d[j])
      event.IPD[k:(k+d[j]-1)]<- rep(1,d[j])
      k<-k+d[j]
    }
  }
  #Write censor time and event indicator (=0) for each censor, as separate row in t.IPD and event.IPD
  for (j in 1:(n.t-1)){
    if(cen[j]!=0){
      t.IPD[k:(k+cen[j]-1)]<- rep(((t.S[j]+t.S[j+1])/2),cen[j])
      event.IPD[k:(k+cen[j]-1)]<- rep(0,cen[j])
      k<-k+cen[j]
    }
  }
  #Output IPD
  IPD<-data.frame(time=t.IPD,status=event.IPD,arm=arm, subgroup = subgroup, Pathology = Pathology)
  return(list(KMdata=KMdata,IPD=IPD))
}

make_ipd_figure <- function(ipd_table){
ipd_table$status <- as.integer(ipd_table$status)
ipd_table$tr = interaction(ipd_table$arm,ipd_table$subgroup)
fit <- survival::survfit(survival::Surv(time, status) ~ tr, data = ipd_table)
survminer::ggsurvplot(fit, data = ipd_table, legend = "bottom", risk.table = TRUE,xlab = "Time (Month)")
}

## merge outcome entry by treatment and subgroup

merge_outcome <- function(all_outcome, tab_df, ns){
  tab_df <- tab_df[-which(tab_df$Treatment == ""),]
  tab_df <- make_final_table(tab_df[-1],ns)

  if(is.null(all_outcome)){
    tab_df
  }else{
    col_replace = which(colnames(all_outcome) %in% colnames(tab_df[-c(1,2,3)]))
    if(length(col_replace) > 0){
      all_outcome <- all_outcome[-col_replace]
    }
    dplyr::full_join(all_outcome, tab_df, by =c("Treatment","Subgroup","Pathology"))

  }
}

make_trsub_list <- function(dt1, dt2){
  if(is.null(dt1)&is.null(dt2)){
    in_tr <- " "
    in_sub <- " "
    in_path <- " "
  }else if(is.null(dt1)){
    in_tr<- dt2$Treatment
    in_sub <-  dt2$Subgroup
    in_path <- dt2$Pathology
  }else if(is.null(dt2)){
    in_tr <- dt1$Treatment
    in_sub <-  dt1$Subgroup
    in_path <- dt1$Pathology
  }else{
    in_tr <- union(dt1$Treatment, dt2$Treatment)
    in_sub <- union(dt1$Subgroup, dt2$Subgroup)
    in_path <- union(dt1$Pathology, dt2$Pathology) 
  }

  if(length(unique(in_tr))<2){in_tr = c(" ",in_tr)}
  if(length(unique(in_sub))<2){in_sub = c(" ",in_sub)}
  out_dt <- list(Treatment = sort(unique(in_tr)),Subgroup = sort(unique(in_sub)))
  out_dt
}


##### merge data

merge_trial_info <- function(trial_outcome, input_info){
  table_n_rows <- nrow(trial_outcome)
  PID = input_info$PID
  PaperNickName = input_info$PaperNickName
  Type = as.character(input_info$Type)
  Phase = as.character(input_info$Phase)
  TrLine = as.character(input_info$TrLine)
  NCT = as.character(input_info$NCT)
  Year = as.character(input_info$Year)
  first_author = input_info$FirstAuthor

  common_value <- c(""  ,PID  ,PaperNickName,first_author ,NCT  ,Year   ,Type   ,Phase  ,TrLine)
  common_name  <- c("ID","PID","PaperName"  ,"FirstAuthor","NCT","Year" , "Type","Phase","TrLine")
  common_value_matrix <- as.data.frame(matrix(common_value, nrow=table_n_rows, ncol=length(common_value), byrow=TRUE))
  colnames(common_value_matrix) <- common_name

  study_table <- cbind(common_value_matrix,trial_outcome)
  other_name<- colnames(study_table)[which(!colnames(study_table) %in% common_name)]

  study_table$trial_info <- paste0("Y",study_table$Year, study_table$Type,"P",study_table$Phase,"TL",study_table$TrLine,study_table$Treatment,"-",study_table$Subgroup)
  study_table <- study_table[,c(common_name,"trial_info",other_name)]
  colnames(study_table) <- make.names(colnames(study_table))
  study_table
}

##### summary table
make_pie <- function(var_value, var_name){
  colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(114,147,203)', 'rgb(144,103,167)', 'rgb(171,104,87)')

  fig <- plot_ly(labels = ~names(table(var_value)), values = ~table(var_value), type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent+value',
                 showlegend = FALSE,
                 insidetextfont = list(color = '#FFFFFF'),
                 marker = list(colors = colors,
                               line = list(color = '#FFFFFF', width = 1)),
                 height = 200)
  m <- list(
    l = 10,
    r = 10,
    b = 0,
    t = 30,
    pad = 0
  )
  fig <- fig %>% layout(title = var_name,
                        #autosize = F,
                        margin = m,
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

  fig
}
#make_pie(ddf$Treatment,"Treatment")
# p1 <- make_pie(ddf$Treatment,"Treatment")
# p2 <- make_pie(ddf$Subgroup,"Subgroup")
# p3 <- make_pie(ddf$Phase,"Phase")


