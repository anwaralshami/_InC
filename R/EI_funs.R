
#remotes::install_git("https://github.com/shinyTree/shinyTree",quiet = T,)
library(shinyTree)

treeToJSON <- function(tree, keepRoot = FALSE, topLevelSlots = c("default", "all"),createNewId = TRUE,pretty = FALSE) {
  ## match against "default"/"all", if this returns an error we take topLevelSlots as is
  ## i.e. a vector of names to keep
  if (!requireNamespace("data.tree", quietly = TRUE)) {
    msg <- paste("library", sQuote("data.tree"), "cannot be loaded. Try to run",
                 sQuote("install.packages(\"data.tree\")"))
    stop(msg, domain = NA)
  }
  nodesToKeep <- list(default = c("id", "text", "icon", "state",
                                  "li_attr", "a_attr", "type"),
                      all     = NULL)
  topLevelSlots <- tryCatch(nodesToKeep[[match.arg(topLevelSlots)]],
                            error = function(e) topLevelSlots)
  node_to_list <- function(node, 
                           node_name = NULL) {
    fields <- mget(node$fields, node)
    NOK <- sapply(fields, function(slot) !is.atomic(slot) && !is.list(slot))
    if (any(NOK)) {
      msg <- sprintf(ngettext(length(which(NOK)),
                              "unsupported slot of type %s at position %s",
                              "unsupported slots of types %s at positions %s"),
                     paste0(dQuote(sapply(fields[NOK], typeof)),
                            collapse = ", "),
                     paste0(sQuote(names(fields)[NOK]),
                            collapse = ", "))
      warning(msg,
              domain = NA)
      fields[NOK] <- NULL
    }
    if (is.null(fields$text)) {
      fields$text <- if(!is.null(fields$name)) fields$name else node_name
    }
    fields$icon <- fixIconName(fields$icon)
    if (!is.null(fields$state)) {
      valid_states <- c("opened", "disabled", "selected", "loaded")
      states_template <- stats::setNames(rep(list(FALSE), length(valid_states)),
                                         valid_states)
      NOK <- !names(fields$state) %in% valid_states
      if (any(NOK)) {
        msg <- sprintf(ngettext(length(which(NOK)),
                                "invalid state %s",
                                "invalid states %s"),
                       paste0(dQuote(names(fields$state)[NOK]),
                              collapse = ", "))
        warning(msg,
                domain = NA)
      }
      states_template[names(fields$state[!NOK])] <- fields$state[!NOK]
      fields$state <- states_template
    }
    if (is.null(topLevelSlots)) {
      slots_to_move <- character(0)
    } else {
      slots_to_move <- names(fields)[!names(fields) %in% topLevelSlots]
    }
    data_slot <- fields[slots_to_move]
    if (length(data_slot)) {
      fields$data <- data_slot
      fields[slots_to_move] <- NULL
    }
    if (!is.null(node$children)) {
      ## purrr::imap would make code cleaner but did not want to add another dependency
      ## unname needed to create an JSON array as opposed to an JSON object
      fields$children <- unname(lapply(names(node$children), 
                                       function(i) node_to_list(node$children[[i]],
                                                                i)))
    }
    fields
  }
  ## clone tree as we do not want to alter the original tree
  tree <- data.tree::Clone(tree)
  nodes <- data.tree::Traverse(tree, filterFun = data.tree::isNotRoot)
  old_ids <- data.tree::Get(nodes, "id")
  if (createNewId) {
    if (any(!is.na(old_ids))) {
      warning(paste("slot",
                    dQuote("id"), 
                    "will be stored in",
                    dQuote("id.orig")),
              domain = NA)
      data.tree::Set(nodes, id.orig = old_ids)
    }
    new_ids <- seq_along(nodes)
  } else {
    if (any(is.na(old_ids)) ||
        any(duplicated(old_ids))) {
      warning(paste("old ids are invalid (duplicated values or NA),",
                    "creating new ids"),
              domain = NA)
      new_ids <- seq_along(nodes)
    } else {
      new_ids <- old_ids
    }
  }
  
  data.tree::Set(nodes, id = new_ids)
  treeList <- node_to_list(tree)
  if (!keepRoot) {
    ## to prune off the root node return the first children list
    treeList <- treeList$children
  }
  ## use as.character b/c updateTree needs an unparsed JSON string, as 
  ## the parsing is done in shinyTree.js
  as.character(jsonlite::toJSON(treeList, 
                                auto_unbox = TRUE, 
                                pretty = pretty))
}

fixIconName <- function(icon){
  ## - 'yes' branch of 'if' covers everything which should not be changed
  ##   e.g. "/images/ball.jpg" or "fa fa-file
  ## - 'no' branch of 'if' covers all cases which need to be changed:
  ##   use regex (str_match) to capture groups: 
  ##     * group 1 is either 'glyphicon', 'fa' or 'NA' (if not present)
  ##     * group 2 is the rest wihtout a potential dash '-'
  ##     * if group 1 is empty set it to 'fa'
  ##     * paste the pieces together
  res <- ifelse(grepl("[/\\]|(glyphicon|fa) \\1-", icon), 
                icon, 
                {
                  parts <- stringr::str_match(icon, "(glyphicon|fa)*-*(\\S+)")
                  parts[, 2] <- ifelse(is.na(parts[, 2]), "fa", parts[, 2])
                  paste(parts[, 2], paste(parts[, 2], parts[, 3], sep = "-")) 
                })
  ## if NULL was given as parameter res will be length zero
  if (!length(res)) {
    NULL
  } else {
    res
  }
}

tweakInputTable <- function (inp, years){
  #inp is ActivityDataInputTable()
  #years is input$range
  inp %>%
    select(L1,L2,L1name,L2name,Sector,Type, Abatement,Region, Technology,Fuel, activityUnit)%>%
    rename(Unit = activityUnit) ->inp
  r1<-(ncol(inp)+1)
  r2<-(ncol(inp)+1+years[2]-years[1])
  inp[,r1:r2]<-0
  colnames(inp)[r1:r2]<-as.character(years[1]:years[2])
  return(inp)
  
}

cleanEID <- function(EFs,AUs,EUs){
  require(dplyr)
  EFs%>%
    #unify unit writing
    mutate(Unit = as.character(EFs$Unit))%>%
    mutate(Unit = ifelse(Unit == "g km-1","g/Kg", Unit))%>%
    mutate(Unit = ifelse(Unit == "g km-1 vehicle-1","g/km vehicle -1", Unit))%>%
    mutate(Unit = ifelse(Unit == "g NH3 kg-1 N applied","g NH3/kg N applied", Unit))%>%
    mutate(Unit = ifelse(Unit == "kg ha-1","kg/ha", Unit))%>%
    mutate(Unit = ifelse(Unit == "kg NH3 capita -1","kg NH3/capita", Unit))%>%
    mutate(Unit = ifelse(Unit == "kg NH3 kg–1 waste N applied","kg NH3/kg waste N applied", Unit))%>%
    mutate(Unit = ifelse(Unit == "kg NH3-N per kg N in feedstock","kg NH3-N/kg N in feedstock", Unit))%>%
    mutate(Unit = ifelse(Unit == "kg N2O-N (kg TAN entering store)–1","kg N2O-N/kg TAN entering store", Unit))%>%
    mutate(Unit = ifelse(Unit == "kg NMVOC kg dm–1 ha–1","kg NMVOC/kg dm ha–1", Unit))%>%
    mutate(Unit = ifelse(Unit == "kg NO2 capita–1","kg NO2/ capita", Unit))%>%
    mutate(Unit = ifelse(Unit == "kg NH3 capita -1","kg NH3/ capita", Unit))%>%
    #mutate(Type = ifelse(Type == "Tier 2 Emission Factor","Tier 2 emission factor", Type))%>%
    #mutate(Abatement = ifelse(as.character(Abatement) == "",NA, Abatement))%>%
    #get the big sectors
    mutate(L1 = unlist(lapply(strsplit(as.character(EFs$NFR),split = "\\."), function(l){unlist(l)[1]})))%>%
    mutate(L2 = unlist(lapply(strsplit(as.character(EFs$NFR),split = "\\."), function(l){unlist(l)[2]})))%>%
    
    #get the raw activity unit
    mutate(activityUNit = unlist(lapply(strsplit(unlist(lapply(strsplit(as.character(EFs$Unit),"/"), 
                                                               function(l){unlist(l)[2]}))," "), 
                                        function(l){unlist(l)[1]}))) %>%
    
    #get the raw activity unit description
    mutate(activityUNitdesc = unlist(lapply(strsplit(unlist(lapply(strsplit(as.character(EFs$Unit),"/"), 
                                                                   function(l){unlist(l)[2]}))," "), 
                                            function(l){
                                              paste(unlist(l)[-1],collapse = " ")
                                            }
    )
    ))%>%
    #join activity units in measures of weight
    mutate(activityUNit = ifelse(activityUNit == "(g" ,"g", activityUNit)) %>%
    mutate(activityUNit = ifelse(activityUNit == "tonne" | 
                                   activityUNit == "tonnes"| 
                                   activityUNit == "MG"|
                                   activityUNit == "t"|
                                   activityUNit == "ton","Mg", activityUNit))%>%
    #add multipliers to normalize activity rates to small number of units
    right_join(AUs)->EFs2
  EFs2 %>%
    # get emission unit
    mutate(emissionUNit = unlist(lapply(strsplit(unlist(lapply(strsplit(as.character(EFs2$Unit),"/"), 
                                                               function(l){unlist(l)[1]}))," "), 
                                        function(l){
                                          paste(unlist(l)[1],collapse = " ")
                                        }
    )
    ))%>%
    # get emission unit description
    mutate(emissionUNitdesc = unlist(lapply(strsplit(unlist(lapply(strsplit(as.character(EFs2$Unit),"/"), 
                                                                   function(l){unlist(l)[1]}))," "), 
                                            function(l){
                                              paste(unlist(l),collapse = " ")
                                            }
    )
    ))%>%
    right_join(EUs) %>%
    #column renaming
    select(-activityUNit)%>%
    rename(activityUnit = unifiedUnit)%>%
    select(-emissionUNit)%>%
    rename(emissionUnit = emissionUnitUnified)%>%
    mutate_each(funs(empty_as_na))->EFs2
  return(EFs2)
}

empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse((as.character(x)=="" | as.character(x)=="Na"| as.character(x)=="NA"), NA, x)
}

createInputFileName <- function(country,years){
  #country is input$country
  #years is input$range
  return(paste(country,"_",years[1],"-",years[2],".csv",collapse = "",sep = ""))
}

computeEmission <- function(dat,EFs2){
  #dat is the uploaded input file
  #EFs2 is the cleanEID EMEP db
  dat%>%
    mutate(L1=as.character(L1))%>%
    mutate(L2=as.character(L2))%>%
    mutate(Type=as.character(Type))%>%
    mutate(Fuel=as.character(Fuel))%>%
    mutate(Abatement=as.character(Abatement))%>%
    mutate(Technology=as.character(Technology))%>%
    mutate(Sector=as.character(Sector))%>%
    mutate(Region=as.character(Region))%>%
    select(-Unit)-> dat
  EFs2 %>%
    mutate(L1=as.character(L1))%>%
    mutate(L2=as.character(L2))%>%
    mutate(Type=as.character(Type))%>%
    mutate(Fuel=as.character(Fuel))%>%
    mutate(Abatement=as.character(Abatement))%>%
    mutate(Technology=as.character(Technology))%>%
    mutate(Sector=as.character(Sector))%>%
    mutate(Region=as.character(Region))%>%
    inner_join(dat,by=c("L1","L2","Type","Fuel","Abatement","Technology","Sector","Region"))%>%
    mutate_at(colnames(dat)[grepl("X....",colnames(dat))], 
              function(x)(as.numeric(x)*as.numeric(.$Value)*as.numeric(.$activityMultiplier)/as.numeric(.$emissionMultiplier)))->test
  return(test)
}

summarizeEmissionAcrossPollutants <- function(df,sumBy,filterBy){
  #df is emissionValues()
  return(
    df %>%
      group_by(Pollutant, emissionUnit,!!as.name(sumBy))%>%
      summarize_at(colnames(df)[grepl("X....",colnames(df))], sum)%>%
      filter(!!as.name(sumBy) == filterBy)
  )
  
}

summarizeEmissionBySpecificPollutant <- function(df,sumBy,filterBy){
  #df is emissionValues()
  
  return(
    df %>%
      group_by(Pollutant,emissionUnit,!!as.name(sumBy))%>%
      summarize_at(colnames(df)[grepl("X....",colnames(df))], sum)%>%
      filter(Pollutant == filterBy)
  )
  
}

summarizeEmissionByPollutants <- function(df,sumBy){
  #df is emissionValues()
  return(
    df %>%
      group_by(Pollutant,emissionUnit,!!as.name(sumBy))%>%
      summarize_at(colnames(df)[grepl("X....",colnames(df))], sum)
  )
  
}