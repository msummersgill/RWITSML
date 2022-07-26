#' Pull log data for a given log from a WITSML Store
#'
#' @param url url for the WITSML Data Store
#' @param user Username for the authenticating user
#' @param password Username for the authenticating user
#' @param uidWell Unique Identifier for the Parent Well
#' @param uid Unique Identifier for the Log
#' @param curves Character vector containing a list of mnemonic keys for log results required. If blank, defaults to all
#' @param startDateTimeIndex An optional UTC character timestamp specifying the desired start of a time based log
#' @param endDateTimeIndex An optional UTC character timestamp specifying the desired end of a time based log
#' @param startIndex An optional depth value specifying the desired start of a depth based log
#' @param endIndex An optional depth value specifying the desired end of a depth based log
#' @param printProgress Option to print the end of each data chunk received when pagination is required
#' @param verbose if TRUE, verbose output from underlying curl functions will be printed to console. Useful for troubleshooting
#' @return A data.table object with the specified log data
#' @examples
#' RWITSML::GetLogData(url = "https://hub.us.pason.com/hub/witsml/store",
#'                     user = hpidc::GetKeys()$pason$user,
#'                     password = hpidc::GetKeys()$pason$password,
#'                     uidWell = "us_27990298",
#'                     uid = "us_27990298_wb1_log_dfr_time_1s",
#'                     curves = c("TIME","DEPT","BDEP"),
#'                     startDateTimeIndex = "2022-04-01T00:00:00+00:00",
#'                     endDateTimeIndex = "2022-04-01T00:04:59+00:00",
#'                     verbose = FALSE)
#'
#' RWITSML::GetLogData(url = "https://hub.us.pason.com/hub/witsml/store",
#'                     user = hpidc::GetKeys()$pason$user,
#'                     password = hpidc::GetKeys()$pason$password,
#'                     uidWell = "us_27990298",
#'                     uid = "us_27990298_wb1_log_1",
#'                     curves = c("GAM","INCL","AZ"),
#'                     startIndex = 1,
#'                     endIndex = 24645,
#'                     verbose = FALSE)
#'
#' RWITSML::GetLogData(url = "https://witsml.welldata.net/witsml/wmls.asmx",
#'                     user = hpidc::GetKeys()$totco$user,
#'                     password = hpidc::GetKeys()$totco$password,
#'                     uidWell = "9ce20147-e1d4-45db-a8cb-bedb3e50b65e",
#'                     uidWellbore = "9ce20147-e1d4-45db-a8cb-bedb3e50b65e",
#'                     uid = "Depth1",
#'                     curves = c("GAMMA_RAY_DEPTH","GAMMA_RAY"),
#'                     startIndex = NULL,
#'                     endIndex = NULL,
#'                     printProgress = FALSE,
#'                     verbose = FALSE)
#'
#' RWITSML::GetLogData(url = "https://witsml.welldata.net/witsml/wmls.asmx",
#'                     user = hpidc::GetKeys()$totco$user,
#'                     password = hpidc::GetKeys()$totco$password,
#'                     uidWell = "9ce20147-e1d4-45db-a8cb-bedb3e50b65e",
#'                     uidWellbore = "9ce20147-e1d4-45db-a8cb-bedb3e50b65e",
#'                     uid = "Time_120Sec",
#'                     curves = c("BIT_DEPTH","TOT_DPT_MD"),
#'                     startDateTimeIndex = "2022-07-20T00:00:00+00:00",
#'                     endDateTimeIndex = "2022-07-20T23:59:59+00:00",
#'                     printProgress = FALSE,
#'                     verbose = FALSE)
#'

GetLogData <- function(url = NULL,
                       user = NULL,
                       password = NULL,
                       uidWell = NULL,
                       uidWellbore = NULL,
                       uid = NULL,
                       curves = NULL,
                       startDateTimeIndex = NULL,
                       endDateTimeIndex = NULL,
                       startIndex = NULL,
                       endIndex = NULL,
                       printProgress = TRUE,
                       verbose = FALSE) {

  if(is.null(uidWellbore)) uidWellbore <- ""

  H <- GetLogHeader(url = url,
                    user = user,
                    password = password,
                    uidWell = uidWell,
                    uidWellbore = uidWellbore,
                    uid = uid,
                    verbose = FALSE)

  IndexCurveName <- H$indexCurve$text

  ## Key off mnemonics and use the curves requested as key to select relevant rows
  ## TODO: check and see if we can just map everything instead of defaulting to character
  setkey(H$logCurveInfoDT,mnemonic)

  if(is.null(curves)) curves <- H$logCurveInfoDT[,mnemonic]

  curvesAddIndex <- unique(c(IndexCurveName,curves))

  Missing <- setdiff(curvesAddIndex,H$logCurveInfoDT[,mnemonic])
  if(length(Missing) > 0) stop(paste0("Curve(s) ",Missing,collapse = ","," not present in Log Header"))

  Curves <- H$logCurveInfoDT[.(curvesAddIndex)][!is.na(`.attrs`)]

  Curves[, RType := fcase(typeLogData == "date time","POSIXct",
                          typeLogData == "double","numeric",
                          typeLogData == "integer","integer",
                          typeLogData == "unitless","character",
                          default = "character")]


  if(H$indexType == "date time"){
    ## Default to log start/end
    QueryStart <- DataStart <- H$startDateTimeIndex
    QueryEnd <- DataEnd <- H$endDateTimeIndex

    DataStartTS <- as.POSIXct(gsub(":00$","00",DataStart), tz = 'UTC', format = "%FT%H:%M:%OS%z")
    DataEndTS <- as.POSIXct(gsub(":00$","00",DataEnd), tz = 'UTC', format = "%FT%H:%M:%OS%z")

    ## Override start if user wants something after the beginning of the log
    if(!is.null(startDateTimeIndex)){
      startDateTimeIndexTS <- as.POSIXct(gsub(":00$","00",startDateTimeIndex), tz = 'UTC', format = "%FT%H:%M:%OS%z")
      if(startDateTimeIndexTS > DataStartTS) QueryStart <- startDateTimeIndex
    }

    ## Override start if user wants something before the end of the log
    if(!is.null(endDateTimeIndex)) {
      endDateTimeIndexTS <- as.POSIXct(gsub(":00$","00",endDateTimeIndex), tz = 'UTC', format = "%FT%H:%M:%OS%z")
      if(endDateTimeIndexTS < DataEndTS) QueryEnd <- endDateTimeIndex
    }

    ## Generate timestamps for comparison
    QueryStartTS <- as.POSIXct(gsub(":00$","00",QueryStart), tz = 'UTC', format = "%FT%H:%M:%OS%z")
    QueryEndTS <- as.POSIXct(gsub(":00$","00",QueryEnd), tz = 'UTC', format = "%FT%H:%M:%OS%z")

    ## Initialize some variables to loop through pages until we
    Pages <- vector(mode = "list", length = 2^15)
    LastReceivedTS <- as.POSIXct(0, tz ="UTC", origin = "1970-01-01 00:00:00")
    i <- 0L
    KeepGoing <- TRUE

    while(QueryEndTS >= LastReceivedTS  && QueryEndTS > QueryStartTS && KeepGoing){

      i <- i + 1

      Q <- TimeQueryBuilder(uidWell = uidWell,
                            uidWellbore = uidWellbore,
                            uid = uid,
                            curves = Curves[, mnemonic],
                            startDateTimeIndex = QueryStart,
                            endDateTimeIndex = QueryEnd)

      Handle <- curl::new_handle(verbose = verbose)

      curl::handle_setheaders(Handle,
                              "Authorization" = paste0("Basic ",jsonlite::base64_enc(paste0(user,":",password))),
                              "Accept-Encoding" = "gzip",
                              "Accept" = "application/json, text/xml, application/xml, */*",
                              "Content-Type" = "text/xml")

      curl::handle_setopt(Handle,.list = list(customrequest = "POST",
                                              postfields = Q))

      Result <- curl::curl_fetch_memory(url,Handle)

      if(!Result$status_code == 200){
        stop(paste0("HTTP Status Code: ",Result$status_code,
                    "\n\nContent: ",rawToChar(Result$content)))
      }

      Result$content %>%
        rawToChar() %>%
        gsub("&lt;","<", .,fixed = T) %>%
        gsub("&gt;",">", .,fixed = T) -> Raw


      ## In order to parse as valid XML, each
      Start <- stringi::stri_locate_first_fixed(Raw,"<log ")[1,"start"]
      End <- stringi::stri_locate_last_fixed(Raw,"</log>")[1,"end"]
      X <- XML::xmlParseString(substr(Raw,Start,End))
      Root <- XML::xmlRoot(X)

      ## Extract only the log data and read string as a data.table
      ## Use the data in Curves DT to map column names and types for fread to use
      XML::xpathSApply(X, path = "//log/logData/data", XML::xmlValue) %>%
        paste0(., collapse = "\n") %>%
        paste0(., "\n") %>%
        fread(.,
              header = FALSE,
              col.names = Curves[,mnemonic],
              colClasses = Curves[,RType]) -> Pages[[i]]

      ## Update the latest record we've received based on the index column (always included)
      LastReceivedTS <- Pages[[i]][.N][[IndexCurveName]]

      if(printProgress) print(LastReceivedTS)

      ## Define Start time for the next "page" of results using proper timestamp format
      QueryStart <- gsub("00$",":00",format(LastReceivedTS + 1,"%FT%H:%M:%OS%z"))
      QueryStartTS <- as.POSIXct(gsub(":00$","00",QueryStart), tz = 'UTC', format = "%FT%H:%M:%OS%z")
    }


    LD  <- rbindlist(Pages)
    if(!anyDuplicated(LD[[IndexCurveName]]) == 0) warning("Unexpected duplicated rows returned but were removed from result")
    return(unique(LD,by = IndexCurveName))

  } else if(H$indexType == "measured depth") {
    ## Default to log start/end
    QueryStart <- DataStart <- as.numeric(H$startIndex$text)
    QueryEnd <- DataEnd <- as.numeric(H$endIndex$text)


    ## Override start if user wants something after the beginning of the log
    if(!is.null(startIndex)){
      if(startIndex > DataStart) QueryStart <- startIndex
    }

    ## Override start if user wants something before the end of the log
    if(!is.null(endIndex)) {
      if(endIndex < DataEnd) QueryEnd <- endIndex
    }

    ## Initialize some variables to loop through pages until we
    Pages <- vector(mode = "list", length = 2^15)
    LastReceived <- -Inf
    i <- 0L
    KeepGoing <- TRUE

    while(QueryEnd >= LastReceived && QueryEnd > QueryStart && KeepGoing){

      i <- i + 1

      Q <- DepthQueryBuilder(uidWell = uidWell,
                             uidWellbore = uidWellbore,
                             uid = uid,
                             curves = Curves[, mnemonic],
                             startIndex = sprintf("%.4f",QueryStart),
                             endIndex = sprintf("%.4f",QueryEnd))

      Handle <- curl::new_handle(verbose = verbose)

      curl::handle_setheaders(Handle,
                              "Authorization" = paste0("Basic ",jsonlite::base64_enc(paste0(user,":",password))),
                              "Accept-Encoding" = "gzip",
                              "Accept" = "application/json, text/xml, application/xml, */*",
                              "Content-Type" = "text/xml")

      curl::handle_setopt(Handle,.list = list(customrequest = "POST",
                                              postfields = Q))

      Result <- curl::curl_fetch_memory(url,Handle)

      if(!Result$status_code == 200){
        stop(paste0("HTTP Status Code: ",Result$status_code,
                    "\n\nContent: ",rawToChar(Result$content)))
      }

      Result$content %>%
        rawToChar() %>%
        gsub("&lt;","<", .,fixed = T) %>%
        gsub("&gt;",">", .,fixed = T) -> Raw


      ## In order to parse as valid XML, each
      Start <- stringi::stri_locate_first_fixed(Raw,"<log ")[1,"start"]
      End <- stringi::stri_locate_last_fixed(Raw,"</log>")[1,"end"]
      X <- XML::xmlParseString(substr(Raw,Start,End))
      Root <- XML::xmlRoot(X)

      ## Extract only the log data and read string as a data.table
      ## Use the data in Curves DT to map column names and types for fread to use
      try({
        XML::xpathSApply(X, path = "//log/logData/data", XML::xmlValue) %>%
          paste0(., collapse = "\n") %>%
          paste0(., "\n") %>%
          data.table::fread(.,
                            header = FALSE,
                            col.names = Curves[,mnemonic],
                            colClasses = Curves[,RType]) -> Pages[[i]]

      })

      ## Handle some annoying edge cases that arise from WITSML returning index values technically less than requested
      ## TODO: Clean this up to be a little
      if(is.null(Pages[[i]])) KeepGoing <- FALSE
      if(KeepGoing) if(!is.data.table(Pages[[i]])) KeepGoing <- FALSE
      if(KeepGoing) if(nrow(Pages[[i]]) < 1L) KeepGoing <- FALSE
      if(KeepGoing) if(QueryStart > LastReceived) KeepGoing <- FALSE

      ## Update the latest record we've received based on the index column (always included)
      if(KeepGoing) LastReceived <- Pages[[i]][.N][[IndexCurveName]]

      if(printProgress) print(LastReceived)

      ## Define Start time for the next "page" of results using proper timestamp format
      QueryStart <- LastReceived + 0.01
    }

    LD  <- rbindlist(Pages)
    if(!anyDuplicated(LD[[IndexCurveName]]) == 0) warning("Unexpected duplicated rows returned but were removed from result")
    return(unique(LD,by = IndexCurveName))
  }  else {
    stop("Log Index Type Not Supported")
  }

}

TimeQueryBuilder <- function(uidWell = NULL,
                             uidWellbore = NULL,
                             uid = NULL,
                             curves = NULL,
                             startDateTimeIndex = NULL,
                             endDateTimeIndex = NULL) {


  curvesets <- paste0(paste0('
        &lt;logCurveInfo uid=""&gt;
          &lt;mnemonic&gt;',curves,'&lt;/mnemonic&gt;
        &lt;/logCurveInfo&gt;'), collapse = "\n")

  paste0('
  <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/" xmlns:tns="http://www.witsml.org/wsdl/120" xmlns:types="http://www.witsml.org/wsdl/120/encodedTypes" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
    <soap:Body soap:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
      <q1:WMLS_GetFromStore xmlns:q1="http://www.witsml.org/message/120">
        <WMLtypeIn xsi:type="xsd:string">log</WMLtypeIn>
        <QueryIn xsi:type="xsd:string">&lt;logs xmlns="http://www.witsml.org/schemas/131" version="1.3.1.1"&gt;
        &lt;log uidWell="',uidWell,'" uidWellbore="',uidWellbore,'" uid="',uid,'"&gt;
        &lt;nameWell /&gt;
        &lt;nameWellbore /&gt;
        &lt;name /&gt;
        &lt;objectGrowing /&gt;
        &lt;dataRowCount /&gt;
        &lt;serviceCompany /&gt;
        &lt;runNumber /&gt;
        &lt;bhaRunNumber /&gt;
        &lt;pass /&gt;
        &lt;creationDate /&gt;
        &lt;description /&gt;
        &lt;indexType /&gt;
        &lt;startIndex uom="" /&gt;
        &lt;endIndex uom="" /&gt;
        &lt;stepIncrement uom="" numerator="" denominator="" /&gt;
        &lt;startDateTimeIndex&gt; ',startDateTimeIndex,' &lt;/startDateTimeIndex&gt;
        &lt;endDateTimeIndex&gt; ',endDateTimeIndex,' &lt;/endDateTimeIndex&gt;
        &lt;direction /&gt;
        &lt;indexCurve columnIndex="" /&gt;
        ',curvesets,'

        &lt;logData&gt;
        &lt;data /&gt;
        &lt;/logData&gt;
        &lt;/log&gt;
        &lt;/logs&gt;</QueryIn>
        <OptionsIn xsi:type="xsd:string">returnElements=requested;compression=gzip;</OptionsIn>
      </q1:WMLS_GetFromStore>
    </soap:Body>
  </soap:Envelope>
  ')

}


DepthQueryBuilder <- function(uidWell = NULL,
                              uidWellbore = NULL,
                              uid = NULL,
                              curves = NULL,
                              startIndex = NULL,
                              endIndex = NULL) {


  curvesets <- paste0(paste0('
        &lt;logCurveInfo uid=""&gt;
          &lt;mnemonic&gt;',curves,'&lt;/mnemonic&gt;
        &lt;/logCurveInfo&gt;'), collapse = "\n")

  paste0('
  <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/" xmlns:tns="http://www.witsml.org/wsdl/120" xmlns:types="http://www.witsml.org/wsdl/120/encodedTypes" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
    <soap:Body soap:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
      <q1:WMLS_GetFromStore xmlns:q1="http://www.witsml.org/message/120">
        <WMLtypeIn xsi:type="xsd:string">log</WMLtypeIn>
        <QueryIn xsi:type="xsd:string">&lt;logs xmlns="http://www.witsml.org/schemas/131" version="1.3.1.1"&gt;
        &lt;log uidWell="',uidWell,'" uidWellbore="',uidWellbore,'" uid="',uid,'"&gt;
        &lt;nameWell /&gt;
        &lt;nameWellbore /&gt;
        &lt;name /&gt;
        &lt;objectGrowing /&gt;
        &lt;dataRowCount /&gt;
        &lt;serviceCompany /&gt;
        &lt;runNumber /&gt;
        &lt;bhaRunNumber /&gt;
        &lt;pass /&gt;
        &lt;creationDate /&gt;
        &lt;description /&gt;
        &lt;indexType /&gt;
        &lt;startIndex&gt; ',startIndex,' &lt;/startIndex&gt;
        &lt;endIndex&gt; ',endIndex,' &lt;/endIndex&gt;
        &lt;stepIncrement uom="" numerator="" denominator="" /&gt;
        &lt;startDateTimeIndex /&gt;
        &lt;endDateTimeIndex /&gt;
        &lt;direction /&gt;
        &lt;indexCurve columnIndex="" /&gt;
        ',curvesets,'

        &lt;logData&gt;
        &lt;data /&gt;
        &lt;/logData&gt;
        &lt;/log&gt;
        &lt;/logs&gt;</QueryIn>
        <OptionsIn xsi:type="xsd:string">returnElements=requested;compression=gzip;</OptionsIn>
      </q1:WMLS_GetFromStore>
    </soap:Body>
  </soap:Envelope>
  ')

}
