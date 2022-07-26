#' Pull a list of all wells from a WITSML Store
#'
#' @param url url for the WITSML Data Store
#' @param user Username for the authenticating user
#' @param password Username for the authenticating user
#' @param verbose if TRUE, verbose output from underlying curl functions will be printed to console. Useful for troubleshooting
#' @return A data.table with all wells available
#' @examples
#' RWITSML::GetWells(url = "https://hub.us.pason.com/hub/witsml/store",
#'                   user = hpidc::GetKeys()$pason$user,
#'                   password = hpidc::GetKeys()$pason$password,
#'                   verbose = FALSE)
#'
#' RWITSML::GetWells(url = "https://witsml.welldata.net/witsml/wmls.asmx",
#'                   user = hpidc::GetKeys()$totco$user,
#'                   password = hpidc::GetKeys()$totco$password,
#'                   verbose = FALSE)
#'

GetWells <- function(url = NULL,
                     user = NULL,
                     password = NULL,
                     verbose = FALSE) {
  Q <- '
    <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/" xmlns:tns="http://www.witsml.org/wsdl/120" xmlns:types="http://www.witsml.org/wsdl/120/encodedTypes" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
  <soap:Body soap:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
    <q1:WMLS_GetFromStore xmlns:q1="http://www.witsml.org/message/120">
      <WMLtypeIn xsi:type="xsd:string">well</WMLtypeIn>
      <QueryIn xsi:type="xsd:string">&lt;wells xmlns="http://www.witsml.org/schemas/131" version="1.3.1.1"&gt;
        &lt;well uid=""&gt;
      &lt;name /&gt;
      &lt;nameLegal /&gt;
      &lt;numLicense /&gt;
      &lt;numGovt /&gt;
      &lt;dTimLicense /&gt;
      &lt;field /&gt;
      &lt;country /&gt;
      &lt;state /&gt;
      &lt;county /&gt;
      &lt;region /&gt;
      &lt;district /&gt;
      &lt;block /&gt;
      &lt;timeZone /&gt;
      &lt;operator /&gt;
      &lt;operatorDiv /&gt;
      &lt;pcInterest uom="" /&gt;
      &lt;numAPI /&gt;
      &lt;statusWell /&gt;
      &lt;purposeWell /&gt;
      &lt;fluidWell /&gt;
      &lt;directionWell /&gt;
      &lt;dTimSpud /&gt;
      &lt;dTimPa /&gt;
      &lt;wellheadElevation uom="" datum="" /&gt;
      &lt;wellDatum defaultMeasuredDepth="" defaultVerticalDepth="" defaultElevation="" uid=""&gt;
        &lt;name /&gt;
        &lt;code /&gt;
        &lt;datumName namingSystem="" code="" /&gt;
        &lt;kind /&gt;
        &lt;wellbore&gt;
          &lt;wellboreReference uidRef="" /&gt;
          &lt;wellParent uidRef="" /&gt;
        &lt;/wellbore&gt;
        &lt;rig&gt;
          &lt;rigReference uidRef="" /&gt;
          &lt;wellboreParent uidRef="" /&gt;
          &lt;wellParent uidRef="" /&gt;
        &lt;/rig&gt;
        &lt;elevation uom="" datum="" /&gt;
        &lt;measuredDepth uom="" datum="" /&gt;
        &lt;comment /&gt;
      &lt;/wellDatum&gt;
      &lt;groundElevation uom="" datum="" /&gt;
      &lt;waterDepth uom="" datum="" /&gt;
      &lt;wellLocation uid=""&gt;
        &lt;wellCRS uidRef="" /&gt;
        &lt;latitude uom="" /&gt;
        &lt;longitude uom="" /&gt;
        &lt;easting uom="" /&gt;
        &lt;northing uom="" /&gt;
        &lt;westing uom="" /&gt;
        &lt;southing uom="" /&gt;
        &lt;projectedX uom="" /&gt;
        &lt;projectedY uom="" /&gt;
        &lt;localX uom="" /&gt;
        &lt;localY uom="" /&gt;
        &lt;original /&gt;
        &lt;description /&gt;
      &lt;/wellLocation&gt;
      &lt;referencePoint uid=""&gt;
        &lt;name /&gt;
        &lt;type /&gt;
        &lt;elevation uom="" datum="" /&gt;
        &lt;measuredDepth uom="" datum="" /&gt;
        &lt;location uid=""&gt;
          &lt;wellCRS uidRef="" /&gt;
          &lt;latitude uom="" /&gt;
          &lt;longitude uom="" /&gt;
          &lt;easting uom="" /&gt;
          &lt;northing uom="" /&gt;
          &lt;westing uom="" /&gt;
          &lt;southing uom="" /&gt;
          &lt;projectedX uom="" /&gt;
          &lt;projectedY uom="" /&gt;
          &lt;localX uom="" /&gt;
          &lt;localY uom="" /&gt;
          &lt;original /&gt;
          &lt;description /&gt;
        &lt;/location&gt;
        &lt;description /&gt;
      &lt;/referencePoint&gt;
      &lt;wellCRS uid=""&gt;
        &lt;name /&gt;
        &lt;mapProjection&gt;
          &lt;nameCRS namingSystem="" code="" /&gt;
          &lt;projectionCode /&gt;
          &lt;projectedFrom uidRef="" /&gt;
          &lt;stdParallel1 uom="" /&gt;
          &lt;stdParallel2 uom="" /&gt;
          &lt;centralMeridian uom="" /&gt;
          &lt;originLatitude uom="" /&gt;
          &lt;originLongitude uom="" /&gt;
          &lt;latitude1 uom="" /&gt;
          &lt;longitude1 uom="" /&gt;
          &lt;latitude2 uom="" /&gt;
          &lt;longitude2 uom="" /&gt;
          &lt;latitudeForScale uom="" /&gt;
          &lt;longitudeForScale uom="" /&gt;
          &lt;trueScaleLatitude uom="" /&gt;
          &lt;spheroidRadius uom="" /&gt;
          &lt;scaleFactor /&gt;
          &lt;methodVariant /&gt;
          &lt;perspectiveHeight uom="" /&gt;
          &lt;zone /&gt;
          &lt;NADType /&gt;
          &lt;falseEasting uom="" /&gt;
          &lt;falseNorthing uom="" /&gt;
          &lt;bearing uom="" /&gt;
          &lt;hemisphere /&gt;
          &lt;description /&gt;
          &lt;parameter index="" name="" uom="" description="" /&gt;
        &lt;/mapProjection&gt;
        &lt;geographic&gt;
          &lt;nameCRS namingSystem="" code="" /&gt;
          &lt;geodeticDatumCode /&gt;
          &lt;xTranslation uom="" /&gt;
          &lt;yTranslation uom="" /&gt;
          &lt;zTranslation uom="" /&gt;
          &lt;xRotation uom="" /&gt;
          &lt;yRotation uom="" /&gt;
          &lt;zRotation uom="" /&gt;
          &lt;scaleFactor /&gt;
          &lt;ellipsoidCode /&gt;
          &lt;ellipsoidSemiMajorAxis uom="" /&gt;
          &lt;ellipsoidInverseFlattening /&gt;
        &lt;/geographic&gt;
        &lt;localCRS&gt;
          &lt;usesWellAsOrigin /&gt;
          &lt;origin uidRef="" /&gt;
          &lt;originDescription /&gt;
          &lt;yAxisAzimuth uom="" northDirection="" /&gt;
          &lt;yAxisDescription /&gt;
          &lt;xRotationCounterClockwise /&gt;
        &lt;/localCRS&gt;
        &lt;description /&gt;
      &lt;/wellCRS&gt;
      &lt;commonData&gt;
        &lt;sourceName /&gt;
        &lt;dTimCreation /&gt;
        &lt;dTimLastChange /&gt;
        &lt;itemState /&gt;
        &lt;comments /&gt;
      &lt;/commonData&gt;
      &lt;customData /&gt;
    &lt;/well&gt;
    &lt;/wells&gt;</QueryIn>
      <OptionsIn xsi:type="xsd:string"></OptionsIn>
    </q1:WMLS_GetFromStore>
  </soap:Body>
</soap:Envelope>
  '

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

  Start <- stringi::stri_locate_first_fixed(Raw,"<well ")[1,"start"]
  End <- stringi::stri_locate_last_fixed(Raw,"</well>")[1,"end"]

  X <- XML::xmlParseString(substr(Raw,Start,End))
  Root <- XML::xmlRoot(X)

  WellList <- XML::xmlToList(Root,addAttributes = TRUE)

  Wells <- data.table(uid = sapply(WellList, `[[`, ".attrs"),
                      name = sapply(WellList, `[[`, "name"),
                      field = sapply(WellList, `[[`, "field"),
                      country = sapply(WellList, `[[`, "country"),
                      state = sapply(WellList, `[[`, "state"),
                      county = sapply(WellList, `[[`, "county"),
                      timeZone = sapply(WellList, `[[`, "timeZone"),
                      operator = sapply(WellList, `[[`, "operator"),
                      numAPI = sapply(WellList, `[[`, "numAPI"),
                      statusWell = sapply(WellList, `[[`, "statusWell"),
                      dTimSpud = sapply(WellList, `[[`, "dTimSpud"),
                      dTimPa = sapply(WellList, `[[`, "dTimPa")
  )


  return(Wells)
}
