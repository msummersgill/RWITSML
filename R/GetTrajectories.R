#' Pull a list of trajectories for a given well from a WITSML Store
#'
#' @param url url for the WITSML Data Store
#' @param user Username for the authenticating user
#' @param password Username for the authenticating user
#' @param uidWell Unique Identifier for the Parent Well
#' @param uidWellbore Unique Identifier for the Associated Wellbore. Not required for Pason, required for MD-Totco
#' @param uid Unique Identifier for the trajectory within a given Well Context. Required to pull actual survey data from MD-Totco
#' @param verbose if TRUE, verbose output from underlying curl functions will be printed to console. Useful for troubleshooting
#' @return A nested list with all trajectories for a given well
#' @examples
#' RWITSML::GetTrajectories(url = "https://hub.us.pason.com/hub/witsml/store",
#'                          user = hpidc::GetKeys()$pason$user,
#'                          password = hpidc::GetKeys()$pason$password,
#'                          uidWell = "us_28147307",
#'                          verbose = FALSE)
#'
#' RWITSML::GetTrajectories(url = "https://witsml.welldata.net/witsml/wmls.asmx",
#'                          user = hpidc::GetKeys()$totco$user,
#'                          password = hpidc::GetKeys()$totco$password,
#'                          uidWell = "9ce20147-e1d4-45db-a8cb-bedb3e50b65e",
#'                          uidWellbore = "9ce20147-e1d4-45db-a8cb-bedb3e50b65e",
#'                          uid = "Connect-Surveys",
#'                          verbose = FALSE)

GetTrajectories <- function(url = NULL,
                            user = NULL,
                            password = NULL,
                            uidWell = NULL,
                            uidWellbore = NULL,
                            uid = NULL,
                            verbose = FALSE) {

  if(is.null(uidWellbore)) uidWellbore <- ""
  if(is.null(uid)) uid <- ""
  Q <- paste0('
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/" xmlns:tns="http://www.witsml.org/wsdl/120" xmlns:types="http://www.witsml.org/wsdl/120/encodedTypes" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
  <soap:Body soap:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
    <q1:WMLS_GetFromStore xmlns:q1="http://www.witsml.org/message/120">
      <WMLtypeIn xsi:type="xsd:string">trajectory</WMLtypeIn>
      <QueryIn xsi:type="xsd:string">&lt;trajectorys xmlns="http://www.witsml.org/schemas/131" version="1.3.1.1"&gt;
      &lt;trajectory uidWell="',uidWell,'" uidWellbore="',uidWellbore,'" uid="',uid,'"&gt;
        &lt;nameWell /&gt;
            &lt;nameWellbore /&gt;
            &lt;name /&gt;
            &lt;objectGrowing /&gt;
            &lt;parentTrajectory&gt;
              &lt;trajectoryReference uidRef="" /&gt;
              &lt;wellboreParent uidRef="" /&gt;
            &lt;/parentTrajectory&gt;
            &lt;dTimTrajStart /&gt;
            &lt;dTimTrajEnd /&gt;
            &lt;mdMn uom="" datum="" /&gt;
            &lt;mdMx uom="" datum="" /&gt;
            &lt;serviceCompany /&gt;
            &lt;magDeclUsed uom="" /&gt;
            &lt;gridCorUsed uom="" /&gt;
            &lt;aziVertSect uom="" /&gt;
            &lt;dispNsVertSectOrig uom="" /&gt;
            &lt;dispEwVertSectOrig uom="" /&gt;
            &lt;definitive /&gt;
            &lt;memory /&gt;
            &lt;finalTraj /&gt;
            &lt;aziRef /&gt;
            &lt;trajectoryStation uid=""&gt;
              &lt;target uidRef="" /&gt;
              &lt;dTimStn /&gt;
              &lt;typeTrajStation /&gt;
              &lt;typeSurveyTool /&gt;
              &lt;md uom="" datum="" /&gt;
              &lt;tvd uom="" datum="" /&gt;
              &lt;incl uom="" /&gt;
              &lt;azi uom="" /&gt;
              &lt;mtf uom="" /&gt;
              &lt;gtf uom="" /&gt;
              &lt;dispNs uom="" /&gt;
              &lt;dispEw uom="" /&gt;
              &lt;vertSect uom="" /&gt;
              &lt;dls uom="" /&gt;
              &lt;rateTurn uom="" /&gt;
              &lt;rateBuild uom="" /&gt;
              &lt;mdDelta uom="" datum="" /&gt;
              &lt;tvdDelta uom="" datum="" /&gt;
              &lt;modelToolError /&gt;
              &lt;gravTotalUncert uom="" /&gt;
              &lt;dipAngleUncert uom="" /&gt;
              &lt;magTotalUncert uom="" /&gt;
              &lt;gravAccelCorUsed /&gt;
              &lt;magXAxialCorUsed /&gt;
              &lt;sagCorUsed /&gt;
              &lt;magDrlstrCorUsed /&gt;
              &lt;gravTotalFieldReference uom="" /&gt;
              &lt;magTotalFieldReference uom="" /&gt;
              &lt;magDipAngleReference uom="" /&gt;
              &lt;magModelUsed /&gt;
              &lt;magModelValid /&gt;
              &lt;geoModelUsed /&gt;
              &lt;statusTrajStation /&gt;
              &lt;rawData&gt;
                &lt;gravAxialRaw uom="" /&gt;
                &lt;gravTran1Raw uom="" /&gt;
                &lt;gravTran2Raw uom="" /&gt;
                &lt;magAxialRaw uom="" /&gt;
                &lt;magTran1Raw uom="" /&gt;
                &lt;magTran2Raw uom="" /&gt;
              &lt;/rawData&gt;
              &lt;corUsed&gt;
                &lt;gravAxialAccelCor uom="" /&gt;
                &lt;gravTran1AccelCor uom="" /&gt;
                &lt;gravTran2AccelCor uom="" /&gt;
                &lt;magAxialDrlstrCor uom="" /&gt;
                &lt;magTran1DrlstrCor uom="" /&gt;
                &lt;magTran2DrlstrCor uom="" /&gt;
                &lt;sagIncCor uom="" /&gt;
                &lt;sagAziCor uom="" /&gt;
                &lt;stnMagDeclUsed uom="" /&gt;
                &lt;stnGridCorUsed uom="" /&gt;
                &lt;dirSensorOffset uom="" /&gt;
              &lt;/corUsed&gt;
              &lt;valid&gt;
                &lt;magTotalFieldCalc uom="" /&gt;
                &lt;magDipAngleCalc uom="" /&gt;
                &lt;gravTotalFieldCalc uom="" /&gt;
              &lt;/valid&gt;
              &lt;matrixCov&gt;
                &lt;varianceNN uom="" /&gt;
                &lt;varianceNE uom="" /&gt;
                &lt;varianceNVert uom="" /&gt;
                &lt;varianceEE uom="" /&gt;
                &lt;varianceEVert uom="" /&gt;
                &lt;varianceVertVert uom="" /&gt;
                &lt;biasN uom="" /&gt;
                &lt;biasE uom="" /&gt;
                &lt;biasVert uom="" /&gt;
              &lt;/matrixCov&gt;
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
              &lt;sourceStation&gt;
                &lt;stationReference /&gt;
                &lt;trajectoryParent uidRef="" /&gt;
                &lt;wellboreParent uidRef="" /&gt;
              &lt;/sourceStation&gt;
              &lt;commonData&gt;
                &lt;sourceName /&gt;
                &lt;dTimCreation /&gt;
                &lt;dTimLastChange /&gt;
                &lt;itemState /&gt;
                &lt;comments /&gt;
              &lt;/commonData&gt;
            &lt;/trajectoryStation&gt;
            &lt;commonData&gt;
              &lt;sourceName /&gt;
              &lt;dTimCreation /&gt;
              &lt;dTimLastChange /&gt;
              &lt;itemState /&gt;
              &lt;comments /&gt;
            &lt;/commonData&gt;
            &lt;customData /&gt;
          &lt;/trajectory&gt;
        &lt;/trajectorys&gt;</QueryIn>
      <OptionsIn xsi:type="xsd:string">returnElements=requested</OptionsIn>
    </q1:WMLS_GetFromStore>
  </soap:Body>
</soap:Envelope>
  ')

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

  Start <- stringi::stri_locate_first_fixed(Raw,"<trajectory ")[1,"start"]
  End <- stringi::stri_locate_last_fixed(Raw,"</trajectory>")[1,"end"]

  X <- XML::xmlParseString(substr(Raw,Start,End))
  Root <- XML::xmlRoot(X)

  TrajectoryList <- XML::xmlToList(Root,addAttributes = TRUE)

  return(TrajectoryList)
}
