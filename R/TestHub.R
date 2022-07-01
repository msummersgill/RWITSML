#' Test connection and capabilities of a WITSML Data Store
#'
#' @param url url for the WITSML Data Store
#' @param user Username for the authenticating user
#' @param password Username for the authenticating user
#' @return A summary of server capabilities
#' @examples
#' RWITSML::TestHub(url = "https://hub.us.pason.com/hub/witsml/store",
#'                  user = hpidc::GetKeys()$pason$user,
#'                  password = hpidc::GetKeys()$pason$password,
#'                  verbose = TRUE)
#'

TestHub <- function(url = NULL,
                    user = NULL,
                    password = NULL,
                    verbose = TRUE) {

  ## TODO: point to a package installed text file for some of these queries
  Q <- '
  <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/" xmlns:tns="http://www.witsml.org/wsdl/120" xmlns:types="http://www.witsml.org/wsdl/120/encodedTypes" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
  <soap:Body soap:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
    <q1:WMLS_GetCap xmlns:q1="http://www.witsml.org/message/120">
        <OptionsIn xsi:type="xsd:string">dataVersion=1.3.1.1</OptionsIn>
      </q1:WMLS_GetCap>
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

  Start <- stringi::stri_locate_first_fixed(Raw,"<capServer ")[1,"start"]
  End <- stringi::stri_locate_last_fixed(Raw,"</capServer>")[1,"end"]

  X <- XML::xmlParseString(substr(Raw,Start,End))
  Root <- XML::xmlRoot(X)

  XML::xmlToList(Root)[["capServer"]]
}

