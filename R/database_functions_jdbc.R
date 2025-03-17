# Simplified version of furdeb::access_dbconnection
# Requires driver(s) downloaded from:
# https://sourceforge.net/projects/ucanaccess/

#' Title
#'
#' @param access_db_path
#' @param ...
#' @param driver_db_dir
#' @param bin_dir
#'
#' @returns
#' @export
#'
#' @examples
access_db_java_connection <- function(
    access_db_path,
    ...,
    driver_db_dir = getwd(),
    bin_dir = "UCanAccess-5.0.1.bin") {

  # access DB path verification ----
  if (!is.character(access_db_path)) {
    stop("invalid \"access_db_path\" argument.\n")
  }

  # initializing Access JDBC driver ----
  class_path <- c(
    file.path(driver_db_dir, bin_dir, "lib", "jackcess-3.0.1.jar"),
    file.path(driver_db_dir, bin_dir, "lib", "hsqldb-2.5.0.jar"),
    file.path(driver_db_dir, bin_dir, "lib", "commons-logging-1.2.jar"),
    file.path(driver_db_dir, bin_dir, "lib", "commons-lang3-3.8.1.jar"),
    file.path(driver_db_dir, bin_dir, "ucanaccess-5.0.1.jar"))

  access_jdbc_driver <- RJDBC::JDBC(
    driverClass = "net.ucanaccess.jdbc.UcanaccessDriver",
    classPath = class_path)

  # connection to Access database ----
  con_string <- paste0("jdbc:ucanaccess://", access_db_path, ...)
  access_jdbc_connection <- RJDBC::dbConnect(access_jdbc_driver, con_string)
  return(access_jdbc_connection)
}

# Open encrypted database? ----
# https://ucanaccess.sourceforge.net/site.html
# https://sourceforge.net/projects/ucanaccess/
# https://sourceforge.net/projects/jackcess/
# https://sourceforge.net/projects/jackcessencrypt/
# https://jackcessencrypt.sourceforge.io/
# https://www.bouncycastle.org/java.html
# https://github.com/gordthompson/ucanaccess-generic-opener
# https://www.webucator.com/article/how-to-create-a-jar-file-in-java/
# This describes how to open a ULS-protected mdb-file
# https://stackoverflow.com/questions/54236316/connect-to-a-ms-access-database-secured-with-a-workgroup-security-file-mdw-usi
# https://github.com/gordthompson/ucanaccess-generic-opener

# https://stackoverflow.com/questions/31429939/how-to-connect-ucanaccess-to-an-access-database-encrypted-with-a-database-passwo
# https://stackoverflow.com/questions/74806419/java-ucanaccess-problem-connecting-to-a-database-with-password

access_encrypted_db_java_connection <- function(
    access_db_path,
    user = "", password,
    driver_db_dir = getwd()) {

  # access DB path verification ----
  if (! is.character(access_db_path)) {
    stop("invalid \"access_db_path\" argument.\n")
  }
  # initializing Access JDBC driver ----
  # bin_dir <- "UCanAccess-5.0.1.bin"
  bin_dir <- "UCanAccess-3.0.3.1.bin"

  class_path <- c(
    file.path(driver_db_dir, bin_dir, "lib", "jackcess-2.1.3.jar"),
    #file.path(driver_db_dir, bin_dir, "lib", "jackcess-3.0.1.jar"),
    #file.path(driver_db_dir, bin_dir, "lib", "jackcess-4.0.5.jar"),
    file.path(driver_db_dir, bin_dir, "lib", "hsqldb.jar"),
    #file.path(driver_db_dir, bin_dir, "lib", "hsqldb-2.5.0.jar"),
    file.path(driver_db_dir, bin_dir, "lib", "commons-logging-1.1.1.jar"),
    #file.path(driver_db_dir, bin_dir, "lib", "commons-logging-1.2.jar"),
    file.path(driver_db_dir, bin_dir, "lib", "commons-lang-2.6.jar"),
    #file.path(driver_db_dir, bin_dir, "lib", "commons-lang3-3.8.1.jar"),
    file.path(driver_db_dir, bin_dir, "lib", "jackcess-encrypt-2.1.1.jar"),
    #file.path(driver_db_dir, bin_dir, "lib", "jackcess-encrypt-3.0.0.jar"),
    #file.path(driver_db_dir, bin_dir, "lib", "jackcess-encrypt-4.0.2.jar"),
    file.path(driver_db_dir, bin_dir, "lib", "bcprov-jdk15on-1.54.jar"),
    #file.path(driver_db_dir, bin_dir, "lib", "bcprov-jdk15to18-173.jar"),
    #file.path(driver_db_dir, bin_dir, "lib", "bcprov-jdk18on-173.jar"),
    file.path(driver_db_dir, bin_dir, "lib", "CryptCodecOpener.jar"),
    file.path(driver_db_dir, bin_dir, "ucanaccess-5.0.1.jar"))

  access_jdbc_driver <- RJDBC::JDBC(
    driverClass = "net.ucanaccess.jdbc.UcanaccessDriver",
    classPath = class_path)

  # connection to Access database ----
  con_string <- paste0("jdbc:ucanaccess://", access_db_path, ";jackcessOpener=com.gordthompson.ucanaccess.crypto.CryptCodecOpener")
  access_jdbc_connection <- RJDBC::dbConnect(access_jdbc_driver, con_string, user, password)
  return(access_jdbc_connection)
}
