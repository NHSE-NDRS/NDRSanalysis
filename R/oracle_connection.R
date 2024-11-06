##### oracle_connection.R

### Oracle functions

#' createConnection: Create a database connection object
#'
#' @param hostname Name of host machine.  Default is "localhost".  When connecting from CAS3: "cas2.asckey"
#' @param port Port number of host machine.  Default is 1523.  When connecting from CAS3: 1521
#' @param sid Database (schema) ID.  Default is "CasRef01"
#' @param drv_path Path and file name of JDBC driver.  Default is "ojdbc7.jar"
#' @param username CAS username
#'
#' @return Returns a database connection object.
#' @export
#'
#' @examples
#' \dontrun{
#' casref01 = createConnection()
#' }
#' @importFrom RJDBC dbConnect JDBC
#' @importFrom rJava .jinit .jaddClassPath .jpackage
#' @importFrom svDialogs "dlgInput"

createConnection <- function(hostname="localhost" , port=1523, sid="CasRef01", drv_path="inst/java/ojdbc7.jar", username = NA)
{

  ## set up DB connection
  drv <- JDBC("oracle.jdbc.OracleDriver", classPath=drv_path, " ")
  constr <- paste0("jdbc:oracle:thin:@",hostname,":",port,":",sid)
  if (is.na(username)){
    username <- tryCatch(readline_tcltk_window("Please enter your AnalysisUserName: "),
                         error = function(e) {
                           u <- readline("Please enter your AnalysisUserName: ")
                           return(u) },
                         warning = function(w) {
                           u <- readline("Please enter your AnalysisUserName: ")
                           return(u) } )}
  con <- dbConnect(drv, constr, username, readline_tcltk_window("Enter Password",masking = T))

  return (con)
}

#Extends dbGetQuery to catch and return useful errors. Return value depends on type of error, to achieve compatibility with dbGetQuery.
#If return type is data.frame then the query succeeded and returned results.
#If return type is list then the query gave an Oracle error, return$oracleerror is the Oracle (most useful for the user), return$fullerror is the error state.
#If return type is character then another error arose, and return is the error state.
#Errors are also printed to the console.
#Specify returndata = FALSE

#' Runs a query in an Oracle database and reports Oracle errors with line and column number for debugging when those occur.
#' Returns a data.frame with the data, or a list containing data on the error arising.
#'
#' @rdname dbGetQueryOracle
#' @param connection JDBCConnection object created separately.
#' @param querystring Query or statement to execute, not terminated by a semicolon unless a PL/SQL statement.
#' @param ... Arguments to fill in holes in querystring if querystring is a prepared statement.
#' @param returndata Default TRUE for dbGetQueryOracle, should be FALSE for statements with no returned data.
#' @param timeit Default FALSE, turn to TRUE to enable profiling.
#' @param rowlimit Default NA, number of rows to return
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' dbGetQueryOracle(con, query)
#' dbSendQueryOracle(con, statement)
#' dbGetQueryOracle(con, statement, returndata = FALSE)
#' dbGetQueryOracle(con, "select ? from dual union select ? from dual", "cow", "hat", timeit = TRUE)
#' }
#'
#'
#' @export
#' @importFrom DBI dbGetQuery dbSendQuery dbFetch

dbGetQueryOracle = function(connection, querystring, ..., returndata = TRUE, timeit = FALSE, rowlimit = NA) {
  options("warning.length"=8170)
  return = tryCatch({
    start.time = Sys.time()
    call_arguments = c(list(conn = connection, statement = querystring), ...)
    if(returndata){
      if(is.na(rowlimit)){
        data = do.call(dbGetQuery, args = call_arguments)
      } else {
        resultset = do.call(dbSendQuery, args = call_arguments)
        data = dbFetch(resultset, n = rowlimit)
      }
      if(timeit) {
        writeLines(paste(as.character(nrow(data)), "rows and",  as.character(ncol(data)), "columns returned."))
        print(Sys.time() - start.time)
      }
      return(data)} else {
        do.call(dbSendQuery, args = call_arguments)
        #return(NA)
        if(timeit) {
          print(Sys.time() - start.time)
        }
      }

  },
  error = function(err){
    oralocation = regexpr("(ORA", err$message, fixed = TRUE)
    if (oralocation == -1 | returndata == FALSE){
      if(returndata == FALSE){errormessage = "You may also have a semicolon at the end of your query - try removing it."} else {
        errormessage = "\nNote that if you attempted to run a statement which does not return data (e.g. CREATE TABLE) your statement may have completed. If the error is truncated then please try again with a shorter query. You may also have a semicolon at the end of your query - try removing it."
      }
      message("The following error was received: \n")
      writeLines(err$message)
      message(errormessage)
      if(nchar(err$message) >= 8170){
        message("It appears your error was truncated. Oracle error messages may not be available for queries exceeding 4000 characters. To see Oracle error messages, try refactoring your code.")
      }
      return(err)} else {
        tryCatch({newquerystring = paste0("begin \n", querystring, "; \n end;")
        data = dbGetQuery(connection, newquerystring)
        print("Unable to recover error.")
        writeLines(err$message)
        if(nchar(err$message) >= 8170){
          message("It appears your error was truncated. Oracle error messages may not be available for queries exceeding 4000 characters. To see Oracle error messages, try refactoring your code.")
        }
        return(err)},
        error = function(newerror){
          neworalocation = regexpr("(ORA", newerror$message, fixed = TRUE)
          oracle.error = substr(newerror$message, neworalocation, nchar(newerror$message))
          oracle.error = decrementlinenumbers(oracle.error)
          errormessage = paste("The following Oracle error was received: \n", oracle.error, " \nIn the following SQL:\n", querystring)
          message("The following Oracle error was received: \n")
          writeLines(oracle.error)
          message("\nIn the following SQL:\n")
          writeLines(addlinenumbers(querystring))
          if(nchar(err$message) >= 8170){
            message("It appears your error was truncated. Oracle error messages may not be available for queries exceeding 4000 characters. To see Oracle error messages, try refactoring your code.")
          }
          return(list(oracleerror = oracle.error, usermessage = errormessage, originalerror = err, plsqlerror = newerror))
        }
        )
      }
  }
  )
}

#' @rdname dbGetQueryOracle
#' @export
dbSendQueryOracle = function(connection, querystring, ...){
  do.call(dbGetQueryOracle, c(list(connection = connection, querystring = querystring, returndata = FALSE), ...))
}

#Necessary to report correct line numbers, as we add an initial "Begin" line when looking for them.

decrementlinenumbers = function(inputstring){
  workinglist = strsplit(inputstring, ": line ")
  newworkinglist = workinglist[[1]]
  for (itemindex in 1:length(workinglist[[1]])){
    item = workinglist[[1]][itemindex]
    columnstart = regexpr(", column", item, fixed = TRUE)
    if (columnstart > 1){
      linenumber = as.numeric(substr(item, 1, columnstart - 1))
      newitem = paste0(as.character(linenumber - 1), substr(item, columnstart,nchar(item)))
      newworkinglist[itemindex] = newitem
    } else {newworkinglist[itemindex] = item}
  }
  return(paste0(newworkinglist, collapse = ": line "))
}

addlinenumbers = function(inputstring){
  workinglist = strsplit(inputstring, "\n")
  newworkinglist = workinglist[[1]]
  numberlines = length(newworkinglist)
  numberdigits = nchar(numberlines)
  for(itemindex in 1:numberlines){
    item = workinglist[[1]][itemindex]
    stringtoprepend = paste0(rep(" ", numberdigits - nchar(as.character(itemindex))), as.character(itemindex), "  ", collapse = "")
    newitem = paste0(stringtoprepend, item)
    newworkinglist[itemindex] = newitem
  }
  return(paste0(newworkinglist, collapse = "\n"))
}

#Returns an explain plan for the query provided.

#' Requests an explain plan for a query in an Oracle database and prints it.
#'
#' @param connection JDBCConnection object created separately.
#' @param querystring Query or statement to execute, not terminated by a semicolon unless a PL/SQL statement.
#' @param ... Arguments to fill in holes in querystring if querystring is a prepared statement.
#' @param returnplan Default FALSE, set to TRUE to return the plan table as a data.frame of text.
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' dbGetQueryOracle(con, query)
#' dbSendQueryOracle(con, statement)
#' dbGetQueryOracle(con, statement, returndata = FALSE)
#' dbGetQueryOracle(con, "select ? from dual union select ? from dual", "cow", "hat", timeit = TRUE)
#' }
#'
#' @export
#' @importFrom DBI dbGetQuery dbSendQuery

dbExplainPlan = function(connection, querystring, ..., returnplan = FALSE){
  options("warning.length"=8170)
  if(length(grep("@", querystring, fixed = TRUE)) > 0){warning("Explaining a remote query will block further explain plans until the connection is reset.")}
  return = tryCatch({
    call_arguments = c(list(conn = connection, statement = paste("explain plan for", querystring)), ...)
    do.call(dbSendQuery, call_arguments)
    plantable = dbGetQueryOracle(connection, "SELECT PLAN_TABLE_OUTPUT FROM TABLE(DBMS_XPLAN.DISPLAY)")
    print(plantable)
    if(returnplan){return(plantable)}
  },
  error = function(err){message("Error in query or you lack necessary privileges. The error message follows:")
    writeLines(err$message)
    return(NA)})
}

#' Builds a wrapper for dbGetQuery for exploration queries
#'
#' @param connection JDBCConnection object created separately.
#' @param timeit Default FALSE, turn to TRUE to enable timing.
#' @param rowlimit Default 50, number of rows queries should return
#'
#' @return None
#'
#' @examples
#'
#' \dontrun{
#' explorer = dbExploreOracle(con, rowlimit = 10)
#' explorer("select * from dual")
#' }
#'
#' @export
dbExploreOracle = function(connection, rowlimit = 50, timeit = FALSE){
  explorefunction = function(querystring, ...){
    return(dbGetQueryOracle(connection, querystring, ..., rowlimit = rowlimit, timeit = timeit))
  }
  return(explorefunction)
}
