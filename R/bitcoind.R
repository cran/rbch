#' Extracting Configuration Settings
#'
#' This function extracts information from the configuration
#' file \code{bitcoin.conf} with respect to the options  \code{rpcuser}
#' and \code{rpcpassword}.
#'
#' @param conf.file \code{character}, the fully qualified path.
#'
#' @return An S4-object of class \code{CONRPC}.
#' @author Bernhard Pfaff
#' @family bitcoind functions
#' @name conrpc
#' @aliases conrpc
#' @rdname conrpc
#' @export
#'
conrpc <- function(conf.file){
    con <- file(conf.file, open = "r", blocking = FALSE)
    bconf <- readLines(con)
    close(con)
    bconfl <- strsplit(bconf, split = "=")
    idx <- which(unlist(lapply(bconfl, function(x) x[1] == "rpcuser")))
    if (length(idx) > 0){
        valrpcuse <- bconfl[[idx]][2]
    } else {
        warning("'rpcuser' not set, using 'rbch'.\n")
        valrpcuse <- "rbch"
    }
    idx <- which(unlist(lapply(bconfl, function(x) x[1] == "rpcpassword")))
    if (length(idx) > 0){
        valrpcpwd <- bconfl[[idx]][2]
    } else {
        warning("'rpcpassword' not set, creating random string.\n")
        valrpcpwd <- paste0(sample(LETTERS, 12, TRUE), collapse = "")
    }
    idx <- which(unlist(lapply(bconfl, function(x) x[1] == "testnet")))
    if (length(idx) > 0){
        tnet <- as.logical(as.numeric(bconfl[[idx]][2]))
    } else {
        warning("'testnet' not set, setting testnet to TRUE.\n")
        tnet <- TRUE
    }
    idx <- which(unlist(lapply(bconfl, function(x) x[1] == "rpcconnect")))
    if (length(idx) > 0){
        rpcconnect <- bconfl[[idx]][2]
    } else {
        rpcconnect <-"127.0.0.1"
    }
    if (isTRUE(tnet)){
        curl <- paste0("http://", rpcconnect, ":18332")
    } else {
        curl <- paste0("http://", rpcconnect, ":8332")
    }
    new("CONRPC",
        rpcuse = valrpcuse,
        rpcpwd = valrpcpwd,
        testnet = tnet,
        url = curl,
        config = conf.file)
}
#' Start bitcoind server process
#'
#' This function does start the bitcoind-server process.
#' It should only be called when no suitable RPC-JSON
#' process is running
#'
#' @param confbch \code{CONRPC} object, returned from \code{conrpc()}.
#'
#' @details The process is started by calling \code{system()}.
#' Hereby, the options: \code{rpcuser}, \code{rpcpassword} and
#' \code{conf} are used in the call to \code{bitcoind}.
#' 
#' @param confbch \code{CONRPC} object, returned from \code{conrpc()}.
#' @author Bernhard Pfaff
#' @return \code{NULL}
#' @family bitcoind functions
#' @name startbch
#' @aliases startbch
#' @rdname startbch
#' @export
#'
startbch <- function(confbch){
    stopifnot(class(confbch) == "CONRPC")
    rpcuse <- slot(confbch, "rpcuse")
    rpcpwd <- slot(confbch, "rpcpwd")
    config <- slot(confbch, "config")
    tnet <- slot(confbch, "testnet")
    if (isTRUE(tnet)){
        strcmd <- paste("bitcoind -daemon -testnet -rpcuser=", rpcuse,
                        " -rpcpassword=", rpcpwd,
                        " -conf=", config,
                        sep = "")
    } else {
        strcmd <- paste("bitcoind -daemon -rpcuser=", rpcuse,
                        " -rpcpassword=", rpcpwd,
                        " -conf=", config,
                        sep = "")
    }
    system(strcmd)
    NULL
}
#' Start bitcoind server process (BTC alias)
#'
#' This function is an alias of startbch
#'
#' @param confbtc \code{CONRPC} object, returned from \code{conrpc()}.
#'
#' @details The process is started by calling \code{system()}.
#' Hereby, the options: \code{rpcuser}, \code{rpcpassword} and
#' \code{conf} are used in the call to \code{bitcoind}.
#'
#' @param confbtc \code{CONRPC} object, returned from \code{conrpc()}.
#' @author Bernhard Pfaff
#' @return \code{NULL}
#' @family bitcoind functions
#' @name startbtc
#' @aliases startbtc
#' @rdname startbtc
#' @export
#'
startbtc <- function(confbtc){
    startbch(confbtc)
    NULL
}
#' Stop bitcoind server process
#'
#' This function stops a running bitcoind process.
#' It calls \code{bitcoin-cli stop} \emph{via} the
#' R function \code{system()}.
#'
#' @param confbch \code{CONRPC} object, returned from \code{conrpc()}.
#'
#' @return NULL
#' @author Bernhard Pfaff
#' @family bitcoind functions
#' @name stopbch
#' @aliases stopbch
#' @rdname stopbch
#' @export
#'
stopbch <- function(confbch){
    stopifnot(class(confbch) == "CONRPC")
    curl <- slot(confbch, "url")
    ans <- POST(curl,
                authenticate(user = slot(confbch, "rpcuse"),
                             password = slot(confbch, "rpcpwd"),
                             type = "basic"),
                body = list(jsonrpc = "1.0",
                            id = "curltest",
                            method = "stop",
                            params = c()),
                encode = "json")
    stop_for_status(ans)
    NULL
}
#' Stop bitcoind server process (BTC alias)
#'
#' This function stops a running bitcoind process.
#' It calls \code{bitcoin-cli stop} \emph{via} the
#' R function \code{system()}.
#'
#' @param confbtc \code{CONRPC} object, returned from \code{conrpc()}.
#'
#' @return NULL
#' @author Bernhard Pfaff
#' @family bitcoind functions
#' @name stopbtc
#' @aliases stopbtc
#' @rdname stopbtc
#' @export
#'
stopbtc <- function(confbtc){
    stopbch(confbtc)
}
#' HTTP post of RPC-JSON
#'
#' This function executes an RPC-JSON post.
#'
#' @param con \code{CONRPC} object, returned from \code{conrpc()}.
#' @param api \code{character} the name of the RPC function.
#' @param plist \code{list} a named list object of the parameters for \code{api}
#'
#' @return A \code{list} object, coerced JSON answer from RPC.
#' @author Bernhard Pfaff
#' @family bitcoind functions
#' @name rpcpost
#' @aliases rpcpost
#' @rdname rpcpost
#' @export
#'
rpcpost <- function(con, api, plist = list()){
    stopifnot(class(con) == "CONRPC")
    api <- as.character(api)
    pid <- paste("rbch", api, sep = "-")
    ans <- POST(slot(con, "url"),
                authenticate(user = slot(con, "rpcuse"),
                             password = slot(con, "rpcpwd"),
                             type = "basic"),
                body = list(jsonrpc = "1.0",
                            id = pid,
                            method = api,
                            params = plist),
                encode = "json")
    ans <- content(ans)
    res <- ans$result
    if (is.null(res)){
        res <- ""
    }
    if (is.integer(res)){
        res <- list(res)
    }
    out <- new("ANSRPC",
               rpcname = api,
               id = pid,
               result = res,
               ecode = ans[["error"]][["code"]],
               emessage = ans[["error"]][["message"]]
               )
    out
}
