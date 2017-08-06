



library(googleway)

get_geocode <- function(df, var)
{
    for(i in seq(1:nrow(df)))
    {
        
        txt = paste('df$LAT[', i, '] = google_geocode(address = df$', var, '[', i, '], key = apiKey)$results$geometry$location$lat' ,  sep="" )
        print(txt)
        eval(parse(text=txt))
        txt = paste('df$LNG[', i, '] = google_geocode(address = df$', var, '[', i, '], key = apiKey)$results$geometry$location$lng' ,  sep="" )
        eval(parse(text=txt))
    }
    return(df)
}


get_address <- function(df)
{
    for(i in seq(1:nrow(df)))
    {
        
        txt = paste('df$address[', i, '] = google_reverse_geocode(location = c(accidents2$LATITUDE[',i, '], accidents2$LONGITUDE[',i,']), key = apiKey)$results$formatted_address[1]' ,  sep="" )
        eval(parse(text=txt))
        
    }
    return(df)
}

google_places <- function(search_string = NULL,
                          location = NULL,
                          radar = FALSE,
                          radius = NULL,
                          rankby = NULL,
                          keyword = NULL,
                          language = NULL,
                          name = NULL,
                          place_type = NULL,
                          price_range = NULL,
                          open_now = NULL,
                          page_token = NULL,
                          simplify = TRUE,
                          curl_proxy = NULL,
                          key
){
    
    ## check if both search_string & location == NULL
    if(is.null(search_string) & is.null(location))
        stop("One of 'search_string' or 'location' must be specified")
    
    if(!is.null(location)){
        if(length(location) != 2 | !is.numeric(location)){
            stop("location must be a numeric vector of latitude/longitude coordinates")
        }else{
            location <- paste0(location, collapse = ",")
        }
    }
    
    ## check radar is logical
    if(!is.logical(radar))
        stop("radar must be logical")
    
    ## if radar search, must provide location, key, radius
    ## if radar search, one of keyword, name or type
    if(isTRUE(radar)){
        if(!is.null(search_string))
            warning("the search_string in a radar search will be ignored")
        
        if(is.null(keyword) & is.null(name) & is.null(place_type))
            stop("when using a radar search, one of keyword, name or place_type must be provided")
        
        if(is.null(location))
            stop("when using a radar search, location must be provided")
        
        if(is.null(radius))
            stop("when using a radar search, radius must be provided")
        
    }
    
    ## radius must be included if using a location search
    if(is.null(search_string) & !is.null(location) & is.null(radius))
        stop("you must specify a radius if only using a 'location' search")
    
    ## check radius < 50000m
    if(!is.null(radius)){
        if(!is.numeric(radius))
            stop("radius must be numeric between 0 and 50000")
        
        if(radius > 50000 | radius < 0)
            stop("radius must be numeric between 0 and 50000")
    }
    
    ## rankby has correct arguments
    if(!is.null(rankby) & !is.null(location))
        if(!rankby %in% c("prominence","distance","location"))
            stop("rankby must be one of either prominence, distance or location")
    
    ## warning if rankby used with search_string
    if(!is.null(search_string) & !is.null(rankby))
        warning("The 'rankby' argument is ignored when using a 'search_string'")
    
    ## radius must not be included if rankby=distance
    if(!is.null(rankby) & !is.null(location)){
        if(!is.null(radius) & rankby == "distance"){
            warning("radius is ignored when rankby == 'distance'")
            radius <- NULL
        }
    }
    
    ## if rankby == distance, then one of keyword, name or place_type must be specified
    if(!is.null(rankby) & !is.null(location)){
        if(rankby == "distance" &
           is.null(keyword) & is.null(name) & is.null(place_type))
            stop("you have specified rankby to be 'distance', so you must provide one of 'keyword','name' or 'place_type'")
    }
    
    ## language check
    if(!is.null(language) & (class(language) != "character" | length(language) > 1))
        stop("language must be a single character vector or string")
    
    
    ## warning if name used with search_string
    if(!is.null(search_string) & !is.null(name))
        warning("The 'name' argument is ignored when using a 'search_string'")
    
    if(length(name) > 1)
        name <- paste0(name, collapse = "|")
    
    ## price range is between 0 and 4
    if(!is.null(price_range)){
        if(!is.numeric(price_range) | (is.numeric(price_range) & length(price_range) != 2))
            stop("price_range must be a numeric vector of length 2")
    }
    
    if(!is.null(price_range)){
        if(!price_range[1] %in% 0:4 | !price_range[2] %in% 0:4)
            stop("price_range must be between 0 and 4 inclusive")
    }
    
    ## check place type
    if(!is.null(place_type)){
        if(length(place_type) > 1 | !is.character(place_type))
            stop("place_type must be a string vector of length 1")
    }
    
    ## open_now is boolean
    if(!is.null(open_now)){
        if(!is.logical(open_now) | length(open_now) != 1)
            stop("open_now must be logical of length 1")
    }
    
    ## page token is single string
    if(!is.null(page_token)){
        if(!is.character(page_token) | length(page_token) != 1)
            stop("page_token must be a string of length 1")
    }
    
    LogicalCheck(simplify)
    
    ## construct the URL
    ## if search string is specified, use the 'textsearch' url
    ## if no search_string, use the 'lat/lon' url
    if(isTRUE(radar)){
        map_url <- "https://maps.googleapis.com/maps/api/place/radarsearch/json?"
    }else{
        if(!is.null(search_string)){
            search_string <- gsub(" ", "+", search_string)
            map_url <- paste0("https://maps.googleapis.com/maps/api/place/textsearch/json?query=", search_string)
        }else{
            map_url <- paste0("https://maps.googleapis.com/maps/api/place/nearbysearch/json?")
        }
    }
    
    map_url <- constructURL(map_url, c("location" = location,
                                       "radius" = radius,
                                       "rankby" = rankby,
                                       "keywrod" = keyword,
                                       "language" = language,
                                       "name" = name,
                                       "type" = place_type,
                                       "minprice" = price_range[1],
                                       "maxprice" = price_range[2],
                                       "opennow" = open_now,
                                       "pagetoken" = page_token,
                                       "key" = key))
    
    
    return(fun_download_data(map_url, simplify, curl_proxy))
    
}

atitudeCheck <- function(lat, arg){
    if(!is.numeric(lat) | lat < -90 | lat > 90)
        stop(paste0(arg, " must be a value between -90 and 90 (inclusive)"))
}

# Longitude Check
#
# Checks that a value is between -90:90
LongitudeCheck <- function(lat, arg){
    if(!is.numeric(lat) | lat < -180 | lat > 180)
        stop(paste0(arg, " must be a value between -180 and 180 (inclusive)"))
}


# URL Check
#
# Checks for a valid URL
# @param url url to check
URLCheck <- function(url){
    if(length(url) != 1)
        stop("only one URL is valid")
    
    
}


# Logical Check
#
# Checks for the correct logical parameter
# @param param parameter to check
LogicalCheck <- function(param){
    if(!is.logical(param) | length(param) != 1)
        stop(paste0(deparse(substitute(param))," must be logical - TRUE or FALSE"))
    
}

# Check hex colours
#
# Checks for valid hexadecimal value
#
# @param df \code{data.frame}
# @param cols string of columns to check
check_hex_colours <- function(df, cols){
    ## checks the columns of data that should be in HEX colours
    
    for(myCol in cols){
        if(!all(grepl("^#(?:[0-9a-fA-F]{3}){1,2}$", df[, myCol])))
            stop(paste0("Incorrect colour specified in ", myCol, ". Make sure the colours in the column are valid hexadecimal HTML colours"))
    }
}

# Check opacities
#
# Checks for valid opacity values
#
# @param df \code{data.frame}
# @param cols string of columns to check
check_opacities <- function(df, cols){
    
    for(myCol in cols){
        ## allow NAs through
        vals <- df[, myCol][!is.na(df[, myCol])]
        if(length(vals) > 0)
            if(any(vals < 0) | any(vals > 1))
                stop(paste0("opacity values for ", myCol, " must be between 0 and 1"))
    }
}

# Check for columns
#
# Checks for valid columns
#
# @param df \code{data.frame}
# @param cols string of columns
check_for_columns <- function(df, cols){
    
    ## check to see if the specified columns exist
    if(!all(cols %in% names(df)))
        stop(paste0("Could not find columns: "
                    , paste0(cols[!cols %in% names(df)], collapse = ", ")
                    , " in the data"))
    
}

# Latitude column
#
# calls the correct function to check for latitude column
# @param data \code{data.frame}
# @param lat string identifying the latitude column
# @param calling_function the function that called this function
latitude_column <- function(data, lat, calling_function){
    
    if(is.null(lat)){
        lat_col <- find_lat_column(names(data), calling_function)
        names(data)[names(data) == lat_col[1]] <- "lat"
    }else{
        ## check the supplied latitude column exists
        check_for_columns(data, lat)
        # names(data)[names(data) == lat] <- "lat"
    }
    return(data)
}

# Longitude column
#
# calls the correct function to check for longitude column
# @param data \code{data.frame}
# @param lon string identifying the longitude column
# @param calling_function the function that called this function
longitude_column <- function(data, lon, calling_function){
    if(is.null(lon)){
        lon_col <- find_lon_column(names(data), calling_function)
        names(data)[names(data) == lon_col[1]] <- "lng"
    }else{
        check_for_columns(data, lon)
        # names(data)[names(data) == lon] <- "lng"
    }
    return(data)
}

# Find Lat Column
#
# Tries to identify the latitude column
# @param names string of column names
# @param calling_function the function that called this function
# @param stopOnFailure logical
find_lat_column = function(names, calling_function, stopOnFailure = TRUE) {
    
    lats = names[grep("^(lat|lats|latitude|latitudes)$", names, ignore.case = TRUE)]
    
    if (length(lats) == 1) {
        # if (length(names) > 1) {
        #   message("Assuming '", lats, " is the latitude column")
        # }
        ## passes
        return(list(lat = lats))
    }
    
    if (stopOnFailure) {
        stop(paste0("Couldn't infer latitude column for ", calling_function))
    }
    
    list(lat = NA)
}

# Find Lon Column
#
# Tries to identify the longitude column
# @param names string of column names
# @param calling_function the function that called this function
# @param stopOnFailure logical
find_lon_column = function(names, calling_function, stopOnFailure = TRUE) {
    
    lons = names[grep("^(lon|lons|lng|lngs|long|longs|longitude|longitudes)$", names, ignore.case = TRUE)]
    
    if (length(lons) == 1) {
        # if (length(names) > 1) {
        #   message("Assuming '", lons, " is the longitude column")
        # }
        ## passes
        return(list(lon = lons))
    }
    
    if (stopOnFailure) {
        stop(paste0("Couldn't infer longitude columns for ", calling_function))
    }
    
    list(lon = NA)
}


google_map_update <- function(map_id,
                              session = shiny::getDefaultReactiveDomain(),
                              data = NULL,
                              deferUntilFlush = TRUE) {
    
    if (is.null(session)) {
        stop("google_map_update must be called from the server function of a Shiny app")
    }
    
    structure(
        list(
            session = session,
            id = map_id,
            x = structure(
                list(),
                google_map_data = data
            ),
            deferUntilFlush = deferUntilFlush,
            dependencies = NULL
        ),
        class = "google_map_update"
    )
}


#' Google dispatch
#'
#' Extension points for plugins
#'
#' @param map a map object, as returned from \code{\link{google_map}}
#' @param funcName the name of the function that the user called that caused
#'   this \code{google_dispatch} call; for error message purposes
#' @param google_map an action to be performed if the map is from
#'   \code{\link{google_map}}
#' @param google_map_update an action to be performed if the map is from
#'   \code{\link{google_map_update}}
#'
#' @return \code{google_dispatch} returns the value of \code{google_map} or
#' or an error. \code{invokeMethod} returns the
#' \code{map} object that was passed in, possibly modified.
#'
#' @export
google_dispatch = function(map,
                           funcName,
                           google_map = stop(paste(funcName, "requires a map update object")),
                           google_map_update = stop(paste(funcName, "does not support map udpate objects"))
) {
    if (inherits(map, "google_map"))
        return(google_map)
    else if (inherits(map, "google_map_update"))
        return(google_map_update)
    else
        stop("Invalid map parameter")
}


#' @param data a data object that will be used when evaluating formulas in
#'   \code{...}
#' @param method the name of the JavaScript method to invoke
#' @param ... unnamed arguments to be passed to the JavaScript method
#' @rdname google_dispatch
#' @export
invoke_method = function(map, data, method, ...) {
    args = evalFormula(list(...), data)
    
    google_dispatch(map,
                    method,
                    google_map = {
                        x = map$x$calls
                        if (is.null(x)) x = list()
                        n = length(x)
                        x[[n + 1]] = list(functions = method, args = args)
                        map$x$calls = x
                        map
                    },
                    google_map_update = {
                        invoke_remote(map, method, args)
                    }
    )
}


invoke_remote = function(map, method, args = list()) {
    if (!inherits(map, "google_map_update"))
        stop("Invalid map parameter; googlemap_update object was expected")
    
    msg <- list(
        id = map$id,
        calls = list(
            list(
                dependencies = lapply(map$dependencies, shiny::createWebDependency),
                method = method,
                args = args
            )
        )
    )
    
    sess <- map$session
    if (map$deferUntilFlush) {
        
        sess$onFlushed(function() {
            sess$sendCustomMessage("googlemap-calls", msg)
        }, once = TRUE)
        
    } else {
        sess$sendCustomMessage("googlemap-calls", msg)
    }
    map
}


### ----------
## taken from Rstudio::leaflet package

# Evaluate list members that are formulae, using the map data as the environment
# (if provided, otherwise the formula environment)
evalFormula = function(list, data) {
    evalAll = function(x) {
        if (is.list(x)) {
            structure(lapply(x, evalAll), class = class(x))
        } else resolveFormula(x, data)
    }
    evalAll(list)
}



resolveFormula = function(f, data) {
    if (!inherits(f, 'formula')) return(f)
    if (length(f) != 2L) stop("Unexpected two-sided formula: ", deparse(f))
    
    doResolveFormula(data, f)
}

doResolveFormula = function(data, f) {
    UseMethod("doResolveFormula")
}


doResolveFormula.data.frame = function(data, f) {
    eval(f[[2]], data, environment(f))
}




# Set Defaults
#
# @param col column to check / add to the data
# @param val default value for the column
# @param df data to be checked/ added to
SetDefault <- function(col, val, df){
    if(is.null(col)){
        ## use the default value
        return(rep(val, nrow(df)))
    }else{
        ## if a column has been supplied, use that,
        ## otherwise, use the default value supplied
        if(col %in% names(df)){
            return(df[, col])
        }else{
            ## assume the value supplied is the default value
            return(rep(col, nrow(df)))
        }
    }
}

# Construct url
#
# Constructs the relevant API url, given the arguments
# @param map_url string map url
# @param urlArgs other arguments to append to the URL string
constructURL <- function(map_url, urlArgs){
    
    return(paste0(map_url,
                  paste0("&",
                         paste0(names(urlArgs)),
                         "=",
                         paste0(urlArgs), collapse = "")
    )
    )
}


#' Map Styles
#'
#' Various styles for a \code{google_map()} map.
#'
#' @examples
#' \dontrun{
#' map_key <- "your_map_key"
#' google_map(key = map_key, style = map_styles()$silver)
#'
#' }
#'
#' @note you can generate your own map styles at \url{https://mapstyle.withgoogle.com/}
#'
#' @return list of styles
#' @export
map_styles <- function(){
    
    standard <- '[]'
    silver <- '[{"elementType": "geometry","stylers": [{"color": "#f5f5f5"}]},{"elementType": "labels.icon","stylers": [{"visibility": "off"}]},{"elementType": "labels.text.fill","stylers": [{"color": "#616161"}]},{"elementType": "labels.text.stroke","stylers": [{"color": "#f5f5f5"}]},{"featureType": "administrative.land_parcel","elementType": "labels.text.fill","stylers": [{"color": "#bdbdbd"}]},{"featureType": "poi","elementType": "geometry","stylers": [{"color": "#eeeeee"}]},{"featureType": "poi","elementType": "labels.text.fill","stylers": [{"color": "#757575"}]},{"featureType": "poi.park","elementType": "geometry","stylers": [{"color": "#e5e5e5"}]},{"featureType": "poi.park","elementType": "labels.text.fill","stylers": [{"color": "#9e9e9e"}]},{"featureType": "road","elementType": "geometry","stylers": [{"color": "#ffffff"}]},{"featureType": "road.arterial","elementType": "labels.text.fill","stylers": [{"color": "#757575"}]},{"featureType": "road.highway","elementType": "geometry","stylers": [{"color": "#dadada"}]},{"featureType": "road.highway","elementType": "labels.text.fill","stylers": [{"color": "#616161"}]},{"featureType": "road.local","elementType": "labels.text.fill","stylers": [{"color": "#9e9e9e"}]},{"featureType": "transit.line","elementType": "geometry","stylers": [{"color": "#e5e5e5"}]},{"featureType": "transit.station","elementType": "geometry","stylers": [{"color": "#eeeeee"}]},{"featureType": "water","elementType": "geometry","stylers": [{"color": "#c9c9c9"}]},{"featureType": "water","elementType": "labels.text.fill","stylers": [{"color": "#9e9e9e"}]}]'
    retro <- '[{"elementType": "geometry","stylers": [{"color": "#ebe3cd"}]},{"elementType": "labels.text.fill","stylers": [{"color": "#523735"}]},{"elementType": "labels.text.stroke","stylers": [{"color": "#f5f1e6"}]},{"featureType": "administrative","elementType": "geometry.stroke","stylers": [{"color": "#c9b2a6"}]},{"featureType": "administrative.land_parcel","elementType": "geometry.stroke","stylers": [{"color": "#dcd2be"}]},{"featureType": "administrative.land_parcel","elementType": "labels.text.fill","stylers": [{"color": "#ae9e90"}]},{"featureType": "landscape.natural","elementType": "geometry","stylers": [{"color": "#dfd2ae"}]},{"featureType": "poi","elementType": "geometry","stylers": [{"color": "#dfd2ae"}]},{"featureType": "poi","elementType": "labels.text.fill","stylers": [{"color": "#93817c"}]},{"featureType": "poi.park","elementType": "geometry.fill","stylers": [{"color": "#a5b076"}]},{"featureType": "poi.park","elementType": "labels.text.fill","stylers": [{"color": "#447530"}]},{"featureType": "road","elementType": "geometry","stylers": [{"color": "#f5f1e6"}]},{"featureType": "road.arterial","elementType": "geometry","stylers": [{"color": "#fdfcf8"}]},{"featureType": "road.highway","elementType": "geometry","stylers": [{"color": "#f8c967"}]},{"featureType": "road.highway","elementType": "geometry.stroke","stylers": [{"color": "#e9bc62"}]},{"featureType": "road.highway.controlled_access","elementType": "geometry","stylers": [{"color": "#e98d58"}]},{"featureType": "road.highway.controlled_access","elementType": "geometry.stroke","stylers": [{"color": "#db8555"}]},{"featureType": "road.local","elementType": "labels.text.fill","stylers": [{"color": "#806b63"}]},{"featureType": "transit.line","elementType": "geometry","stylers": [{"color": "#dfd2ae"}]},{"featureType": "transit.line","elementType": "labels.text.fill","stylers": [{"color": "#8f7d77"}]},{"featureType": "transit.line","elementType": "labels.text.stroke","stylers": [{"color": "#ebe3cd"}]},{"featureType": "transit.station","elementType": "geometry","stylers": [{"color": "#dfd2ae"}]},{"featureType": "water","elementType": "geometry.fill","stylers": [{"color": "#b9d3c2"}]},{"featureType": "water","elementType": "labels.text.fill","stylers": [{"color": "#92998d"}]}]'
    dark <- '[{"elementType": "geometry","stylers": [{"color": "#212121"}]},{"elementType": "labels.icon","stylers": [{"visibility": "off"}]},{"elementType": "labels.text.fill","stylers": [{"color": "#757575"}]},{"elementType": "labels.text.stroke","stylers": [{"color": "#212121"}]},{"featureType": "administrative","elementType": "geometry","stylers": [{"color": "#757575"}]},{"featureType": "administrative.country","elementType": "labels.text.fill","stylers": [{"color": "#9e9e9e"}]},{"featureType": "administrative.land_parcel","stylers": [{"visibility": "off"}]},{"featureType": "administrative.locality","elementType": "labels.text.fill","stylers": [{"color": "#bdbdbd"}]},{"featureType": "poi","elementType": "labels.text.fill","stylers": [{"color": "#757575"}]},{"featureType": "poi.park","elementType": "geometry","stylers": [{"color": "#181818"}]},{"featureType": "poi.park","elementType": "labels.text.fill","stylers": [{"color": "#616161"}]},{"featureType": "poi.park","elementType": "labels.text.stroke","stylers": [{"color": "#1b1b1b"}]},{"featureType": "road","elementType": "geometry.fill","stylers": [{"color": "#2c2c2c"}]},{"featureType": "road","elementType": "labels.text.fill","stylers": [{"color": "#8a8a8a"}]},{"featureType": "road.arterial","elementType": "geometry","stylers": [{"color": "#373737"}]},{"featureType": "road.highway","elementType": "geometry","stylers": [{"color": "#3c3c3c"}]},{"featureType": "road.highway.controlled_access","elementType": "geometry","stylers": [{"color": "#4e4e4e"}]},{"featureType": "road.local","elementType": "labels.text.fill","stylers": [{"color": "#616161"}]},{"featureType": "transit","elementType": "labels.text.fill","stylers": [{"color": "#757575"}]},{"featureType": "water","elementType": "geometry","stylers": [{"color": "#000000"}]},{"featureType": "water","elementType": "labels.text.fill","stylers": [{"color": "#3d3d3d"}]}]'
    night <- '[{"elementType": "geometry","stylers": [{"color": "#242f3e"}]},{"elementType": "labels.text.fill","stylers": [{"color": "#746855"}]},{"elementType": "labels.text.stroke","stylers": [{"color": "#242f3e"}]},{"featureType": "administrative.locality","elementType": "labels.text.fill","stylers": [{"color": "#d59563"}]},{"featureType": "poi","elementType": "labels.text.fill","stylers": [{"color": "#d59563"}]},{"featureType": "poi.park","elementType": "geometry","stylers": [{"color": "#263c3f"}]},{"featureType": "poi.park","elementType": "labels.text.fill","stylers": [{"color": "#6b9a76"}]},{"featureType": "road","elementType": "geometry","stylers": [{"color": "#38414e"}]},{"featureType": "road","elementType": "geometry.stroke","stylers": [{"color": "#212a37"}]},{"featureType": "road","elementType": "labels.text.fill","stylers": [{"color": "#9ca5b3"}]},{"featureType": "road.highway","elementType": "geometry","stylers": [{"color": "#746855"}]},{"featureType": "road.highway","elementType": "geometry.stroke","stylers": [{"color": "#1f2835"}]},{"featureType": "road.highway","elementType": "labels.text.fill","stylers": [{"color": "#f3d19c"}]},{"featureType": "transit","elementType": "geometry","stylers": [{"color": "#2f3948"}]},{"featureType": "transit.station","elementType": "labels.text.fill","stylers": [{"color": "#d59563"}]},{"featureType": "water","elementType": "geometry","stylers": [{"color": "#17263c"}]},{"featureType": "water","elementType": "labels.text.fill","stylers": [{"color": "#515c6d"}]},{"featureType": "water","elementType": "labels.text.stroke","stylers": [{"color": "#17263c"}]}]'
    aubergine <- '[{"elementType": "geometry","stylers": [{"color": "#1d2c4d"}]},{"elementType": "labels.text.fill","stylers": [{"color": "#8ec3b9"}]},{"elementType": "labels.text.stroke","stylers": [{"color": "#1a3646"}]},{"featureType": "administrative.country","elementType": "geometry.stroke","stylers": [{"color": "#4b6878"}]},{"featureType": "administrative.land_parcel","elementType": "labels.text.fill","stylers": [{"color": "#64779e"}]},{"featureType": "administrative.province","elementType": "geometry.stroke","stylers": [{"color": "#4b6878"}]},{"featureType": "landscape.man_made","elementType": "geometry.stroke","stylers": [{"color": "#334e87"}]},{"featureType": "landscape.natural","elementType": "geometry","stylers": [{"color": "#023e58"}]},{"featureType": "poi","elementType": "geometry","stylers": [{"color": "#283d6a"}]},{"featureType": "poi","elementType": "labels.text.fill","stylers": [{"color": "#6f9ba5"}]},{"featureType": "poi","elementType": "labels.text.stroke","stylers": [{"color": "#1d2c4d"}]},{"featureType": "poi.park","elementType": "geometry.fill","stylers": [{"color": "#023e58"}]},{"featureType": "poi.park","elementType": "labels.text.fill","stylers": [{"color": "#3C7680"}]},{"featureType": "road","elementType": "geometry","stylers": [{"color": "#304a7d"}]},{"featureType": "road","elementType": "labels.text.fill","stylers": [{"color": "#98a5be"}]},{"featureType": "road","elementType": "labels.text.stroke","stylers": [{"color": "#1d2c4d"}]},{"featureType": "road.highway","elementType": "geometry","stylers": [{"color": "#2c6675"}]},{"featureType": "road.highway","elementType": "geometry.stroke","stylers": [{"color": "#255763"}]},{"featureType": "road.highway","elementType": "labels.text.fill","stylers": [{"color": "#b0d5ce"}]},{"featureType": "road.highway","elementType": "labels.text.stroke","stylers": [{"color": "#023e58"}]},{"featureType": "transit","elementType": "labels.text.fill","stylers": [{"color": "#98a5be"}]},{"featureType": "transit","elementType": "labels.text.stroke","stylers": [{"color": "#1d2c4d"}]},{"featureType": "transit.line","elementType": "geometry.fill","stylers": [{"color": "#283d6a"}]},{"featureType": "transit.station","elementType": "geometry","stylers": [{"color": "#3a4762"}]},{"featureType": "water","elementType": "geometry","stylers": [{"color": "#0e1626"}]},{"featureType": "water","elementType": "labels.text.fill","stylers": [{"color": "#4e6d70"}]}]'
    
    return(list(standard = standard,
                silver = silver,
                retro = retro,
                dark = dark,
                night = night,
                aubergine = aubergine))
}

LayerId <- function(layer_id){
    if(!is.null(layer_id) & length(layer_id) != 1)
        stop("please provide a single value for 'layer_id'")
    
    if(is.null(layer_id)){
        return("defaultLayerId")
    }else{
        return(layer_id)
    }
}

# Object Columns
#
# Defines the columns used by the Maps API so only those required
# are kept
#
# @param obj string specifying the type of object
# @return vector of column names
objectColumns <- function(obj = c("polylinePolyline",
                                  "polylineCoords",
                                  "polygonPolyline",
                                  "polygonCoords")){
    
    return(
        switch(obj,
               "polylineCoords" = c("id", "lat","lng", "geodesic","stroke_colour",
                                    "stroke_weight","stroke_opacity","mouse_over",
                                    "mouse_over_group", "info_window", "z_index"),
               "polylinePolyline" = c("id", "polyline", "geodesic","stroke_colour",
                                      "stroke_weight","stroke_opacity","mouse_over",
                                      "mouse_over_group", "info_window", "z_index"),
               "polygonCoords" = c("id","pathId","lat","lng","stroke_colour",
                                   "stroke_weight","stroke_opacity","fill_colour",
                                   "fill_opacity", "info_window","mouse_over",
                                   "mouse_over_group", "z_index"))
    )
}



# polyCheck.sf <- function(data, polyline, lat, lon){
#   ## sf objects will use the 'sfc_ geometry' column
#   ## return data, and the de-constructed geometry column
#
#   ## geom column
#   geomCol <- which(unlist(lapply(data, function(x) "sfc" %in% class(x))))
#
#
#
# }
#
#
# polyCheck.default <- function(data, polyline, lat, lon){
#   ## nothing to do
#   return(list(data = data, polyline = polyline, lat = lat, lon = lon))
# }
#
#
# sfData <- function(geom) UseMethod("sfData")
#
# sfData.sfc_LINESTRING <- function(geom){
#
# }
#
#
# sfData.sfc_MULTIPOLYGON <- function(geom){
#
#
#     lapply(geom, function(x){
#
#          lapply(1:length(x), function(y){
#
#           data.frame(
#             lineId = y,
#             lat = x[[y]][[1]][,2],
#             lon = x[[y]][[1]][,1],
#             hole = (y > 1)[c(T, F)]
#           )
#         })
#     })
#
#
# }
#
#
# createJSON.default <- function(geom){
#
# }

# createJSON <- function(obj){
#   UseMethod("dataType", obj)
# }
#
# #' @export
# dataType.default <- function(data) stop(paste0("I don't yet know how to work with objects of class ", class(data)))
#
# #' @export
# dataType.data.frame <- function(data) print("data.frame")
#
# #' @export
# dataType.data.table <- function(data) print("data.table")
#
# #' @export
# dataType.SpatialLinesDataFrame <- function(shp, id = NULL) {
#   print("spatial lines data frame")
#   ## extract polyline stuff
#   # sp::SpatialLines(data@lines[1])
#
#   ## need to extract a data.frame of attributes (slot(shp, "data")), AND
#   ## a data.frame of each list of coordinates in lat/lon
#
#   ## in the spatialpolylinesdataframe, each 'line' corresponds to a row of the data
#   ## if no id has been specified, create one based on rowname
#   data <- shp@data
#
#   if(!is.null(id))
#     data[, 'id'] <- as.character(data[, id])
# }



#' @useDynLib googleway
#' @importFrom Rcpp evalCpp
#' @importFrom jsonlite fromJSON
#' @importFrom curl curl
#' @importFrom grDevices col2rgb
#' @importFrom stats setNames
NULL


#' Pipe
#'
#' Uses the pipe operator (\code{\%>\%}) to chain statements. Useful for adding
#' layers to a \code{google_map}
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs,rhs A google map and a layer to add to it
#' @examples
#' \dontrun{
#'
#' key <- "your_api_key"
#' google_map(key = key) %>%
#' add_traffic()
#'
#' }
NULL

## build notes
# --use-valgrind
directions_data <- function(base_url,
                            information_type = c("directions","distance"),
                            origin,
                            destination,
                            mode = c('driving','walking','bicycling','transit'),
                            departure_time = NULL,
                            arrival_time = NULL,
                            waypoints = NULL,
                            optimise_waypoints = FALSE,
                            alternatives = FALSE,
                            avoid = NULL,
                            units = c("metric", "imperial"),
                            traffic_model = NULL,
                            transit_mode = NULL,
                            transit_routing_preference = NULL,
                            language = NULL,
                            region = NULL,
                            key,
                            simplify = TRUE,
                            curl_proxy = NULL){
    
    ## parameter check
    if(is.null(key))
        stop("A Valid Google Developers API key is required")
    
    mode <- match.arg(mode)
    units <- match.arg(units)
    # traffic_model <- match.arg(traffic_model)
    
    LogicalCheck(simplify)
    
    ## transit_mode is only valid where mode = transit
    if(!is.null(transit_mode) & mode != "transit"){
        warning("You have specified a transit_mode, but are not using mode = 'transit'. Therefore this argument will be ignored")
        transit_mode <- NULL
    }else if(!is.null(transit_mode) & mode == "transit"){
        transit_mode <- match.arg(transit_mode, choices = c("bus","subway","train","tram","rail"))
    }
    
    ## transit_routing_preference only valid where mode == transit
    if(!is.null(transit_routing_preference) & mode != "transit"){
        warning("You have specified a transit_routing_preference, but are not using mode = 'transit'. Therefore this argument will be ignored")
        transit_routing_preference <- NULL
    }else if(!is.null(transit_routing_preference) & mode == "transit"){
        transit_routing_preference <- match.arg(transit_routing_preference, choices = c("less_walking","fewer_transfers"))
        transit_routing_preference <- paste0(transit_routing_preference, collapse = "|")
    }
    
    ## check avoid is valid
    if(!all(tolower(avoid) %in% c("tolls","highways","ferries","indoor")) & !is.null(avoid)){
        stop("avoid can only include tolls, highways, ferries or indoor")
    }else{
        if(length(avoid) > 1){
            avoid <- paste0(tolower(avoid), collapse = "+")
        }else{
            avoid <- tolower(avoid)
        }
    }
    
    ## check departure time is valid
    if(!is.null(departure_time) & !inherits(departure_time, "POSIXct"))
        stop("departure_time must be a POSIXct object")
    
    if(!is.null(departure_time)){
        if(departure_time < Sys.time()){
            stop("departure_time must not be in the past")
        }
    }
    
    ## check arrival time is valid
    if(!is.null(arrival_time) & !inherits(arrival_time, "POSIXct"))
        stop("arrival_time must be a POSIXct object")
    
    if(!is.null(arrival_time) & !is.null(departure_time)){
        warning("you have supplied both an arrival_time and a departure_time - only one is allowed. The arrival_time will be ignored")
        arrival_time <- NULL
    }
    
    ## check alternatives is valid
    LogicalCheck(alternatives)
    
    if(!is.null(alternatives))
        alternatives <- tolower(alternatives)
    
    ## check traffic model is valid
    if(!is.null(traffic_model) & is.null(departure_time))
        stop("traffic_model is only accepted with a valid departure_time")
    
    if(!is.null(traffic_model)){
        traffic_model <- match.arg(traffic_model, choices = c("best_guess", "pessimistic","optimistic"))
    }
    
    ## check origin/destinations are valid
    if(information_type == "directions"){
        origin <- fun_check_location(origin, "Origin")
        destination <- fun_check_location(destination, "Destination")
    }else if(information_type == "distance"){
        origin <- fun_check_multiple_locations(origin, "Origins elements")
        destination <- fun_check_multiple_locations(destination, "Destinations elements")
    }
    
    ## check departure time is valid
    departure_time <- ifelse(is.null(departure_time), as.integer(Sys.time()), as.integer(departure_time))
    arrival_time <- as.integer(arrival_time)
    
    ## check waypoints are valid
    if(!is.null(waypoints) & !mode %in% c("driving", "walking","bicycling"))
        stop("waypoints are only valid for driving, walking or bicycling modes")
    
    if(!is.null(waypoints) & class(waypoints) != "list")
        stop("waypoints must be a list")
    
    if(!is.null(waypoints) & !all(names(waypoints) %in% c("stop", "via")))
        stop("waypoint list elements must be named either 'via' or 'stop'")
    
    ## check if waypoints should be optimised, and thefore only use 'stop' as a valid waypoint
    if(optimise_waypoints == TRUE){
        if(any(names(waypoints) %in% c("via")))
            stop("waypoints can only be optimised for stopovers. Each waypoint in the list must be named as stop")
    }
    
    if(!is.null(waypoints)){
        ## construct waypoint string
        # waypoints <- paste0(lapply(waypoints, function(x) fun_check_waypoints(x)), collapse = "|")
        
        waypoints <- sapply(1:length(waypoints), function(x) {
            if(length(names(waypoints)) > 0){
                if(names(waypoints)[x] == "via"){
                    paste0("via:", fun_check_location(waypoints[[x]]))
                }else{
                    ## 'stop' is the default in google, and the 'stop' identifier is not needed
                    fun_check_location(waypoints[[x]])
                }
            }else{
                fun_check_location(waypoints[[x]])
            }
        })
        
        if(optimise_waypoints == TRUE){
            waypoints <- paste0("optimize:true|", paste0(waypoints, collapse = "|"))
        }else{
            waypoints <- paste0(waypoints, collapse = "|")
        }
    }
    
    ## language check
    if(!is.null(language) & (class(language) != "character" | length(language) > 1))
        stop("language must be a single character vector or string")
    
    if(!is.null(language))
        language <- tolower(language)
    
    ## region check
    if(!is.null(region) & (class(region) != "character" | length(region) > 1))
        stop("region must be a two-character string")
    
    if(!is.null(region))
        region <- tolower(region)
    
    ## construct url
    if(information_type == "directions"){
        args <- c("origin" = origin, "destination" = destination)
        
    }else if(information_type == "distance"){
        args <- c("origins" = origin, "destinations" = destination)
    }
    
    args <- c(args, "waypoints" = waypoints,
              "departure_time" = departure_time,
              "arrival_time" = arrival_time,
              "alternatives" = alternatives,
              "avoid" = avoid,
              "units" = units,
              "mode" = mode,
              "transit_mode" = transit_mode,
              "transit_routing_preference" = transit_routing_preference,
              "language" = language,
              "region" = region,
              "key" = key)
    
    map_url <- constructURL(base_url, args)
    
    if(length(map_url) > 1)
        stop("invalid map_url")
    
    return(fun_download_data(map_url, simplify, curl_proxy))
    
}


fun_download_data <- function(map_url, simplify, curl_proxy = NULL){
    
    out <- NULL
    ## check map_url is valid
    if(length(map_url) > 1)
        stop("invalid map_url")
    
    ## check for a valid connection
    if(curl::has_internet() == FALSE)
        stop("Can not retrieve results. No valid internet connection (tested using curl::has_internet() )")
    
    ## if a proxy has been passed in, use it
    if(!is.null(curl_proxy)){
        con <- curl_proxy(map_url)
        out <- readLines(con)
        close(con)
        if(simplify == TRUE){
            out <- jsonlite::fromJSON(out)
        }
        return(out)
    }
    
    if(simplify == TRUE){
        out <- jsonlite::fromJSON(map_url)
    }else{
        # out <- readLines(curl::curl(map_url))
        con <- curl::curl(map_url)
        tryCatch({
            out <- readLines(con)
            close(con)
        },
        error = function(cond){
            close(con)
            warning("There was an error downloading results. Please manually check the following URL is valid by entering it into a browswer. If valid, please file a bug report citing this URL (note: your API key has been removed, so you will need to add that back in) \n\n", gsub("key=.*","",map_url), "key=", sep = "")
        })
    }
    return(out)
}


fun_check_multiple_locations <- function(loc, type){
    loc <- sapply(1:length(loc), function(x) {
        fun_check_location(loc[[x]], type)
    })
    loc <- paste0(loc, collapse = "|")
}


fun_check_location <- function(loc, type){
    if(is.numeric(loc) & length(loc) == 2){
        loc <- paste0(loc, collapse = ",")
    }else if(is.character(loc) & length(loc) == 1){
        loc <- gsub(" ", "+", loc)
    }else{
        stop(paste0(type, " must be either a numeric vector of lat/lon coordinates, or an address string"))
    }
    return(loc)
}

fun_check_address <- function(address){
    if(is.character(address) & length(address) == 1){
        address <- gsub(" ", "+", address)
    }else{
        stop("address must be a string of length 1")
    }
    return(address)
}




% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/google_map_layers.R
\name{add_heatmap}
\alias{add_heatmap}
\title{Add heatmap}
\usage{
    add_heatmap(map, data = get_map_data(map), lat = NULL, lon = NULL,
                weight = NULL, option_gradient = NULL, option_dissipating = FALSE,
                option_radius = 0.01, option_opacity = 0.6, layer_id = NULL,
                digits = 4)
}
\arguments{
    \item{map}{a googleway map object created from \code{google_map()}}
    
    \item{data}{data frame containing at least two columns, one specifying the
        latitude coordinates, and the other specifying the longitude. If Null, the
        data passed into \code{google_map()} will be used.}
    
    \item{lat}{string specifying the column of \code{data} containing the 'latitude'
        coordinates. If left NULL, a best-guess will be made}
    
    \item{lon}{string specifying the column of \code{data} containing the 'longitude'
        coordinates. If left NULL, a best-guess will be made}
    
    \item{weight}{string specifying the column of \code{data} containing the 'weight'
        associated with each point. If NULL, each point will get a weight of 1.}
    
    \item{option_gradient}{vector of colours to use as the gradient colours. see Details}
    
    \item{option_dissipating}{logical Specifies whether heatmaps dissipate on zoom.
        When dissipating is FALSE the radius of influence increases with zoom level to
        ensure that the color intensity is preserved at any given geographic location.
        Defaults to FALSE}
    
    \item{option_radius}{numeric. The radius of influence for each data point, in pixels.}
    
    \item{option_opacity}{The opacity of the heatmap, expressed as a number between
        0 and 1. Defaults to 0.6.}
    
    \item{layer_id}{single value specifying an id for the layer.}
    
    \item{digits}{integer. Use this parameter to specify how many digits (decimal places)
        should be used for the latitude / longitude coordinates.}
}
\description{
    Adds a heatmap to a google map
}
\details{
    \code{option_gradient} colours can be two of the R colour specifications;
    either a colour name (as listed by \code{colors()}, or a hexadecimal string of the
                          form \code{"#rrggbb"}).
    The first colour in the vector will be used as the colour that fades to transparent,
    while the last colour in the vector will be use in the centre of the 'heat'.
}
\examples{
    \dontrun{
        
        map_key <- 'your_api_key'
        
        set.seed(20170417)
        df <- tram_route
        df$weight <- sample(1:10, size = nrow(df), replace = T)
        
        google_map(key = map_key, data = df) \%>\%
            add_heatmap(lat = "shape_pt_lat", lon = "shape_pt_lon", weight = "weight",
                        option_radius = 0.001)
        
        ## specifying different colour gradient
        option_gradient <- c('orange', 'blue', 'mediumpurple4', 'snow4', 'thistle1')
        
        google_map(key = map_key, data = df) \%>\%
            add_heatmap(lat = "shape_pt_lat", lon = "shape_pt_lon", weight = "weight",
                        option_radius = 0.001, option_gradient = option_gradient)
        
    }
}


#' Google map
#'
#' Generates a google map object
#'
#' The data argument is only needed if you call other functions to add layers to the map, such as \code{add_markers()} or \code{add_polylines}. However, the data argument can also be passed into those functions as well.
#'
#' In order to use Google Maps you need a valid Google Maps Web JavaScript API key. See the Google Maps API documentation \url{https://developers.google.com/maps/}
#'
#' @import htmlwidgets
#' @import htmltools
#' @import shiny
#'
#' @aliases googleway
#' @param key A valid Google Maps API key. see Details
#' @param data data to be used on the map. This will likely contain two columns for latitude and longitude, and / or encoded polylines for plotting polylines and polygons
#' @param location \code{numeric} vector of latitude/longitude (in that order) coordinates for the initial starting position of the map. The map will automatically set the location and zoom if markers are supplied through \link{add_markers}. If null, the map will default to Melbourne, Australia.
#' @param zoom \code{integer} representing the zoom level of the map (0 is fully zoomed out)
#' @param width the width of the map
#' @param height the height of the map
#' @param padding the padding of the map
#' @param styles JSON string representation of a valid Google Maps styles Array. See the Google documentation for details \url{https://developers.google.com/maps/documentation/javascript/styling}
#' @param search_box \code{boolean} indicating if a search box should be placed on the map
#' @param zoom_control logical
#' @param map_type_control logical
#' @param scale_control logical
#' @param street_view_control logical
#' @param rotate_control logical
#' @param fullscreen_control logical
#' @examples
#' \dontrun{
#'
#' map_key <- "your_api_key"
#' df <- structure(list(lat = c(-37.8201904296875, -37.8197288513184,
#' -37.8191299438477, -37.8187675476074, -37.8186187744141, -37.8181076049805
#' ), lon = c(144.968612670898, 144.968414306641, 144.968139648438,
#' 144.967971801758, 144.967864990234, 144.967636108398), weight = c(31.5698964400217,
#' 97.1629025738221, 58.9051092562731, 76.3215389118996, 37.8982300488278,
#' 77.1501972114202), opacity = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2)), .Names = c("lat",
#' "lon", "weight", "opacity"), row.names = 379:384, class = "data.frame")
#'
#' google_map(key = map_key, data = df_line) %>%
#'  add_markers() %>%
#'  add_heatmap() %>%
#'  add_traffic()
#'
#' ## style map using 'cobalt simplified' style
#' style <- '[{"featureType":"all","elementType":"all","stylers":[{"invert_lightness":true},
#' {"saturation":10},{"lightness":30},{"gamma":0.5},{"hue":"#435158"}]},
#' {"featureType":"road.arterial","elementType":"all","stylers":[{"visibility":"simplified"}]},
#' {"featureType":"transit.station","elementType":"labels.text","stylers":[{"visibility":"off"}]}]'
#' google_map(key = map_key, styles = style)
#'
#' }
#'
#'
#' @export
google_map <- function(key,
                       data = NULL,
                       location = NULL,
                       zoom = NULL,
                       width = NULL,
                       height = NULL,
                       padding = 0,
                       styles = NULL,
                       search_box = FALSE,
                       zoom_control = TRUE,
                       map_type_control = TRUE,
                       scale_control = FALSE,
                       street_view_control = TRUE,
                       rotate_control = TRUE,
                       fullscreen_control = TRUE) {
    
    if(is.null(location))
        location <- c(-37.9, 144.5)  ## Melbourne, Australia
    
    if(is.null(zoom))
        zoom <- 8
    
    # forward options using x
    x = list(
        lat = location[1],
        lng = location[2],
        zoom = zoom,
        styles = styles,
        search_box = search_box,
        zoomControl = zoom_control,
        mapTypeControl = map_type_control,
        scaleControl = scale_control,
        streetViewControl = street_view_control,
        rotateControl = rotate_control,
        fullscreenControl = fullscreen_control
    )
    
    # create widget
    googlemap <- htmlwidgets::createWidget(
        name = 'google_map',
        x = structure(
            x,
            google_map_data = data
        ),
        package = 'googleway',
        width = width,
        height = height,
        
        sizingPolicy = htmlwidgets::sizingPolicy(
            defaultWidth = '100%',
            defaultHeight = 800,
            padding = padding,
            browser.fill = FALSE
        )
    )
    
    # if(search_box == TRUE){
    header <- paste0('<script src="https://maps.googleapis.com/maps/api/js?key=',
                     key, '&libraries=visualization,geometry,places"></script>')
    # }else{
    #   header <- paste0('<script src="https://maps.googleapis.com/maps/api/js?key=',
    #                    key, '&libraries=visualization,geometry"></script>')
    # }
    
    googlemap$dependencies <- c(
        googlemap$dependencies,
        list(
            htmltools::htmlDependency(
                name = "googleway",
                version = "9999",
                src=".",
                head = header,
                all_files = FALSE
            )
        )
    )
    
    return(googlemap)
}

get_map_data = function(map){
    attr(map$x, "google_map_data", exact = TRUE)
}

#' Shiny bindings for google_map
#'
#' Output and render functions for using google_map within Shiny applications and interactive Rmd documents.
#'
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a google_map
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @name google_map-shiny
#' @examples
#' \dontrun{
#' library(shiny)
#' library(googleway)
#'
#' ui <- fluidPage(google_mapOutput("map"))
#'
#' server <- function(input, output, session){
#'
#' api_key <- "your_api_key"
#'
#'   df <- structure(list(lat = c(-37.8201904296875, -37.8197288513184,
#'   -37.8191299438477, -37.8187675476074, -37.8186187744141, -37.8181076049805
#'   ), lon = c(144.968612670898, 144.968414306641, 144.968139648438,
#'   144.967971801758, 144.967864990234, 144.967636108398), weight = c(31.5698964400217,
#'   97.1629025738221, 58.9051092562731, 76.3215389118996, 37.8982300488278,
#'   77.1501972114202), opacity = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2)), .Names = c("lat",
#'   "lon", "weight", "opacity"), row.names = 379:384, class = "data.frame")
#'
#'   output$map <- renderGoogle_map({
#'     google_map(key = api_key)
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @export
google_mapOutput <- function(outputId, width = '100%', height = '400px'){
    htmlwidgets::shinyWidgetOutput(outputId,
                                   'google_map',
                                   width,
                                   height,
                                   package = 'googleway')
}

#' @rdname google_map-shiny
#' @export
renderGoogle_map <- function(expr, env = parent.frame(), quoted = FALSE) {
    if (!quoted) { expr <- substitute(expr) } # force quoted
    htmlwidgets::shinyRenderWidget(expr, google_mapOutput, env, quoted = TRUE)
}


# google_map_html <- function(id, style, class, ...){
#   list(
#       tags$div(id = id, class = class, style = style,
#                tags$div(id = "search-container", class = "inner-addon right-addon",
#                         tags$input(id = "pac-input", class = "controls", type = "text"),
#                         tags$span(id = "search-clear", class="glyphicon glyphicon-remove-cirlce")))
#       )
# }

