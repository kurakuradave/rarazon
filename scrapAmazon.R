options(stringsAsFactors=FALSE)




library( RSelenium )




### Define Scroll Page Down JS script
script <- "window.scrollTo(0, Math.max(document.documentElement.scrollHeight, document.body.scrollHeight, document.documentElement.clientHeight));"
### Scroll Down
scrollDownXTimes <- function( x ){
    for( i in 1:x ){
        remDr$executeScript(script, args = list())
        Sys.sleep(2)
    }
}




getTboundary <- function(pcs){
    ret <- which(grepl("Verified\\sPurchase",pcs))
    if(length(ret)==0){ret<-length(pcs)}
    return( ret )
}

getHboundary <- function( pcs ){
    ret <- which(grepl("^By.*on.*\\d{4}$",pcs))
    if(length(ret)==0){ret<- -1}
    return( ret )
}




parseWeight <- function( aString, aType ) {
    if( length(aString)==0 ){ aString <- 0 }
    if( aType=="amazon" ){
        aString <- regmatches( aString, regexpr( "^(One|\\d+)", aString ) )
        if(aString=="One"){ aString <- "1" }
    }
    return( aString )
}




parseComments <- function( aString, aType ){
    if( length(aString)==0 ){aString<-"0"}
    if(aType=="amazon"){
        aString <- regmatches(aString, regexpr("\\d+", aString))
    }
    return(aString)
}





chopTail <- function( aReview, aType ){
    if(length(aReview)==0){aReview<-""}
    if(aType=="amazon"){

        endPos <- nchar(aReview)

        pplFoundHelpfulPos <- regexpr( "(One|\\d+)\\s(person|people)\\sfound\\sthis\\shelpful", aReview, ignore.case=TRUE, perl=TRUE )[ 1 ] - 2

        hasCommentsPos <- regexpr( "\\n\\d+\\scomments\\n", aReview )[ 1 ] - 1
        
        postCommentPos <- regexpr( "\\nComment\\nWas\\sthis\\sreview\\shelpful\\sto\\syou\\?\\nYes\\nNo\\n", aReview )[ 1 ] - 1
        
        wasHelpfulPos <-  regexpr( "\\nWas\\sthis\\sreview\\shelpful\\sto\\syou\\?\\nYes\\nNo\\n", aReview )[ 1 ] - 1

        cEndPos <- c( endPos, pplFoundHelpfulPos, hasCommentsPos, postCommentPos, wasHelpfulPos )
        cEndPos <- cEndPos[ cEndPos > 0 ]
        endPos <- min(cEndPos)
               
        aReview <- substr( aReview,
                           0,
                           endPos
                         )
    }
    ## take out subproductname and verified purchase
    pcs <- unlist( strsplit( aReview, "\\n" ) )
    tboundary <- getTboundary(pcs)
    hboundary <- getHboundary(pcs)
    aReview <- paste( c(pcs[1:hboundary], pcs[(tboundary+1):length(pcs)] ), collapse="\n" )
    return( aReview )
}



parseUsername <- function( aString, aType ){
    if(length(aString)==0){aString<-""}
    if(aType=="amazon"){
        aString <- substr( aString,
                           3,
                          regexpr("(january|february|march|april|may|june|july|august|september|october|november|december\\s\\d{1,2}\\,\\s\\d{4})",
                                  aString,
                                  ignore.case=TRUE,
                                  perl=TRUE
                                 )[1] - 4
                         )
    }
    return( aString )
}





parseReviewDate <- function( aString, aType ) {
    if(length(aString)==0){aString<-""}
    if(aType=="amazon"){
        aString <- regmatches( aString,
                          regexpr("(january|february|march|april|may|june|july|august|september|october|november|december)\\s\\d{1,2}\\,\\s\\d{4}",
                                  aString,
                                  ignore.case = TRUE,
                                  perl = TRUE
                                )
                            )    
    }
    return(aString)
}




parseMisc <- function( aString, aType ){
    if(length(aString)==0){aString<-""}
    if(aType == "amazon"){
        pcs <- unlist( strsplit( aString, "\\n" ) )
        tboundary <- getTboundary( pcs )
        hboundary <- getHboundary( pcs )

        if((tboundary-hboundary>1) & (hboundary > 0)){
            aString <- pcs[tboundary-1]
        } else {
            aString <- ""
        }
    }
    return( aString )
}





parseVerifiedPurchase <- function( aString, aType ){
    if(length(aString)==0){aString<-""}
    if(aType == "amazon"){
        if(grepl("Verified\\sPurchase",aString)){
            aString <- "Verified Purchase"
        }
    }
    return( aString )
}




parseReviewTitle <- function( aString, aType ){
    if(length(aString)==0){aString<-""}
    if(aType=="amazon"){

    }
    return( aString )
}




parseReviewBody <- function( aVector, aType ){
    ret <- ""
    if( aType=="amazon" ){
        ret <- paste( aVector, collapse="\n" )
    }
    return( ret )
}




hasOpeningRemarks <- function( rawPieces ){
    ret <- FALSE
    if( grepl( "^By.*on.*\\d{4}$", rawPieces[2] )==FALSE ){
        ret <- TRUE
    }
    return( ret )
}




buildAmazonDF <- function( aVector ){
    ret <- lapply( aVector, function(x){
        weight <- parseWeight( regmatches( x, regexpr( "(One|\\d+)\\s(person|people)\\sfound\\sthis\\shelpful", x, ignore.case=TRUE, perl=TRUE ) ), "amazon" )

        comments <- parseComments( regmatches( x, regexpr( "\\n\\d+\\scomment(s)?\\nReport\\sabuse$", x ) ), "amazon" )

        misc1 <- parseMisc( regmatches( x, regexpr( "\\n.*?\\nVerified\\sPurchase\\n", x ) ), "amazon" )    

        misc2 <- parseVerifiedPurchase( regmatches( x, regexpr( "\\n.*?\\nVerified\\sPurchase\\n", x ) ), "amazon" )

        misc3 <- ""

        x <- chopTail( x, "amazon" )

        rawPieces <- unlist( strsplit( x, "\n" ) )

        ## handle opening remarks
        if( hasOpeningRemarks( rawPieces ) ){
            misc3 <- rawPieces[1]
            rawPieces <- rawPieces[-1]
        }
        
        username <- parseUsername( rawPieces[2], "amazon" )
        
        reviewdate <- parseReviewDate( rawPieces[2], "amazon" )
        
        reviewTitle <- parseReviewTitle( rawPieces[ 1 ], "amazon" )
        
        reviewBody <- parseReviewBody( rawPieces[ 3:length(rawPieces) ], "amazon" )
        ## DEBUG
        #print(username)
        #print(reviewdate)
        #print(weight)
        #print(comments)
        #print(misc1)
        #print(misc2)
        #print(misc3)
        #print(reviewTitle)
        #print(reviewBody)

        aRow <- data.frame( title = reviewTitle,
                           body = reviewBody,
                           user = username,
                           date = reviewdate,
                           weight = weight,
                           comments = comments,
                           misc1 = misc1,
                           misc2 = misc2,
                           misc3 = misc3
                           )
        return( aRow )
    } )
    ret <- do.call("rbind", ret) ## this creates a df
    return( ret )
}




populateDF <- function( holderDF, additionalDF ) {
## avoid doing rbind on empty dataframe
    if(nrow(holderDF)==0){
        holderDF <- additionalDF
    } else{
        holderDF <- rbind( holderDF, additionalDF )
    }
    return( holderDF )
}




scrapProductReviewAmazon <- function( aURL ){
    ### Connect to Selenium Docker Image
    remDr <- remoteDriver(port = 4446L)
    remDr$open()

    ## initialize variables
    sDF <- data.frame()
    pageNum <- 0
    rawVector <- c()
    hasNextPage <- TRUE
    nextElemIdentified <- FALSE

    ## navigate to root page
    remDr$navigate( aURL )

    ## scrap all available pages
    while( hasNextPage ) {
        if(nextElemIdentified){
            webElem$clickElement()
            nextElemIdentified <- FALSE
        }
        pageNum <- pageNum + 1
        print( paste( "processing page", pageNum, remDr$getTitle() ) )
        Sys.sleep(3)
        webElems <- remDr$findElements( "xpath", "//div[contains(concat(' ', @data-hook, ' '), 'review')]" )    
        
        rawReviews <- unlist( lapply( webElems, function(x){ x$getElementText() } ) )
        rawVector <- c( rawVector, rawReviews )
        print( paste( "Adding", length(rawReviews), "Reviews from Page", pageNum ) )
        
        webElem <- remDr$findElement( "xpath", "//li[contains(concat(' ', @class, ' '), 'last')]/a" )
        nextElemIdentified <- TRUE
        nextURL <- webElem$getElementAttribute("href")
        
        if( grepl( "pageNumber\\=\\d+", nextURL ) ){
            hasNextPage <- TRUE
        } else {
            hasNextPage <- FALSE
        }
    }
    print( paste( "In Total, ", length(rawVector), "Reviews Obtained." ) )
    ## DEBUG
    ## saveRDS( rawVector, './rawVector.rds' )
    ## convert to dataframe
    sDF <- populateDF( sDF, buildAmazonDF( rawVector ) )
    
    return( sDF )
}




################################
###      START PROGRAM       ###
################################

myReviews3 <- scrapProductReviewAmazon( "https://www.amazon.com/Roland-FP-30-DIGITAL-PIANO-Black/product-reviews/B01B3FBDC4/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews" )

write.csv( myReviews3, './Roland_FP30.csv' )

myReviews4 <- scrapProductReviewAmazon("https://www.amazon.com/Remo-HD-8510-00-Fiberskyn-Frame-Drum/product-reviews/B0002F7KGK/ref=pd_lpo_vtph_267_bs_lp_cr_1/146-9278367-8150568?ie=UTF8&refRID=Y250MAMQPZWMCPWZSE2V")

write.csv( myReviews4, "./Remo_Frame_Drum.csv" )

myReviews1 <- scrapProductReviewAmazon( "https://www.amazon.com/Alesis-Recital-Beginner-Full-Size-Semi-Weighted/product-reviews/B01DZXE9NC/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews" )

write.csv( myReviews1, "./Alesis_Recital.csv" )

myReviews2 <- scrapProductReviewAmazon("https://www.amazon.com/Alesis-VORTEXWIRELESS2-Midi-Controller/product-reviews/B078S9L1VZ/ref=cm_cr_dp_d_acr_sr?ie=UTF8&reviewerType=all_reviews")

write.csv( myReviews2, "./Alesis_Vortex_Wireless_2.csv" )


myReviews5 <- scrapProductReviewAmazon("https://www.amazon.com/Meinl-Percussion-FD18SD-TF-18-Inch-Synthetic/product-reviews/B0033PQU3A/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews")

write.csv(myReviews5, "./Meinl_Sea_Drum.csv")

myReviews6 <- scrapProductReviewAmazon("https://www.amazon.com/Meinl-Percussion-FD18T-D-18-Inch-African/product-reviews/B0033PQU44/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews")

write.csv( "./Meinl_Deep_Shell_Tar_Natural.csv" )

myReviews7 <- scrapProductReviewAmazon("https://www.amazon.com/Remo-Ocean-Drum-Standard-22/product-reviews/B0002F7KS8/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews")

write.csv(myReviews7, "./Remo_Ocean_Drum.csv")
