named_entity <- function(text.var, entity.annotator, word.annotator = word_annotator(),
                         element.chunks = floor(2000 * (23.5/mean(sapply(text.var, nchar), na.rm = TRUE)))){
    
    len <- length(text.var)
    
    ## locate empty or missing text elements
    nas <- sort(union(which(is.na(text.var)), grep("^\\s*$", text.var)))
    
    ## Get annotator
    entity.annotator <- switch(entity.annotator,
                               person_annotator = Maxent_Entity_Annotator(kind = "person"),
                               location_annotator = Maxent_Entity_Annotator(kind = "location"),
                               organization_annotator = Maxent_Entity_Annotator(kind = "organization"),
                               date_annotator = Maxent_Entity_Annotator(kind = "date"),
                               money_annotator = Maxent_Entity_Annotator(kind = "money"),
                               percent_annotator  = Maxent_Entity_Annotator(kind = "percentage"),
                               stop("`entity.annotator` does not appear to be an annotator.  See `?annotators`.")
    )
    
    
    ## replace empty text with a period
    if(length(nas) > 0){
        text.var[nas] <- "."
    }
    
    ## Chunking the text into memory sized chunks:
    ## caluclate the start/end indexes of the chunks
    ends <- c(utils::tail(seq(0, by = element.chunks,
                              length.out = ceiling(len/element.chunks)), -1), len)
    starts <- c(1, utils::head(ends + 1 , -1))
    
    ## chunk the text
    text_list <- Map(function(s, e) {text.var[s:e]}, starts, ends)
    
    ## loop through the chunks and tag them
    out <- lapply(text_list, function(x){
        x <- entify(x, entity.annotator, word.annotator)
        gc()
        x
    })
    
    lens <- sapply(text_list, length)
    
    out <- unlist(lapply(seq_along(out), function(i){
        
        vectout <- vector(mode = "list", length = lens[i])
        if (is.null(out[[i]][["entities"]])) return(vectout)
        if (length(out[[i]][["entities"]]) == 1){
            splits <- out[[i]][["entities"]]
        } else {
            splits <- split(out[[i]][["entities"]], out[[i]][["locations"]])
        }
        vectout[unique(out[[i]][["locations"]])] <- splits
        vectout
    }), recursive = FALSE)
    
    class(out) <- c("entity", class(out))
    attributes(out)[["type"]] <- attributes(entity.annotator)[["type"]]
    out
}