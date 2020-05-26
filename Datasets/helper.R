# Function to customise the ggplot2 scheme
theme_base_edited <- function (){
    require(ggthemes)
    ggthemes::theme_base()  %+replace% 
        theme(legend.text = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.position = "bottom",
              axis.text = element_text(colour="black", size = 12),
              axis.title = element_text(size = 12),#,
              plot.title = element_text(size = 12),
              plot.background = element_rect(colour = NA, size = 4, fill=NA),
              strip.text = element_text(size = 12)
        )
}

# Functions to produce texreg tables

aggregate.matrix <- function(models, gof.names=NA, custom.gof.names=NA, digits=2, 
    returnobject = "m", coef.recode=NULL) {

  # aggregate GOF statistics in a matrix and create list of coef blocks
  gofs <- matrix(nrow = length(gof.names), ncol = length(models))
  row.names(gofs) <- gof.names
  coefs <- list()
  decimal.matrix <- matrix(nrow = length(gof.names), ncol = length(models))
  for (i in 1:length(models)) {
    cf <- models[[i]]@coef
    se <- models[[i]]@se
    pv <- models[[i]]@pvalues
    cil <- models[[i]]@ci.low
    ciu <- models[[i]]@ci.up
    if (length(se) == 0) {
      coef <- cbind(cf, cil, ciu)
    } else {
      if (length(pv) > 0) {
        coef <- cbind(cf, se, pv)
      } else { #p-values not provided -> use p-values of 0.99
        coef <- cbind(cf, se, rep(0.99, length(cf)))
      }
    }
    rownames(coef) <- models[[i]]@coef.names
    if(!is.null(coef.recode))
        rownames(coef) <- car::recode(rownames(coef), coef.recode)
    coefs[[i]] <- coef
    if (length(models[[i]]@gof) > 0) {
      for (j in 1:length(models[[i]]@gof)) {
        rn <- models[[i]]@gof.names[j]
        val <- models[[i]]@gof[j]
        col <- i
        if (is.na(models[[i]]@gof.decimal[j])) {
          dec <- digits
        } else if (models[[i]]@gof.decimal[j] == FALSE) {
          dec <- 0
        } else {
          dec <- digits
        }
        row <- which(row.names(gofs) == rn)
        gofs[row, col] <- val
        decimal.matrix[row, col] <- dec
      }
    }
  }
  
  # figure out correct order of the coefficients
  coef.order <- character()
  for (i in 1:length(coefs)) {
    for (j in 1:length(rownames(coefs[[i]]))) {
      if (!rownames(coefs[[i]])[j] %in% coef.order) {
        coef.order <- append(coef.order, rownames(coefs[[i]])[j])
      }
    }
  }
  
  # merge the coefficient tables
  if (length(coefs) == 1) {
    m <- coefs[[1]]
  } else if (length(coefs) > 1) {
    m <- coefs[[1]]
    for (i in 2:length(coefs)) {
      m <- merge(m, coefs[[i]], by = 0, all = TRUE)
      rownames(m) <- m[, 1]
      m <- m[, colnames(m) != "Row.names"]
      colnames(m) <- NULL
    }
  }
  colnames(m) <- rep(colnames(coefs[[1]]), length(coefs))
  
  # reorder merged coefficient table
  m.temp <- matrix(nrow = nrow(m), ncol = ncol(m))
  for (i in 1:nrow(m)) {
    new.row <- which(coef.order == rownames(m)[i])
    for (j in 1:length(m[i,])) {
      m.temp[new.row, j] <- m[i, j]
    }
  }
  rownames(m.temp) <- coef.order
  colnames(m.temp) <- colnames(m)
  m <- m.temp
  
  if (returnobject == "m") {
    return(m)
  } else if (returnobject == "gofs") {
  
    #replace GOF names by custom names
    if (is.null(custom.gof.names)) {
      #do nothing
    } else if (class(custom.gof.names) != "character") {
      stop("Custom GOF names must be provided as a vector of strings.")
    } else if (length(custom.gof.names) != length(gof.names)) {
      stop(paste("There are", length(gof.names), 
          "GOF statistics, but you provided", length(custom.gof.names), 
          "custom names for them."))
    } else {
      custom.gof.names[is.na(custom.gof.names)] <- 
          rownames(gofs)[is.na(custom.gof.names)]
      rownames(gofs) <- custom.gof.names
    }
    
    return(gofs)
    
  } else if (returnobject == "decimal.matrix") {
    return(decimal.matrix)
  }
}

  