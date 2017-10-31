
#' rnbi.qqplot.single
#'
#' Draw the qqplot for single data for idat analysis
#' Takes in a differential methlytion p-value data frame and return a plot object which can be used to draw qqplot of p-values using plotly
#' @param x a dataframe of differential methlytion p-value
#' @return return a plot object which can be used to draw qqplot of p-values using plotly
#' @export

rnbi.qqplot.single <- function(x) {
  col = "#252525"
  size = 1
  type = 20
  abline_col = "red"
  abline_size = 0.5
  abline_type = 1
  highlight = NULL
  highlight_color = "#00FF00"
  xlab = "Expected -log10(p)"
  ylab = "Observed -log10(p)"
  title = "Q-Q Plot"

  setnames(x, "diffmeth.p.val", "P")

  qqr <- rnbi.qqplot.data.single(x )

  d2 <- qqr$data

  # plot
  p <- ggplot2::ggplot(data = d2, ggplot2::aes_string(x = 'EXPECTED', y = 'OBSERVED',text = 'CGID')) +
    geom_point() +
    ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1),
                         size = abline_size,
                         color = abline_col,
                         linetype = abline_type) +


    ggplot2::labs(x = xlab,
                  y = ylab,
                  title = title)

  p

}



########################################################################################################################


#' rnbi.qqplot.data.single
#' For Idat analysis
#' convert the p-values of a one analysis in to an object of class qqplot.data which will be used to draw qqplot .
#'
#' @param x a dataframe of differential methlytion p-value
#' @param p column consist of differential methylation p-values
#' @return return an object of class qqplot.data which will be used to draw qqplot .
#' @export

rnbi.qqplot.data.single <- function(x,p = "P") {

  ## Make sure you have p column exists in x.
  if (!(p %in% names(x))) stop(paste("Column", p, "not found 'x' data.frame"))

  # Check for numeric p
  if (!is.numeric(x[[p]])) stop(sprintf("p argument specified as %s but this column is not numeric in the 'x' data.frame", p))

  # Check if any p are not in (0,1)
  if (any(x[[p]]<0)) stop("Negative p-values found. These must be removed.")
  if (any(x[[p]]>1)) stop("P-values greater than 1 found. These must be removed.")
  if (any(is.na(x[[p]]))) stop("NA P-values found. These must be removed")


  # Create a new data.frame with columns called P.
  d <- data.frame(P = x[[p]])

  d[["CGID"]] <- x[["cgid"]]

  # sort d by decreasing p-value
  d <- d[order(d[["P"]] ,decreasing = FALSE), , drop = FALSE]

  # Observed and expected
  d[["OBSERVED"]] <- -log10(d[["P"]])
  d[["EXPECTED"]] <- -log10(stats::ppoints(length(d[["P"]])))

  # droping the P column
  d <- d[,-(1),drop=FALSE]

  qqplot.data <- list(data = d)

  class(qqplot.data) <- "qqplot.data"

  qqplot.data

}

########################################################################################################################

#' rnbi.qqplot.single.rrbs
#'
#' Draw the qqplot for single data for RRBS analysis
#'
#' @param x a dataframe of differential methlytion p-value
#' @return return an object to draw the qqplot for single data for RRBS analysis
#' @export

rnbi.qqplot.single.rrbs <- function(x) {
  col = "#252525"
  size = 1
  type = 20
  abline_col = "red"
  abline_size = 0.5
  abline_type = 1
  highlight = NULL
  highlight_color = "#00FF00"
  xlab = "Expected -log10(p)"
  ylab = "Observed -log10(p)"
  title = "Q-Q Plot"


  setnames(x, "diffmeth.p.val", "P")

  qqr <- rnbi.qqplot.data.single.rrbs(x )

  d2 <- qqr$data

  # plot
  p <- ggplot2::ggplot(data = d2, ggplot2::aes_string(x = 'EXPECTED', y = 'OBSERVED',text = 'Chromosome')) +
    geom_point() +
    ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1),
                         size = abline_size,
                         color = abline_col,
                         linetype = abline_type) +


    ggplot2::labs(x = xlab,
                  y = ylab,
                  title = title)

  p

}



########################################################################################################################

#' rnbi.qqplot.data.single
#' For RRBS analysis
#' convert the p-values of a one analysis in to an object of class qqplot.data which will be used to draw qqplot .
#'
#' @param x a dataframe of differential methlytion p-value
#' @param p column consist of differential methylation p-values
#' @return return an object of class qqplot.data which will be used to draw qqplot for RRBS analysis
#' @export

rnbi.qqplot.data.single.rrbs <- function(x,p = "P") {

  ## Make sure you have p column exists in x.
  if (!(p %in% names(x))) stop(paste("Column", p, "not found 'x' data.frame"))

  # Check for numeric p
  if (!is.numeric(x[[p]])) stop(sprintf("p argument specified as %s but this column is not numeric in the 'x' data.frame", p))

  # Check if any p are not in (0,1)
  if (any(x[[p]]<0)) stop("Negative p-values found. These must be removed.")
  if (any(x[[p]]>1)) stop("P-values greater than 1 found. These must be removed.")
  if (any(is.na(x[[p]]))) stop("NA P-values found. These must be removed")


  # Create a new data.frame with columns called P.
  d <- data.frame(P = x[[p]])
  d[["Chromosome"]] <- x[["Chromosome"]]

  # sort d by decreasing p-value
  d <- d[order(d[["P"]] ,decreasing = FALSE), , drop = FALSE]

  # Observed and expected
  d[["OBSERVED"]] <- -log10(d[["P"]])
  d[["EXPECTED"]] <- -log10(stats::ppoints(length(d[["P"]])))

  # droping the P column
  d <- d[,-(1),drop=FALSE]

  qqplot.data <- list(data = d)

  class(qqplot.data) <- "qqplot.data"

  qqplot.data

}





########################################################################################################################

#' rnbi.qqplot.double
#'
#' draw qqplot for two analysis
#'
#' @param x a dataframe of differential methlytion p-value from one analysis
#' @param y a dataframe of differential methlytion p-value from second analysis
#' @return return an object to draw qqplot for two analysis simultaneously using plotly
#' @export

rnbi.qqplot.double <- function(x,y) {
  col = "#252525"
  size = 1
  type = 20
  abline_col = "red"
  abline_size = 0.5
  abline_type = 1
  highlight = NULL
  highlight_color = "#00FF00"
  xlab = "Expected -log10(p)"
  ylab = "Observed -log10(p)"
  title = "Q-Q Plot"


  qqr <- rnbi.qqplot.data.double(x,y )

  d2 <- qqr$data

  # Everything on the same plot
  p <- ggplot(d2, aes(expected,observed, col=analysis , text = cgid)) +
    geom_point() +
    ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1),
                         size = abline_size,
                         color = abline_col,
                         linetype = abline_type) +


    ggplot2::labs(x = xlab,
                  y = ylab,
                  title = title)



  p

}

########################################################################################################################

#' rnbi.qqplot.data.double
#'
#' convert the p-values of the two analysis in to an object of class qqplot.data which will be used to draw qqplot.
#'
#' @param x a dataframe of differential methlytion p-value from one analysis
#' @param y a dataframe of differential methlytion p-value from second analysis
#' @param p column consist of differential methylation p-values
#' @return return an object of class qqplot.data which will be used to draw qqplot for two analysis
#' @export


rnbi.qqplot.data.double <- function(x,y,
                                    p = "diffmeth.p.val",

                                    ...) {

  x <- data.frame(P = x[[p]], cgid = x[["cgid"]])
  y <- data.frame(P = y[[p]], cgid = y[["cgid"]])

  # sort d by decreasing p-value
  x <- x[order(x[["P"]] ,decreasing = FALSE), , drop = FALSE]
  y <- y[order(y[["P"]] ,decreasing = FALSE), , drop = FALSE]

  # Create a new data.frame with columns called P.
  d <- data.frame(P = x[["P"]] , Q = y[["P"]])


  # Observed and expected
  d[["Analysis_1"]] <- -log10(d[["P"]])
  d[["EXPECTED"]] <- -log10(stats::ppoints(length(d[["P"]]) ))

  d[["Analysis_2"]] <- -log10(d[["Q"]])
  d[["EXPECTEDQ"]] <- -log10(stats::ppoints(length(d[["Q"]])))


  # making a customized dataframe with the columns and values needed to display in the plot
  # using the melt function from the library reshape that allows to combine columns
  ########################################################################################

  # needed the analysis names and oberseved values
  df1 <- data.frame(id = 1:nrow(d) , analysis1 = d[["Analysis_1"]] , analysis2 = d[["Analysis_2"]])
  m <- melt(df1, id=c("id"))

  # needed the expected values
  df2 <- data.frame(id = 1:nrow(d) , expected1 = d[["EXPECTED"]] , expected2 = d[["EXPECTEDQ"]])
  e <- melt(df2, id=c("id"))

  # needed the cgids to display as text in the plot
  df3 <- data.frame(id = 1:nrow(d) , cgid1 = x[["cgid"]] , cgid2 = y[["cgid"]])
  f <- melt(df3, id=c("id"))
  f

  # final customized data frame
  df4 <- data.frame(analysis = m$variable  , observed = m$value, expected = e$value , cgid = f$value)

  ########################################################################################

  #df4
  #analysis   observed   expected	 cgid
  #-----------------------------------------
  #analysis1	 10.64	    2.30	  cg00645010
  #analysis1	 8.84	      1.82	  cg27534567
  #analysis2	 8.23	      1.60	  cg01070250
  #analysis2   8.23	      1.60	  cg01070250
  ########################################################################################


  qqplot.data <- list(data = df4)

  class(qqplot.data) <- "qqplot.data"

  qqplot.data

}


########################################################################################################################

#' rnbi.qqplot.double.rrbs
#'  For RRBS analysis
#' draw qqplot for two analysis
#'
#' @param x a dataframe of differential methlytion p-value from one analysis
#' @param y a dataframe of differential methlytion p-value from second analysis
#' @return return an object to draw qqplot for two analysis simultaneously using plotly for RRBS analysis
#' @export

rnbi.qqplot.double.rrbs <- function(x,y) {
  col = "#252525"
  size = 1
  type = 20
  abline_col = "red"
  abline_size = 0.5
  abline_type = 1
  highlight = NULL
  highlight_color = "#00FF00"
  xlab = "Expected -log10(p)"
  ylab = "Observed -log10(p)"
  title = "Q-Q Plot"



  qqr <- rnbi.qqplot.data.double.rrbs(x,y )

  d2 <- qqr$data


  # Everything on the same plot
  p <- ggplot(d2, aes(expected,observed, col=analysis, text = chromosome)) +
    geom_point() +
    ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1),
                         size = abline_size,
                         color = abline_col,
                         linetype = abline_type) +


    ggplot2::labs(x = xlab,
                  y = ylab,
                  title = title)



  p

}

########################################################################################################################

#' rnbi.qqplot.data.double.rrbs
#'  For RRBS analaysis
#' convert the p-values of the two analysis in to an object of class qqplot.data which will be used to draw qqplot.
#'
#' @param x a dataframe of differential methlytion p-value from one analysis
#' @param y a dataframe of differential methlytion p-value from second analysis
#' @param p column consist of differential methylation p-values
#' @return return an object of class qqplot.data which will be used to draw qqplot for two RRBS analysis
#' @export

rnbi.qqplot.data.double.rrbs <- function(x,y,
                                         p = "diffmeth.p.val",

                                         ...) {

  x <- data.frame(P = x[[p]], Chromosome = x[["Chromosome"]])
  y <- data.frame(P = y[[p]], Chromosome = y[["Chromosome"]])

  # sort d by decreasing p-value
  x <- x[order(x[["P"]] ,decreasing = FALSE), , drop = FALSE]
  y <- y[order(y[["P"]] ,decreasing = FALSE), , drop = FALSE]


  x_length = length(x[["P"]])

  y_p <- y[["P"]]
  y_p <- y_p[1: x_length]

  y_chromo <- y[["Chromosome"]]
  y_chromo <- y_chromo[1: x_length]

  # Create a new data.frame with columns called P.
  d <- data.frame(P = x[["P"]] , Q = y_p)

  d[["Chromosome"]] <- x[["Chromosome"]]
  d[["ChromosomeQ"]] <- y_chromo

  # Observed and expected
  d[["Analysis_1"]] <- -log10(d[["P"]])
  d[["EXPECTED"]] <- -log10(stats::ppoints(length(d[["P"]]) ))

  d[["Analysis_2"]] <- -log10(d[["Q"]])
  d[["EXPECTEDQ"]] <- -log10(stats::ppoints(length(d[["Q"]])))


  # making a customized dataframe with the columns and values needed to display in the plot
  # using the melt function from the library reshape that allows to combine columns
  ########################################################################################

  # needed the analysis names and oberseved values
  df1 <- data.frame(id = 1:nrow(d) , analysis1 = d[["Analysis_1"]] , analysis2 = d[["Analysis_2"]])
  m <- melt(df1, id=c("id"))

  # needed the expected values
  df2 <- data.frame(id = 1:nrow(d) , expected1 = d[["EXPECTED"]] , expected2 = d[["EXPECTEDQ"]])
  e <- melt(df2, id=c("id"))

  x_length = length(x[["Chromosome"]])
  y_chromo <- y[["Chromosome"]]
  #length(y_chromo[1: x_length])

  df3 <- data.frame(id = 1:nrow(d) , cgid1 = d[["Chromosome"]] , cgid2 = d[["ChromosomeQ"]])
  f <- melt(df3, id=c("id"))


  # final customized data frame
  df4 <- data.frame(analysis = m$variable  , observed = m$value, expected = e$value, chromosome = f$value)

  ########################################################################################


  qqplot.data <- list(data = df4)

  class(qqplot.data) <- "qqplot.data"

  qqplot.data

}


#' rnbi.qqplot.data.static
#'  For 450K analysis
#' read and convert the p values data into a list so that it can be used to in the qqplot function "qqunif.plot"
#'
#' @param wd a working directory of RnBeads reports
#' @param f a index of the comparison file
#' @return return an object that can be used to in the qqplot function "qqunif.plot"
#' @export


rnbi.qqplot.data.static.idat <- function(wd,f) {

  if ( file.exists( isolate({ paste(wd,'differential_methylation_data',f,sep="/") }) ) ){
    filename <- file.path(wd, 'differential_methylation_data',f)


    filename= as.character(filename)

    # fread function from the library data.table
    list.diff.p.values <- fread(filename,sep = ",", select = c("diffmeth.p.val"))

    # converting the data into list so that it can be plotted

    list.diff.p.values <- as.data.frame(list.diff.p.values)

    list.diff.p.values <- as.matrix(list.diff.p.values)

    list.diff.p.values <- lapply(seq_len(ncol(list.diff.p.values)), function(col) list.diff.p.values[,col])

    list.diff.p.values <- unlist(list.diff.p.values)

    return(list.diff.p.values)
  }

  else{

    empty_list <- list()

    return(empty_list)
  }


}


#' rnbi.qqplot.data.static
#'  For RRBS analysis
#' read and convert the p values data into a list so that it can be used to in the qqplot function "qqunif.plot"
#'
#' @param wd a working directory of RnBeads reports
#' @param f a index of the comparison file
#' @return return an object that can be used to in the qqplot function "qqunif.plot"
#' @export


rnbi.qqplot.data.static.rrbs <- function(wd,f) {

  if ( file.exists( isolate({ paste(wd,'differential_methylation_data',f,sep="/") }) ) ){
    filename <- file.path(wd, 'differential_methylation_data',f)



    filename= as.character(filename)

    # fread function from the library data.table
    list.diff.p.values <- fread(input = paste('zcat < ',filename,sep = ''), select = c("diffmeth.p.val"))

    # converting the data into list so that it can be plotted

    list.diff.p.values <- as.data.frame(list.diff.p.values)

    list.diff.p.values <- as.matrix(list.diff.p.values)

    list.diff.p.values <- lapply(seq_len(ncol(list.diff.p.values)), function(col) list.diff.p.values[,col])

    list.diff.p.values <- unlist(list.diff.p.values)

    return(list.diff.p.values)
  }

  else{

    empty_list <- list()

    return(empty_list)
  }


}




# http://stats.stackexchange.com/questions/110755/how-calculate-inflation-observed-and-expected-p-values-from-uniform-distribution
# refrerence : http://genome.sph.umich.edu/wiki/Code_Sample:_Generating_QQ_Plots_in_R

#Quantile-quantile plots (qq-plots) can be useful for verifying that a set of values
#come from a certain distribution. For example in a genome-wide association study,
#we expect that most of the SNPs we are testing not to be associated with the disease.
#Under the null, this means that the p-values we get from tests where no true
#association exists should follow a uniform(0,1) distribution. Since we're usually most
#interested in really small p-values, we generally transform the p-values by -log10 so
#that the smallest values near zero become the larger values and are thus easier to see.

#' qqunif.plot
#'
#' Takes in a p-values
#' @param pvalues p-values
#' @param should.thin
#' @param thin.obs.places
#' @param thin.exp.places
#' @param xlab
#' @param ylab
#' @param draw.conf
#' @param conf.points
#' @param conf.col
#' @param conf.alpha
#' @param already.transformed
#' @param pch
#' @param prepanel
#' @param par.settings
#' @return functiont to draw qqplot
#' @export

qqunif.plot<-function(pvalues,
                      should.thin=T, thin.obs.places=2, thin.exp.places=2,
                      xlab=expression(paste("Expected (",-log[10], " p-value)")),
                      ylab=expression(paste("Observed (",-log[10], " p-value)")),
                      draw.conf=TRUE, conf.points=1000, conf.col="lightgray", conf.alpha=.05,
                      already.transformed=FALSE, pch=20, aspect="fill", prepanel=prepanel.qqunif,
                      par.settings=list(superpose.symbol=list(pch=pch)), ...) {


  #error checking
  if (length(pvalues)==0) stop("pvalue vector is empty, can't draw plot")
  if(!(class(pvalues)=="numeric" ||
         (class(pvalues)=="list" && all(sapply(pvalues, class)=="numeric"))))
    stop("pvalue vector is not numeric, can't draw plot")
  if (any(is.na(unlist(pvalues)))) stop("pvalue vector contains NA values, can't draw plot")
  if (already.transformed==FALSE) {
    if (any(unlist(pvalues)==0)) stop("pvalue vector contains zeros, can't draw plot")
  } else {
    if (any(unlist(pvalues)<0)) stop("-log10 pvalue vector contains negative values, can't draw plot")
  }


  grp<-NULL
  n<-1
  exp.x<-c()
  if(is.list(pvalues)) {
    nn<-sapply(pvalues, length)
    rs<-cumsum(nn)
    re<-rs-nn+1
    n<-min(nn)
    if (!is.null(names(pvalues))) {
      grp=factor(rep(names(pvalues), nn), levels=names(pvalues))
      names(pvalues)<-NULL
    } else {
      grp=factor(rep(1:length(pvalues), nn))
    }
    pvo<-pvalues
    pvalues<-numeric(sum(nn))
    exp.x<-numeric(sum(nn))
    for(i in 1:length(pvo)) {
      if (!already.transformed) {
        pvalues[rs[i]:re[i]] <- -log10(pvo[[i]])
        exp.x[rs[i]:re[i]] <- -log10((rank(pvo[[i]], ties.method="first")-.5)/nn[i])
      } else {
        pvalues[rs[i]:re[i]] <- pvo[[i]]
        exp.x[rs[i]:re[i]] <- -log10((nn[i]+1-rank(pvo[[i]], ties.method="first")-.5)/(nn[i]+1))
      }
    }
  } else {
    n <- length(pvalues)+1
    if (!already.transformed) {
      exp.x <- -log10((rank(pvalues, ties.method="first")-.5)/n)
      pvalues <- -log10(pvalues)
    } else {
      exp.x <- -log10((n-rank(pvalues, ties.method="first")-.5)/n)
    }
  }


  #this is a helper function to draw the confidence interval
  panel.qqconf<-function(n, conf.points=1000, conf.col="gray", conf.alpha=.05, ...) {
    require(grid)
    conf.points = min(conf.points, n-1);
    mpts<-matrix(nrow=conf.points*2, ncol=2)
    for(i in seq(from=1, to=conf.points)) {
      mpts[i,1]<- -log10((i-.5)/n)
      mpts[i,2]<- -log10(qbeta(1-conf.alpha/2, i, n-i))
      mpts[conf.points*2+1-i,1]<- -log10((i-.5)/n)
      mpts[conf.points*2+1-i,2]<- -log10(qbeta(conf.alpha/2, i, n-i))
    }
    grid.polygon(x=mpts[,1],y=mpts[,2], gp=gpar(fill=conf.col, lty=0), default.units="native")
  }

  #reduce number of points to plot
  if (should.thin==T) {
    if (!is.null(grp)) {
      thin <- unique(data.frame(pvalues = round(pvalues, thin.obs.places),
                                exp.x = round(exp.x, thin.exp.places),
                                grp=grp))
      grp = thin$grp
    } else {
      thin <- unique(data.frame(pvalues = round(pvalues, thin.obs.places),
                                exp.x = round(exp.x, thin.exp.places)))
    }
    pvalues <- thin$pvalues
    exp.x <- thin$exp.x
  }
  gc()

  prepanel.qqunif= function(x,y,...) {
    A = list()
    A$xlim = range(x,y)*1.02
    #A$xlim[1]=0
    A$ylim = A$xlim
    return(A)
  }

  #draw the plot
  xyplot(pvalues~exp.x, groups=grp, xlab=xlab, ylab=ylab, aspect=aspect,
         prepanel=prepanel, scales=list(axs="i"), pch=pch,
         panel = function(x, y, ...) {
           if (draw.conf) {
             panel.qqconf(n, conf.points=conf.points,
                          conf.col=conf.col, conf.alpha=conf.alpha)
           };
           panel.xyplot(x,y, ...);
           panel.abline(0,1);
         }, par.settings=par.settings, ...
  )

}


