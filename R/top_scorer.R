#' Draw a Venn Diagram
#'
#' Takes in any numeric value and square it
#' @param x A numeric value to be squared
#' @return Squared value of x
#' @export

multipleVennDiagram <- function(cb.checked,results.dir,nrows.value) {


  if (length(cb.checked) == 1){


  }
  else if(length(cb.checked) == 2){



  }
  else if(length(cb.checked) == 3){
    # qq.value1 <- as.character(cb.checked[1])
    # qq.dir1 <- file.path(results.dir, qq.value1)
    # nrows.value <- as.character(100)
    # dataset1 <- readComparisonData(qq.value1,qq.dir1,1, nrows.value)


  }

  else if(length(cb.checked) == 4){
    hsb2 <- read.csv("http://www.ats.ucla.edu/stat/data/hsb2.csv")
    attach(hsb2)

    print(hsb2)
    hw <- (write >= 60)
    hm <- (math >= 60)
    hr <- (read >= 60)
    sc <- (science >= 60)
    ss <- (socst >= 60)

    c3 <- cbind(hw, hm, hr, sc)
    a <- vennCounts(c3)
    p <- vennDiagram(a, include = "both",
                names = c("High Writing", "High Math", "High Reading", "science"),
                cex = 1, counts.col = "red", circle.col = c("green","blue", "orange","yellow"))


  }
  else{

    hsb2 <- read.csv("http://www.ats.ucla.edu/stat/data/hsb2.csv")
    attach(hsb2)

    hw <- (write >= 60)
    hm <- (math >= 60)
    hr <- (read >= 60)
    sc <- (science >= 60)
    ss <- (socst >= 60)

    c3 <- cbind(hw, hm, hr, sc, ss)
    a <- vennCounts(c3)
    p <- vennDiagram(a, include = "both",
                names = c("High Writing", "High Math", "High Reading", "science", "social studies"),
                cex = 1, counts.col = "red", circle.col = c("green","blue", "orange","yellow" ,"pink"))


  }

}
