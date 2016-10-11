# library(data.table)
# #
# results.dir = file.path(getwd(), 'application/results')
# # results.dir = file.path(getwd(), 'results')
# rwaDir <- file.path(results.dir, 'cardiac ventricle fibroblast')
# # #rwaDir
# #
# # foo <- mtcars
# # foo <- as.matrix(foo)
# # rownames(foo) <- NULL
# # colnames(foo) <- NULL
# #
# #
# # foo <- lapply(seq_len(ncol(foo)), function(col) foo[,col])
# #
# #
# filename <- normalizePath(file.path(rwaDir, paste('differential_methylation_data/diffMethTable_site_cmp1', '.csv', sep='')))
# #
# a.file <- read.csv(file = as.character(filename), nrows=100, header = TRUE)[ ,9:10]
# a.file
# a.file <- as.matrix(a.file)
# rownames(a.file) <- NULL
# colnames(a.file) <- NULL
# a.file <- lapply(seq_len(ncol(a.file)), function(col) a.file[,col])
#
# dataset <- a.file
# dataset
# #
# filen= as.character(filename)
# #
# # # fread function from the library data.table
# a.file <- fread(filen,sep = ",", select = c("diffmeth.p.val"),nrows = 10)
# a.file
# a.file <- as.data.frame(a.file)
# a.file
#
# a.file <- as.matrix(a.file)
# a.file
#
#
# #rownames(a.file) <- NULL
# #colnames(a.file) <- NULL
# a.file <- lapply(seq_len(ncol(a.file)), function(col) a.file[,col])
# a.file
# a.file <- unlist(a.file)
# a.file
#
#
# trees$Height
# qqnorm(trees$Height)
#
# qqnorm(a.file)


# output$compqqplot2 <- renderPlot({
#
#   qq.value2 <- as.character(input$input_modules)
#
#   qq.dir2 <- file.path(results.dir, qq.value2)
#
#   qq.value2 <- as.character(input$radio_comp)
#
#   if (is.null(input$radio_comp)){
#     # print error/ warning message
#     qqplot(1,1,main="Normal Q-Q Plot", ylab="diffmeth.p.val")
#     text(1,1,"No data available or no comparison file exist")
#   }
#
#   else if (input$radio_comp == "Choice1"){
#
#     # print error/ warning message
#     qqplot(1,1,main="Normal Q-Q Plot", ylab="diffmeth.p.val")
#     text(1,1,"No data available or no comparison file exist")
#   }
#   else{
#
#     print (qq.value2)
#     if (qq.value2 == ""){
#       x <- list()
#       x
#     }
#     else{
#       #fucntion from the RnBeadsInterface package
#       x <- comparison_plot(qq.dir2 , qq.value2)
#     }
#
#     if(length(x) == 0) {
#
#       # print error/ warning message
#       qqplot(1,1,main="Normal Q-Q Plot", ylab="diffmeth.p.val")
#       text(1,1,"No data available or no comparison file exist")
#
#     }
#     else{
#       qqplot(x,x,main="Normal Q-Q Plot", ylab="diffmeth.p.val")
#     }
#
#   }
#
#
# }, height = 400, width = 500)

# bn <- basename(file.path(getwd(), 'results/Dataset 1','data_import_data/annotation.csv'))
# print (bn)
# dn <- dirname(file.path(getwd(), 'results/Dataset 1','data_import_data/annotation.csv'))
# print (dn)
#
# comb <- paste(dn,bn, sep = '/')
# print (comb)
#
# tmp <- file.path(getwd(), paste('results/Dataset 1','/data_import_data/annotation.csv'))
# print (tmp)
# #removing space
# tmp <- gsub(" /", "/", tmp)
# print (tmp)
# data[-length(data)] <- paste0(data[-length(data)], ',')
# print (data)
#
# data <- strsplit(paste(data, collapse=', '), ' ')[[1]]
# print (data)




# tmp <- file.path(getwd(), paste('application','results','embryonic stem cell','differential_methylation.html',sep='/'))
# # #removing space
# # tmp <- gsub(" /", "/", tmp)
#
# filename= as.character(tmp)
# filename
#
# differential.methylation.path <- filename
#
# webpage <- readLines(tc <- textConnection(differential.methylation.path)); close(tc)
# pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
# pagetree
#
# # Extract table header and contents of the comparison table of differential methylation
#
# query = "//*/div[@id='section3']/ul/li"
# dates = xpathSApply(pagetree, query, xmlValue)
# dates
# comp_names <- list()
# comp_names_counter <- 1
# for (i in 1:length(dates)) {
#
#
#   if ((i>5)){
#
#
#     if(dates[i] == ""){
#       break
#     }
#
#     comp_names[comp_names_counter] <- dates[i]
#     comp_names_counter = comp_names_counter + 1
#
#   }
# }
# comp_names
