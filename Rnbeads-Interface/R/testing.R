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




# tmp <- file.path(getwd(), paste('application','results','mesangial cell','differential_methylation.html',sep='/'))
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
# query = "//*/table[@class='tabdata']/tr/td[@class='header']"
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









#
# #removing min low coverage and low confidence sites#
# data = read.table(file="f:/Rtesting/diffMethTable_site_cmp1.csv", header=T, sep=",")
#
# data
# names(data)
#
# # boxplot(data$mean.covg.RheumatoidArthritis , data=data, main="data",xlab="xlab", ylab="ylab")
# #
# # boxplot(data$mean.diff , data=data, main="data",xlab="xlab", ylab="ylab")
# #
# #
# # filtering<-data[!data$mean.covg.RheumatoidArthritis <5 & !data$mean.covg.Normal <5, ]
#
#
# #check for column name (difference between site list and promoter or CGI list)#
# filtering<-data[!data$mean.diff <0.05 & !data$mean.diff >-0.05 & !data$mean.covg.RheumatoidArthritis <5 & !data$mean.covg.Normal <5, ]
# #generate coordinate of site - check for "Start" or "i.Start"#
# filtering$Coord_CG <- paste(filtering$Chromosome, filtering$Start, sep=":")
# write.table(filtering, file="d:/RWork/Kuehnen/differential_sites.csv", sep=",", col.name=TRUE, row.name=FALSE)
#
# ###########################################################
#
# data1 = read.table(file = "d:/RWork/Kuehnen/differential_sites.csv", sep = ",", header = T)
#
# data2 = read.table(file = "d:/RWork/Eleonora/SNPs_hg19/CT.txt", sep = "\t", header = T)
# data3<-data1[!data1$Coord_CG %in% data2$Coord, ]
#
# data4 = read.table(file = "d:/RWork/Eleonora/SNPs_hg19/CG.txt", sep = "\t", header = T)
# data5<-data3[!data3$Coord_CG %in% data4$Coord, ]
#
# data6 = read.table(file = "d:/RWork/Eleonora/SNPs_hg19/GT.txt", sep = "\t", header = T)
# data7<-data5[!data5$Coord_CG %in% data6$Coord, ]
#
# data8 = read.table(file = "d:/RWork/Eleonora/SNPs_hg19/AC.txt", sep = "\t", header = T)
# data9<-data7[!data7$Coord_CG %in% data8$Coord, ]
#
# data10 = read.table(file = "d:/RWork/Eleonora/SNPs_hg19/AG.txt", sep = "\t", header = T)
# data11<-data9[!data9$Coord_CG %in% data10$Coord, ]
#
# write.table(data11,file="d:/RWork/Kuehnen/differential_sites_woSNPs.csv", col.name=T, row.name=F, sep=",")
#
# #genome annotation#
# library(data.table)
# data<-read.table(file="d:/RWork/Kuehnen/differential_sites_woSNPs.csv", sep=",", header=T)
# #put this line only for promoters and cpgislands#
# #data1<-data[!is.na(data$Chromosome),]
#
# data2<-read.table(file="d:/RWork/Annot_hg19.txt", sep="\t", header = T)#, quote="")
# #put this line only for sites#
# data1<-cbind(data[ ,1:3],End=(1+data[ ,3]),data[ ,4:ncol(data)])
#
# setkey(setDT(data1), Chromosome, Start, End)
# setkey(setDT(data2), Chromosome, Start, End)
# result<-foverlaps(data1, data2)
# write.table(result,file="d:/RWork/Kuehnen/differential_sites_annot.csv",sep=",",col.name=TRUE,row.name=FALSE)
#
# ###########################################################
