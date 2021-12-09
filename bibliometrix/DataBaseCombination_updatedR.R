#(c) Research HUB | Visit https://researchhub.org/

#install.packages(bibliometrix) #if you are running for the first time, remove the # and run command  
library(bibliometrix)
biblioshiny()  #to run biblioshiny

#Step 1: Data loading and converting
DW <- ("C:/Users/ziaul/Dropbox/MY PC/1. USN-Vestfold/6. Block courses/Bibliometric Analysis PhD course/2. Fall 2020/Extract Data/WOS/CE_AI_WOS_198_txt.txt")
DS <- ("C:/Users/ziaul/Dropbox/MY PC/1. USN-Vestfold/6. Block courses/Bibliometric Analysis PhD course/2. Fall 2020/Extract Data/Scopus/CE_AI_SCOPUS_335_Bibtext.bib")

#Step 2: The object D can be converted in a data frame using the function convert2df
MW <- convert2df(DW, dbsource = "wos", format = "plaintext")
MS <- convert2df(DS, dbsource = "scopus", format = "bibtex")

#STep 3: To merge:
M <- mergeDbSources(MW, MS)

#Step 4: export this file in excel for biblioshiny
write.csv(M, "C:/Users/ziaul/Dropbox/MY PC/1. USN-Vestfold/6. Block courses/Bibliometric Analysis PhD course/2. Fall 2020/Extract Data/WOS-SCOPUS-COMBINED-2022.csv")

#Proceed with bibliometrix
#bibliometric analysis
results <- biblioAnalysis(M, sep = ";")
S <- summary(object = results, k = 10, pause = FALSE)
plot(x = results, k = 10, pause = FALSE)

#To obtain the most frequent cited manuscripts:
CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:10])

#To obtain the most frequent cited first authors:
CR <- citations(M, field = "author", sep = ";")
cbind(CR$Cited[1:10])

#To obtain the most frequent local cited authors:
CR <- localCitations(M, sep = ";")

#Author ranking based on dominance
DF <- dominance(results, k = 10)
DF

#H-index
authors=gsub(","," ",names(results$Authors)[1:10])
indices <- Hindex(M, field = "author", elements=authors, sep = ";", years = 50)
indices$H

#Top-Authors' Productivity over the Time
topAU <- authorProdOverTime(M, k = 10, graph = TRUE)

#Lotka's Law coefficient estimation
L <- lotka(results)
L$AuthorProd # Author Productivity. Empirical Distribution
L$Beta # Beta coefficient estimate
L$C #constant
L$R2 #goodness of fit
L$p.value # P-value of K-S two sample test


#bibliogrphic coupling
NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "references", sep = ";")
#bib coupling for research clusters
net=networkPlot(NetMatrix, type= "mds",normalize = "association", cluster= "louvain",  n = 50, size=4, size.cex = T, label=T, labelsize=3, label.cex=T, remove.isolates = T, Title = "Bibliographic coupling of articles")

#Bibliographic co-citation
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ".  ")
net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)

#Bibliographic collaboration
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "authors", sep = ";") #authors
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";") #country

#keyword co-occurrence
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)

netstat <- networkStat(NetMatrix)

#The summary statistics of the network
names(netstat$network) 
names(netstat$vertex)
summary(netstat, k=10)

#Co-Word Analysis: The conceptual structure of a field
CS <- conceptualStructure(M,field="ID", method="CA", minDegree=4, clust=5, stemming=FALSE, labelsize=10, documents=10)

#Historical Direct Citation Network
options(width=130) # Create a historical citation network
histResults <- histNetwork(M, min.citations = 1, sep = ";")
net <- histPlot(histResults, n=15, size = 10, labelsize=5) # Plot a historical co-citation network
