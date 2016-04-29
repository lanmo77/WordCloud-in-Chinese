
library(rjson)
library(RCurl)
nr=50*10
data.text=matrix(nrow=nr)

count=1

for (n in 12:50){

URL=paste('http://m.weibo.cn/page/json?containerid=1005052355790234_-_WEIBO_SECOND_PROFILE_WEIBO&page=',n,sep='')

WData=fromJSON(file = URL, method="C")


for (i in 1:10){
  lala=WData$cards[[1]]$card_group[[i]]$mblog$text
  data.text[count,]=lala
  count=count+1
  print(count)
}
}


save(data.text,file='data.text.Rdata')

#install.packages("tmcn", repos="http://R-Forge.R-project.org")
#install.packages("tmcn.crfpp", repos="http://R-Forge.R-project.org")
#install.packages("tmcn.word2vec", repos="http://R-Forge.R-project.org")
#install.packages("Rweibo", repos="http://R-Forge.R-project.org")
#install.packages("Rwordseg", repos="http://R-Forge.R-project.org")

library(tmcn)
library(Rwordseg)
library(tm)
library(dplyr)
library(wordcloud)
require(RColorBrewer)
require(scales)
library(grDevices)

clean=removeNumbers(data.text) 
clean=gsub("\\<[^\\]]*\\>", "", clean, perl=TRUE)
clean=gsub("\\[[^\\]]*\\]", "", clean, perl=TRUE)
clean=removePunctuation(clean)



names(clean)='Weibo'

clean[,1]=as.character(clean[,1])

# subset空值
len.ind=nchar(clean[,1])
clean=as.data.frame(clean)
clean['ind']=len.ind
clean.new=filter(clean,ind>1)
clean.new=clean$V1

#通过Rwordseg分词 
rt=segmentCN(as.character(clean.new))

 
rt1=c(do.call("cbind",rt)) 
rt2=table(rt1) 
rt3=as.data.frame(rt2) 
rt4=arrange(rt3,desc(Freq)) 
rt4$rt1=as.character(rt4$rt1) 
rt4$len=nchar(rt4$rt1)


#只保留长度大于2的字符 
rt4=subset(rt4,len>1) 
rt5=rt4[1:100,]
rt5=rt5[-c(1,2,4,6),]
rt5=rt5[-c(6,9),]
# 去除如‘转发’‘微博’等词汇

rt5=rt5[1:50,]


par(family='STKaiti')
wordcloud(rt5$rt1,rt5$Freq,random.order=FALSE,rot.per=.45,col=brewer.pal(9,"Set1"))
