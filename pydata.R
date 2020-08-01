#ダミーデータの読み込み
d <- read.csv("https://raw.githubusercontent.com/koheiyamamoto224/pydata/master/dummy.csv")

#ダミーデータの内容：
#ageは調査時点の年齢
#n：従業先番号
#a：当該の職歴番号が始まったときの年齢
#bとc：その職歴に関する何らかの変数(職種など)

#変数の名前や職歴番号の最大値はデータを見て確認してください
#また、職歴にともなって変化する変数(n,a,b,c)は、nXX、aXXのように、職歴番号に相当する数値が末尾についている必要があります。そうなっていない場合、適当に変数名をアレンジしてください

d=d #データ
range <- 15:30 #抽出したい年齢の区間
age <- "age" #調査時点年齢の変数名
varying.age <- "a" #職歴番号ごとの年齢を表す変数名
varying <- c("n","b","c") #職歴ごとに変動する変数の名前一覧
dansu <- 1:4 #1〜職歴番号の最大値
event <- c("marriage","birth") #ライフイベントが生じた年齢を表す変数の名前一覧
cons <- c("gender") #変動しないがPYデータに含めたい変数一覧

#ワイド形式でデータを作成
#rangeで指定した年数分の列を、varyingで指定した変数の数だけ作成
allvars <- c(varying.age,varying,event)
df <- data.frame(matrix(rep(NA,
                            nrow(d)*
                              length(range)*
                              length(allvars)),
                            nrow=nrow(d)))
#列名をつけておく。アンダーバーの後に年齢(例：n_15)
for(i in 1:length(allvars)){
  for(j in 1:length(range)){
    colnames(df)[length(range)*(i-1)+j] <- paste(allvars[i],
                                                 range[j],
                                                 sep="_")
  }
}

#元データに結合
d <- cbind(d,df)

varying <- varying[varying!=varying.age]

#年齢Xにつきa1<=X<a2のときn1、a2<=X<a3のときn2…というように、職歴にともなって変化する変数の値を各セルに入れていく。満たさない場合はNA。
#jが列数、iが年齢の数(15-30)、kが職歴番号の数
for(k in 1:nrow(d)){
  for(i in 1:length(varying)){
      for(l in 1:length(dansu)){
        for(j in 1:length(range)){
        d[k,paste(varying[i],range[j],sep="_")] <- 
          ifelse(is.na(d[k,paste(varying[i],range[j],sep="_")]) & 
                   range[j]>=max(d[k,paste(varying.age,1,sep="")],
                                 d[k,paste(varying.age,dansu[l],sep="")],
                                 na.rm=TRUE) & 
                   range[j]<min(d[k,age],
                                d[k,paste(varying.age,dansu[l]+1,sep="")],
                                na.rm=TRUE),
                 d[k,paste(varying[i],dansu[l],sep="")],
                 d[k,paste(varying[i],range[j],sep="_")])
      }
    }
  }
}

for(k in 1:nrow(d)){
    for(l in 1:length(dansu)){
      for(j in 1:length(range)){
        d[k,paste(varying.age,range[j],sep="_")] <- 
          ifelse(is.na(d[k,paste(varying.age,range[j],sep="_")]) & 
                   range[j]>=max(d[k,paste(varying.age,1,sep="")],
                                 d[k,paste(varying.age,dansu[l],sep="")],
                                 na.rm=TRUE) & 
                   range[j]<min(d[k,age],
                                d[k,paste(varying.age,dansu[l]+1,sep="")],
                                na.rm=TRUE),
                 range[j],
                 d[k,paste(varying.age,range[j],sep="_")])
      }
    }
}

for(k in 1:nrow(d)){
  if(d[k,age]<=max(range)){
    for(i in 1:length(varying)){
      x <- na.omit(unlist(d[k,paste(varying[i],dansu,sep="")]))
      d[k,paste(varying[i],d[k,age],sep="_")] <- 
        x[length(x)]
    }
    d[k,paste(varying.age,d[k,age],sep="_")] <- d[k,age]
  }
}

write.csv(d,"wide.csv",row.names=FALSE)

myvars <- list(NULL)
for(i in 1:length(varying)){
  myvars[[1]] <- paste(varying.age,range,sep="_")
  myvars[[1+i]] <- paste(varying[i],range,sep="_")
}

dl <- reshape(d,
              varying=myvars,
              v.names=c(varying.age,varying),
              idvar="id",
              direction="long")[,c("id",age,cons,c(varying.age,varying))]

write.csv(dl[order(dl$id),],"long.csv",row.names=FALSE)

