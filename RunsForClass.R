ER<-"E[R| date t]"


# Interesting is picking off %-ile of st.dev of forecast 0=full reg 1=no
# But lambda on each regression can be diff == prior that all sigma are sameish

z<- Forecast(
		D=Data,
		what=c("Market"),
		use=c("CONSTANT","DPratio"),
		Interesting=c(0.5)   
		)
p<-Plot.Forecast(z,clip=c(0.01,.2),target=ER,title="DP Ratio")
print(p)

z<- Forecast(
		D=Data,
		what=c("Market",Use.Industries),
		use=c("CONSTANT","DPratio"),
		Interesting=c(0.5)   
		)
p<-Plot.Forecast(z,clip=c(0.01,.2),target=ER,title="DP Ratio")
print(p)


z<- Forecast(
		D=Data,
		what=c("Market",Use.Industries),
		use=c("CONSTANT","PDratio","gaip","recession","CreditSpread","TermSpreadL","unrate"),
		Interesting=c(0.05)
		)

p<-Plot.Forecast(z,clip=c(0.01,.2),target=ER,title="DP Ratio,int rate, unemployment")		
print(p)

z<- Forecast(
		D=Data,
		what=c("Market",Use.Industries),
		use=c("inflation","unrate"), # Growth IP 
		Interesting=c(0.05)
		)
p<-Plot.Forecast(z,clip=c(0.01,.2),target=ER,title="inflation,unemployment")
print(p)

z<- Forecast(
		D=Data,
		what=c("Market",Use.Industries),
		use=c("CONSTANT","recession"), # Growth IP 
		Interesting=c(0.05)
		)
p<-Plot.Forecast(z,clip=c(0.01,.2),target=ER,title="Just nber recession {1,0}")
print(p)


z<- Forecast(
		D=Data,
		what=c("Market",Use.Industries),
		use=c("FEDFUNDS","TermSpreadS","TermSpreadL", "CreditSpread"), # Growth IP 
		Interesting=c(0.05)
		)
p<-Plot.Forecast(z,clip=c(0.01,.2),target=ER,title="just interest rates")
print(p)


z<- Forecast(
		D=Data,
		what=c("Market",Use.Industries),
		use=c("gaip","gacr"), # Growth IP 
		Interesting=c(0.1)
		)
p<-Plot.Forecast(z,clip=c(0.01,.2),target=ER,title="recent growth")
print(p)

z<- Forecast(
		D=Data,
		what=c("Market",Use.Industries),
		use=colnames(P[,grepl("ga",colnames(P))]), # Growth IP 
		Interesting=c(0.01)
		)
p<-Plot.Forecast(z,clip=c(0.01,.2),target=ER,title="recent growth (many)")	
print(p)

z<- Forecast(
		D=Data,
		what=c("Market",Use.Industries),
		use=colnames(P[,-c(1:3)]), # EVERYTHING
		Interesting=c(0.01)
		)
p<-Plot.Forecast(z,clip=c(0.01,.2),target=ER,title="Everything")		
print(p)

#*** VOL POTS

VR<-"sigma[R| date t]"
z<- Forecast(
		D=Data,
		what=c("Market"),
		use=c("CONSTANT","DPratio"),
		Interesting=c(0.1,0.25,0.5,0.75,.9),
		LHS="V"
		)
Plot.Forecast(z,clip=c(0.01,.25),target=VR,title="DP Ratio (several lambda)",h=sqrt(12),highlight="")


z<- Forecast(
		D=Data,
		what=c("Market",Use.Industries),
		use=c("CONSTANT","DPratio"),
		Interesting=c(0.5),
		LHS="V"
		)
Plot.Forecast(z,clip=c(0.01,.25),target=VR,title="DP Ratio",h=sqrt(12))

z<- Forecast(
		D=Data,
		what=c("Market",Use.Industries),
		use=c("CONSTANT","recession"), # Growth IP 
		Interesting=c(0.5),
		LHS="V"
		)
Plot.Forecast(z,clip=c(0.01,.25),target=VR,title="recession",h=sqrt(12))

z<- Forecast(
		D=Data,
		what=c("Market","Rtail", "Util",  "Oil"),
		use=c("CONSTANT","recession"), # Growth IP 
		Interesting=c(0.5),
		LHS="V"
		)
Plot.Forecast(z,clip=c(0.01,.25),target=VR,title="recession",h=sqrt(12))


z<- Forecast(
		D=Data,
		what=c("Market",Use.Industries),
		use=colnames(P[,-c(1:3)]), # EVERYTHING
		Interesting=c(0.01),
		LHS="V"
		)
Plot.Forecast(z,clip=c(0.01,.25),target=VR,title="Everything",h=sqrt(12))
	
## SHARPE RATIO
SR<-"Sharpe Ratio (annual)"
z<- Forecast(
		D=Data,
		what=c("Market",Use.Industries),
		use=c("CONSTANT","DPratio"),
		Interesting=c(0.5),
		LHS="S"
		)
Plot.Forecast(z,clip=c(-0.01,10),target=SR,title="DP Ratio",h=sqrt(12))

z<- Forecast(
		D=Data,
		what=c("Market",Use.Industries),
		use=c("CONSTANT","recession"), # Growth IP 
		Interesting=c(0.5),
		LHS="S"
		)
Plot.Forecast(z,clip=c(NA,NA),target=SR,title="recession",h=sqrt(12))
	
	

