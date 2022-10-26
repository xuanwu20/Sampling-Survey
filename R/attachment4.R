########################### problem1/ preset #########################
source('ci.r')
source('srs.r')
source('ratio.r')
source('ratio mean.r')
source('regression.r')
source('srs size.r')
source('deff.r')

UNIVERSE = read.csv('diamonds.csv',stringsAsFactors=F)
N = nrow(UNIVERSE)
n = 5000
alpha = 0.05

auxi=c('D','E','F','G','H','I','J')
len=length(auxi)
color=UNIVERSE$color
for(i in 1:len){
  temp=auxi[i]
  color[color==temp]=i+3
}
UNIVERSE$color=as.numeric(color)


set.seed(020)
myindex = sample(row.names(UNIVERSE),n)
mysample = UNIVERSE[myindex,]
rownames(mysample)=c(1:nrow(mysample))

Xbar = mean(UNIVERSE$color)
y.mysample = mysample$price
x.mysample = mysample$color


########################### simple #########################
price.srs.result = srs.mean(N, mysample$price, alpha)

########################### ratio #########################
price.ratio.result = ratio.mean(y.mysample, x.mysample, N, Xbar, alpha)

ci.ratio = price.ratio.result$ybarR.ci
ci.ratio.left = ci.ratio[2,1]
ci.ratio.right = ci.ratio[2,2]

########################### regression #########################
price.reg.result = regression.mean(y.mysample, x.mysample,
                                  N, Xbar, alpha, method = "Min", beta0 = NULL)

########################### output #########################
sink("attach4_result.txt")
cat('This is an application to diamonds data:','\n')
cat('\n')
cat('The estimation of mean price under SRS design','\n')
cat('The full size N = 53940','\n')
cat('The sample size of SRS n = 5000','\n')
cat('\n')

cat('The result for SRS simple estimation:','\n')
cat('\n')
cat('Est = ', price.srs.result$ybar, '\n')
cat('Var = ', price.srs.result$ybar.var, '\n')
cat('SD = ', price.srs.result$ybar.sd, '\n')
cat('CI = [', price.srs.result$left, ',', price.srs.result$right, ']','\n')
cat('\n')

cat('The result for SRS ratio estimation:','\n')
cat('\n')
cat('Est = ', price.ratio.result$ybarR.est, '\n')
cat('Var = ', price.ratio.result$ybarR.var, '\n')
cat('SD = ', price.ratio.result$ybarR.sd, '\n')
cat('CI = [', ci.ratio.left, ',', ci.ratio.right, ']','\n')
cat('\n')

cat('The result for SRS regression estimation:','\n')
cat('\n')
cat('Est = ', price.reg.result$Est, '\n')
cat('Var = ', price.reg.result$Var, '\n')
cat('SD = ', price.reg.result$SD, '\n')
cat('CI = [', price.reg.result$Left, ',', price.reg.result$Right, ']','\n')
cat('\n')








