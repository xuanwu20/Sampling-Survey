############################ preset ##########################
source('sampling.r')
source('ci.r')
source('srs.r')
source('stra srs.r')
source('ratio.r')
source('ratio mean.r')
source('stra ratio.r')

UNIVERSE = read.csv('diamonds.csv',stringsAsFactors=F)
N = nrow(UNIVERSE)
n = 5000
alpha = 0.05

######################## color to score ######################
# auxi=c('D','E','F','G','H','I','J')
# len=length(auxi)
# color=UNIVERSE$color
# for(i in 1:len){
#   temp=auxi[i]
#   color[color==temp]=i
# }
# UNIVERSE$color=color=as.numeric(color)

##################### stratified sampling ####################
stratum = c(0, 0.5, 1.0, 1.5, 2.0, 5.01)
n0 = length(stratum)-1
Nh = numeric(n0)
UNIVERSE.stra = NULL

for(h in 2:(n0+1))
{
  temp11 = stratum[h-1]
  temp12 = stratum[h]
  
  carat = UNIVERSE$carat
  
  UNIVERSE$index[carat<=temp12 & carat>temp11] = h-1
  temp2 = UNIVERSE$index[carat<=temp12 & carat>temp11]
  Nh[h-1] = length(temp2)
}

for(h in 1:n0)
{
  UNIVERSE.stra[[h]] = UNIVERSE[UNIVERSE$index==h,]
  rownames(UNIVERSE.stra[[h]]) = c(1:nrow(UNIVERSE.stra[[h]]))
}

Wh = Nh/N
nh = round(Wh*n)

SAMPLE = stra.sampling(nh, UNIVERSE.stra)

############################ stra.simple ##########################
simple.result = stra.srs.mean2(Nh, SAMPLE$price, SAMPLE$index, alpha)$mean.result

print(simple.result)

######################## stra.ratio.separate ######################
Xbarh = numeric(n0)
for(h in 1:n0)
{
  temp = UNIVERSE[UNIVERSE$index==h,]
  Xbarh[h] = mean(temp$carat)
}

separate.result = separate.ratio.mean(Nh, SAMPLE$price, SAMPLE$carat, 
                          SAMPLE$index, Xbarh, alpha)$yRS.result
print(separate.result)

####################### stra.reg.combined #####################
Xbar = sum(Xbarh*Wh)

combined.result = combined.ratio.mean(Nh, SAMPLE$price, SAMPLE$carat, 
                           SAMPLE$index, Xbar, alpha)$yRC.result
print(combined.result)

############################# output ###########################
sink("attach5_result.txt")
cat('This is an application to diamonds data:','\n')
cat('\n')
cat('Every single observation is marked 1 to 5 by carat','\n')
cat('\n')
cat('"carat" is also chosen as the auxiliary variable','\n')
cat('\n')
cat('The estimation of mean price under Stratified SRS design','\n')
cat('\n')

cat('The full size N = 53940','\n')
cat('The total sample size of Stratified SRS n = 5000','\n')
cat('The respective sample size of each layer nh = ', nh, '\n')
cat('\n')

cat('The result for Stratified SRS simple estimation:','\n')
cat('\n')
cat('Est = ', simple.result$mean.est, '\n')
cat('Var = ', simple.result$mean.var, '\n')
cat('SD = ', simple.result$mean.sd, '\n')
cat('CI = [', simple.result$mean.left, ',', simple.result$mean.right, ']','\n')
cat('\n')

cat('The result for Stratified SRS separate ratio estimation:','\n')
cat('\n')
cat('Est = ', separate.result$Est, '\n')
cat('Var = ', separate.result$Var, '\n')
cat('SD = ', separate.result$SD, '\n')
cat('CI = [', separate.result$Left, ',', separate.result$Right, ']','\n')
cat('\n')

cat('The result for Stratified SRS combined ratio estimation:','\n')
cat('\n')
cat('Est = ', combined.result$Est, '\n')
cat('Var = ', combined.result$Var, '\n')
cat('SD = ', combined.result$SD, '\n')
cat('CI = [', combined.result$Left, ',', combined.result$Right, ']','\n')
cat('\n')




