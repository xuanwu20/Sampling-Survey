#################### problem2 / Stratified Sampling ##################
source('sampling.r')
source('ci.r')
source('srs.r')
source('stra srs.r')
source('ratio.r')
source('ratio mean.r')
source('stra ratio.r')
source('regression.r')
source('stra regression.r')

UNIVERSE = read.csv('diamonds.csv',stringsAsFactors=F)
N = nrow(UNIVERSE)
n = 5000
alpha = 0.05


stratum = c('D','E','F','G','H','I','J')
n0 = length(stratum)
Nh = numeric(n0)
UNIVERSE.stra = NULL

for(h in 1:n0)
{
  temp1 = stratum[h]
  UNIVERSE$index[UNIVERSE$color==temp1] = h
  temp2 = UNIVERSE$index[UNIVERSE$color==temp1]
  Nh[h] = length(temp2)
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
simple.result = stra.srs.mean2(Nh, SAMPLE$price, SAMPLE$index, alpha)
simple.result = simple.result$mean.result

print(simple.result)

######################### stra.reg.separate #######################
Xbarh = numeric(n0)
for(h in 1:n0)
{
  temp = UNIVERSE[UNIVERSE$index==h,]
  Xbarh[h] = mean(temp$carat)
}

separate.result = separate.reg.mean(Nh, SAMPLE$price, SAMPLE$carat,SAMPLE$index, Xbarh, alpha)

print(separate.result)

######################## stra.reg.combined #######################
Xbar = sum(Xbarh*Wh)

combined.result = combined.reg.mean(Nh, SAMPLE$price, SAMPLE$carat, SAMPLE$index, Xbar, alpha)

print(combined.result)

############################ output ##########################
sink("attach2_result.txt")
cat('This is an application to diamonds data:','\n')
cat('\n')
cat('The "carat" is chosen as the related variable','\n')
cat('\n')
cat('Every single observation is marked 1 to 7 by the level of "color" from D to J','\n')
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

cat('The result for Stratified SRS separate regression estimation:','\n')
cat('\n')
cat('Est = ', separate.result$Est, '\n')
cat('Var = ', separate.result$Var, '\n')
cat('SD = ', separate.result$SD, '\n')
cat('CI = [', separate.result$Left, ',', separate.result$Right, ']','\n')
cat('\n')

cat('The result for Stratified SRS combined regression estimation:','\n')
cat('\n')
cat('Est = ', combined.result$Est, '\n')
cat('Var = ', combined.result$Var, '\n')
cat('SD = ', combined.result$SD, '\n')
cat('CI = [', combined.result$Left, ',', combined.result$Right, ']','\n')
cat('\n')

sink()





