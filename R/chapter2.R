#################### problem2 / Stratified Sampling ##################
source('sampling.r')

source('ci.r')
source('srs.r')
source('stra srs.r')
source('stra size.r')
source('ratio.r')
source('ratio mean.r')
source('stra ratio.r')
source('deff.r')

UNIVERSE = read.csv('diamonds.csv',stringsAsFactors=F)
N = nrow(UNIVERSE)
n = 5000
alpha = 0.05
Ybar = mean(UNIVERSE$price)
method = 'r'
bound = 0.01
allocation = 'Prop'


stratum = c('Fair', 'Good', 'Very Good', 'Premium', 'Ideal')
n0 = length(stratum)
Nh = numeric(n0)
UNIVERSE.stra = NULL

for(h in 1:n0)
{
  temp1 = stratum[h]
  UNIVERSE$index[UNIVERSE$cut==temp1] = h
  temp2 = UNIVERSE$index[UNIVERSE$cut==temp1]
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

S2h = var(UNIVERSE$price)
simple.size = strata.mean.size(Nh, S2h, Ch=NULL, allocation, method, bound, Ybar, alpha)


######################## stra.ratio.separate ######################
Xbarh = numeric(n0)
for(h in 1:n0)
{
  temp = UNIVERSE[UNIVERSE$index==h,]
  Xbarh[h] = mean(temp$carat)
}

separate.result = separate.ratio.mean(Nh, SAMPLE$price, SAMPLE$carat,SAMPLE$index, Xbarh, alpha)
separate.result = separate.result$yRS.result

print(separate.result)

var2 = c(simple.result$mean.var, separate.result$Var)
separate.deff = deff(var2)
separate.size = deff.size(separate.deff, simple.size$n)$Size[2]


####################### stra.ratio.combined #####################
Xbar = sum(Xbarh*Wh)

combined.result = combined.ratio.mean(Nh, SAMPLE$price, SAMPLE$carat, SAMPLE$index, Xbar, alpha)
combined.result = combined.result$yRC.result

print(combined.result)

var3 = c(simple.result$mean.var, combined.result$Var)
combined.deff = deff(var3)
combined.size = deff.size(combined.deff, simple.size$n)$Size[2]

############################ output ##########################
totalsize.result = matrix(c(simple.size$n, separate.size, combined.size), ncol = 1)

size.layer.result = as.data.frame(rbind(simple.size$nh, round(Wh*separate.size), round(Wh*combined.size)))
size.layer.result = cbind(size.layer.result, totalsize.result)
colnames(size.layer.result) = c(c('Layer1','Layer2','Layer3','Layer4','Layer5'), '   TotalSize')
rownames(size.layer.result) = c('simple','ratio.sep','ratio.comb')

sink("chapter2_result.txt")
cat('This is an application to diamonds data:','\n')
cat('\n')
cat('The "carat" is chosen as the auxiliary variable','\n')
cat('\n')
cat('Every single observation is marked 1 to 5 by the level of "cut" from Fair to Ideal','\n')
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

cat('The result for Stratified SRS separate Ratio estimation:','\n')
cat('\n')
cat('Est = ', separate.result$Est, '\n')
cat('Var = ', separate.result$Var, '\n')
cat('SD = ', separate.result$SD, '\n')
cat('CI = [', separate.result$Left, ',', separate.result$Right, ']','\n')
cat('\n')

cat('The result for Stratified SRS combined Ratio estimation:','\n')
cat('\n')
cat('Est = ', combined.result$Est, '\n')
cat('Var = ', combined.result$Var, '\n')
cat('SD = ', combined.result$SD, '\n')
cat('CI = [', combined.result$Left, ',', combined.result$Right, ']','\n')
cat('\n')

cat('The result for Stratified SRS size:','\n')
cat('\n')
print(size.layer.result)

sink()





