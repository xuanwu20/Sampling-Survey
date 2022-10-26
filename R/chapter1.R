########################### problem1/ preset #########################
source('ci.r')
source('srs.r')
source('ratio.r')
source('ratio mean.r')
source('regression.r')
source('srs size.r')
source('deff.r')

UNIVERSE = read.csv('diamonds.csv')
N = dim(UNIVERSE)[1]
n = 5000
alpha = 0.05

set.seed(020)
myindex = sample(row.names(UNIVERSE),n)
mysample = UNIVERSE[myindex,]
rownames(mysample)=c(1:nrow(mysample))

Xbar = mean(UNIVERSE$carat)
y.mysample = mysample$price
x.mysample = mysample$carat

Mean.his=mean(UNIVERSE$price)
Var.his=var(UNIVERSE$price)
method='r'
bound=0.01


########################### simple #########################
price.srs.result = srs.mean(N, mysample$price, alpha)
price.simple.est = price.srs.result$ybar
price.simple.var = price.srs.result$ybar.var

simple.size = size.mean(N, Mean.his, Var.his, method, bound, alpha)
simple.size = simple.size$size

print(price.simple.est)


########################### ratio #########################
price.ratio.result = ratio.mean(y.mysample, x.mysample, N, Xbar, alpha)
price.ratio.est    = price.ratio.result$ybarR.est
price.ratio.var    = price.ratio.result$ybarR.var

var2 = c(price.simple.var, price.ratio.var)
ratio.deff = deff(var2)
ratio.size = deff.size(ratio.deff, simple.size)$Size[2]

print(price.ratio.est)

ci.ratio = price.ratio.result$ybarR.ci
ci.ratio.left = ci.ratio[2,1]
ci.ratio.right = ci.ratio[2,2]


########################### regression #########################
price.reg.result = regression.mean(y.mysample, x.mysample,
                                   N, Xbar, alpha, 
                                   method = "Min", beta0 = NULL)
price.reg.est   = price.reg.result$Est
price.reg.var   = price.reg.result$Var

var3 = c(price.simple.var, price.reg.var)
reg.deff = deff(var3)
reg.size = deff.size(reg.deff, simple.size)$Size[2]

print(price.reg.est)


########################### output #########################
size.result = matrix(c(simple.size, ratio.size, reg.size), nrow = 1)
colnames(size.result) = c('simple','ratio','regression')
rownames(size.result) = c('Size')
print(size.result)

sink("chapter1_result.txt")
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

cat('The result for sample size given r=0.01:','\n')
cat('\n')
print(size.result)








