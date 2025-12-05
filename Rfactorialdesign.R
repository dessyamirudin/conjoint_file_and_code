#Example 1
library(conjoint)
experiment<-expand.grid(
  price=c("low","medium","high"),
  variety=c("black","green","red"),
  kind=c("bags","granulated","leafy"),
  aroma=c("yes","no"))
design_full=caFactorialDesign(data=experiment,type="full")
print(design_full)
print(cor(caEncodedDesign(design_full)))


#Example 2
design_na=caFactorialDesign(data=experiment)
print(design_na)
print(cor(caEncodedDesign(design_na)))

#Example 3
design_or=caFactorialDesign(data=experiment,type="orthogonal")
print(design_or)
caEncodedDesign(design_or)
print(cor(caEncodedDesign(design_or)))
cor(caEncodedDesign(design_or))

#Example 4
design_fr_16=caFactorialDesign(data=experiment,type="fractional",cards=16)
print(design_fr_16)
print(cor(caEncodedDesign(design_fr_16)))

#Example 5
design_fr=caFactorialDesign(data=experiment,type="fractional")
print(design_fr)
print(cor(caEncodedDesign(design_fr)))

#Example 6
design_ca=caFactorialDesign(data=experiment,type="ca")
print(design_ca)
print(cor(caEncodedDesign(design_ca)))

#Example 7
design_aca=caFactorialDesign(data=experiment,type="aca")
print(design_aca)
print(cor(caEncodedDesign(design_aca)))

# source: https://www.rdocumentation.org/packages/conjoint/versions/1.41/topics/caFactorialDesign
# source: https://rpubs.com/chia_dc/Conjoint_Analysis
# https://deepblue.lib.umich.edu/bitstream/handle/2027.42/72641/j.1540-5915.1991.tb00357.x.pdf;jsessionid=99983BD1AC9991527EA6B4B76C61DCEA?sequence=1