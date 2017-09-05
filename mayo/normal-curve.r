require(tikzDevice)

tikz('normal.tex', standAlone = TRUE, width=10, height=7)

# Normal distribution curve
x <- seq(-4.5,4.5,length.out=100)
y <- dnorm(x)

# Integration points
xi <- seq(-2,2,length.out=2)
yi <- dnorm(xi)

# plot the curve
plot(seq(-4.5,4.5, length.out=10),seq(0,.45,length.out=10),type="n",ylab='$p(x)$',xlab='$x$')
lines(x,y,col='blue',lwd=3)
title(main="The Normal Distribution", cex.main=4)
# plot the panels
#lines(xi,yi,type='s')
#lines(range(xi),c(0,0))
lines(xi,yi,type='h')

#Add some equations as labels
int <- integrate(dnorm,min(xi),max(xi),subdivisions=length(xi))
text(2.8, 0.3, paste("\\small$\\displaystyle\\int_{", min(xi),
                     "}^{", max(xi), "}p(x)dx\\approx", round(int[['value']],3),
                     '$', sep=''), cex=2.5)

text(-2.8, 0.3, "$p(x)=\\frac{1}{\\sqrt{2\\pi}}e^{-\\frac{x^2}{2}}$", cex=2.5)

#Close the device
dev.off()

# Compile the tex file
tools::texi2dvi('normal.tex',pdf=T)