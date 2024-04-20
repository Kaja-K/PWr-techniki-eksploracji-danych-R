x=c(-10, 9, 1, 2, 5,-2, 6, 2, 1, 0, 1, 4, 5, 6, 3, 7, 3, 2,
    2, 3, 8, 5, 3, 4, 8, 0, 8, 0, 5, 1, 6, 4, 8, 13, 2,-13, 20)
mean=mean(x)
sigma=sd(x)
a=mean+3*sigma
b=mean-3*sigma
for (i in x){
if ((i>a)|(i<b))
{print(i)}
}