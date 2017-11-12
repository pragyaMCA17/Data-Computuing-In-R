spiral<-function(n)
{
m<-matrix(nrow=n,ncol=n)
x<-1
y<-1
b<-1
b2<-n
z<-1:(n*n)
for (i in z)
  { 
    if (y<b2 && x<=y)
    { 
      m[x,y]<-z[i]
      y<-y+1
       
    }

    else if (y==b2 && x<y)
    {
      m[x,y]<-z[i]
      x<-x+1
    }
    else if (x==b2 && x>=y && y>b)
    { 
      m[x,y]<-z[i]
      y<-y-1
    }
    else if (y==b && (x-y)>1)
    { 
      m[x,y]<-z[i]
      x<-x-1
    }
    else 
    { b2<-b2-1
      b<-b+1
      m[x,y]<-z[i]
      y<-y+1
    }

 }
m
}
spiral(4)

