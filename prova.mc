double a[][10];
double x = 1.;

void printmatrix(int n){
    double (*p)[][10] = &a;
    for(int i = 0; i<n; i++)
        for(int j=0; j<10; j++)
            print(a[i][j]);
           //0;
}


void main(){
    char s[]= "ciao   
     bel\\lo";
     char c='!';
    for(int i = 0; i<5; i++)
        for(int j=0; j<10; j++)
            a[i][j] = (i+1)*(j+1);
    
    printmatrix(5);
}