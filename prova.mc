double a[][10];
double (*p)[][10] = &a;

void printmatrix(int n){
    for(int i = 0; i<n; i++)
        for(int j=0; j<10; j++)
            print(a[i][j]);
}

void main(){
    char s[]= "ciao   
     bel\\lo";
    for(int i = 0; i<5; i++)
        for(int j=0; j<10; j++)
            (*p)[i][j] = (i+1)*(j+1);
    
    printmatrix(5);
}