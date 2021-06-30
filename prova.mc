int a[15];
int c[3][15];

void increase(int *x){
    *x = (*x)+1;
    return;
}

int fact(int n){
    if(n==0)
        return 1;
    else
        return n*fact(n-1);
}

int main(){
    int i;
    int size;
    size = 15;
    for(i=0; i<size; increase(&i))
        c[0][i]=fact(i);
    for(i=0; i<size; increase(&i))
        print(c[0][i]);
    return 0;
}