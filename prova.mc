int a[15];
int c[3][20];

void increase(int *x){
    *x = (*x)+1;
    return;
}

int fib(int n){
    if(n==0 || n==1)
        return 1;
    else
        return fib(n-1)+fib(n-2);
}

int main(){
    int i;
    int j;
    j=fib(4);
    i=(j=fib(5));
    print(i);
    return 0;
}