
int fib(int n){
    if(n == 0 || n==1)
        return 1;
    else
        return n*fib(n-1);
}

int main(){    
    int size = 15;
    int arr[15];
    int i = 0;
    for(; i<size;)
        arr[i]=fib(i);
    for(int i=0; i<size; i++)
        print(arr[i]);

    int a = 42;
    return 0;
}