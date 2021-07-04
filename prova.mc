
int ueue;
int fib(int n){
    if(n == 0 || n==1)
        return 1;
    else
        return n*fib(n-1);
}

int main(){    
    char x = 'g';
    char s[] = "h\tello\n";
    for(int i = 0; i<7; i++)
        print_char(s[i]);
    int arr[15];
    arr[2] = 4;
    for(int i = 0; i<15; i++)
        arr[i]=fib(i);
    //for(int i=0; i<15; i++)
        //print(arr[i]);

    int a = 42;
    return 0;
}