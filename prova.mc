
int ueue;
int fib(int n){
    if(n == 0 || n==1)
        return 1;
    else
        return n*fib(n-1);
}

void sort(int a[], int dim){
    for(int i = 0; i<dim; i++)
        a[i]=fib(i);
}

void printString(char s[]){
    int i = 0;
    while(s[i] != '\n')
        print_char(s[i++]);
}

int main(){  
    /*char s[] = "ciao bello";
    for(int i= 0; i<10; i++)
        print_char(s[i]);*/
    int arr[15];
    arr[2] = 17;
    sort(arr, 15); 
    char s[] = "ciao \n bello";
    printString(s);
    //for(int i = 0; i<15; i++)
      //  print(arr[i]);  
    return 0;
}