
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

int foo(){
    int x =0;
    x++;
    return 0;
}

int main(){  
    char t[] = "ciao bello";
    for(int i= 0; i<10; i++)
        print_char(t[i]);
    int arr[15];
    arr[2] = 17;
    sort(arr, 15); 
    char s[] = "ciao \n bello";
    printString(s);
    //printString("bonjour");
    //for(int i = 0; i<15; i++)
      //  print(arr[i]);  
    return 0;
}