int x[10];
int y;

void increase(int *x){
    *x = (*x)+1;
    return;
}

int main(){
    int a;
    a = getint();
    if(a%2 == 0)
        increase(&a);
    print(a);
    return 0;
}