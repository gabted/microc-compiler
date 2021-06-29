int x[10];
int y;

int mul(int a, int b){
    int b;
    return a;
}

void increase(int *x){
    *x = 7;
    return;
}

int main(){
    int a;
    a = 6;
    increase(&a);
    print(a);
    return 0;
}