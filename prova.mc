int x;
int y = 42;
char s[] = "virginia raggi";
int *p = &x;


void modify(int *p){
    *p = 23456789;
}

int main(){
    print(y);
    modify(p);
    print(x);
    return 0;
}