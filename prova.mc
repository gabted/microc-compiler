double y = .1111111;

void foo(int x){
    print(x);
}

int main(){
    double x = 1.2345678;
    double *p = NULL;

    if(p == NULL)
        print(0);
    else
        print(1);

    foo(42);

    print_double(42);
    return 0;
}