int main(){
    int x = 42;
    int *p=&x;
    int *q;
    q = NULL;

    char a='a';
    char b='\n';
    char *r;
    r = NULL;
    if(a == b)
        print(1);
    if(q == NULL)
        print(1);
    print(x);
    return 0;
}