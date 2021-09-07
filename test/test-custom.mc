char s[] = "hello world!";
double a[][10];
double (*p)[][10] = &a;
bool *q = NULL;

void print_matrix(int n){
    for(int i = 0; i<n; i++)
        for(int j=0; j<10; j++)
            print_double(a[i][j]);
}

void print_string(){
    for(int i = 0; s[i] != '\0'; i++)
        print_char(s[i]);
    print_char('\n');//prints two newlines
}

void main(){
    print_string();
    for(int i = 0; i<5; i++)
        for(int j=0; j<10; j++)
            a[i][j] = (i+1)*(j+1)/2.;
    print_matrix(5);
}