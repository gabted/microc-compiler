
void print_array(int a[], int dim){
    for(int i=0; i<dim; i++)
        print(a[i]);
}

void print_matrix(int a[][5], int n, int m){
    for(int i = 0; i<n; i++){
        print_array(a[i], m);
        print_char('\n');
    }
}

int a;
int *b;
int c[10];
int *d[10];
int e[2][10];
int *f[10];
int (*g)[10];
int **h;
int (*i[2])[10];
int *j[2][10];


int main() {
    int n = 3;
    int m = 10;
    int a[3][10];

    for(int i = 0; i<n; i++)
        for(int j = 0; j<m; j++)
            a[i][j] = (i+1)*(j+1);
    
    //print_array(a[0], m);
    print_matrix(a, n, m);
    return 0;
}
