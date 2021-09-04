

void f(char s[]) {
    int i;

    for(i = 0; s[i] != '\n'; i++)
        s[i] = 'a';
}

void printstr(char s[]) {
    int i;
    for(i = 0; s[i] != '\n'; i++)
        print_char(s[i]);
}

int main() {
    int i;
    char g[14] = "hello world!\n";
    printstr(g);

    f(g);

    printstr(g);

    return 0;
}
