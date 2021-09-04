void prints(char s[]) {
    int i=0;
    for(i = 0; s[i] != '\0'; i++)
        print_char(s[i]);
}

int main() {
    int i;

    char s[] = "hello world!\n";

    prints(s);

    return 0;
}
