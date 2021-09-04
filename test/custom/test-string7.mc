int main() {
    int i;

    char s[] = "hello world!\n";

    for(i = 0; s[i] != '\n'; i++)
        print_char(s[i]);

    return 0;
}
