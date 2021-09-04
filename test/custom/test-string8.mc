void prints(char s[14]) {
    int i;
    for(i = 0; s[i] != '\n'; i++)
        print_char(s[i]);
}

int main() {
    int i;

    char s[14] = "hello world!\n";

    prints(s);

    return 0;
}
