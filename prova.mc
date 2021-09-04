
void prints() {
    char s[] = "hello world";
    int i;
    for(i = 0; s[i] != '\0'; i++)
        print_char(s[i]);
}

int main() {
    int i;

    prints();

    return 0;
}
