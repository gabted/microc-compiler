
void print_string(char s[]){
    for(int i = 0; s[i] != '\0'; i++)
        print_char(s[i]);
}

void main(){
    char c = '\0';
    char s[] = "hello\0 world";

    for(int i = 0; s[i] != '\0'; i++)
        print_char(s[i]);


    //print_string(c);
}