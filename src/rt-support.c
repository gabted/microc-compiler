#include <stdio.h>
#include <stdlib.h>

// TODO
int getint(){
  int out;
  scanf(" %d", &out);
  return out;
}

void print(int n){
  printf("%d\n", n);  
}

void print_char(char c){
  printf("%c\n", c);
}

void print_double(double d){
  printf("%f\n", d);
}