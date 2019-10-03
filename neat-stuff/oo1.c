
int x;

  
void f1 () { x = 2; } 
void f2 () { x++; }
void f3 () { 
    if (x<3) {
        x = 0; 
    } else { 
        x = -x; 
    }
}
int main { 
    f1(); f2(); f3(); 
    printf("x=%d\n", x);
}
