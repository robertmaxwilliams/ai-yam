
#include <stdio.h>
#define CONCAT_IMPL( x, y ) x##y
#define MACRO_CONCAT( x, y ) CONCAT_IMPL( x, y )
#define USE_LOCK( lock ) TLockUse MACRO_CONCAT( LockUse, __COUNTER__ )( lock )

#define OPP (

#define EMPTY()
#define DEFER(id) id EMPTY()
#define OBSTRUCT(...) __VA_ARGS__ DEFER(EMPTY)()
#define EXPAND(...) __VA_ARGS__


//#define GOBBLE(arg)


// so the goal here is to have a fixed size grid of functions
// that call eachother based on some arrows
// lets say 4 columns


int foo;
int bar;
void maint(int, int);
void multi_maint(int);

//return codes: 2: terminal exit block
//              1: didn't return
//              0: reached default, error
#define FLONK break; case __COUNTER__ :
#define O FLONK FLONK FLONK


#define UP (loc - (2 * COLS))
#define DOWN (loc + (2 * COLS))


#define P(var) printf("%s=%d\n", #var, var);
#define p(stuff) printf("%s\n", #stuff);

#define OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO void maint(int loc, int ploc) { switch (loc) { case -1:
#define XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX break; }};int main() { multi_maint(2+2*COLS); }
#define OO O
#define OOO O
#define VVV if (DOWN != ploc){ p(down)  multi_maint(DOWN);}
#define AAA if (UP != ploc) { p(up) multi_maint(UP);}
#define OBB FLONK p(right) multi_maint(loc+2); FLONK FLONK
#define CCO FLONK FLONK p(left) multi_maint(loc-2); FLONK
#define END return;
#define COND(n) if (n%4 == 0) multi_maint(UP); else if (n%4 == 1) multi_maint(loc+3); \
    else if (n%4 == 2) multi_maint(DOWN); else if (n%4 == 3) multi_maint(loc-3);


// each line has the following cells:
// 0 1    2     3 4     5   6 7    8    9 10   11     12 13  14
// | | |  code  |R| |  code | |L|  code | | |  code  |   |   |
int COLS = 15;



void multi_maint(int loc) {
    // first run the code at that line
    maint(loc-COLS, -1);
    maint(loc, -1);
    maint(loc+COLS, -1);
    // then try all of the jumps, only one should actually recur down
    maint(loc+1, loc);
    maint(loc-1, loc);
    maint(UP, loc);
    maint(DOWN, loc);
}


int a;
int b;
int c;
// cond will redirect mod(n) = [0, 1, 2, 3] to [up, right, down, left] respectively
// cond must be in center row of box.

OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO  
OO         OOO              OOO            OOO                OO  
O   a=0;    O                O              O                  O  
O   b=0;    OBB    b++;      O              O                  O  
O   c=0;    O                O              O                  O  
OO         OOO     VVV      OOO            OOO                OO  
O           O                O              O                  O  
O   b++;    OBB   COND(b)  CCO   b++;       O                  O  
O           O                O              O                  O  
OO         OOO              OOO            OOO                OO  
O           O                O              O                  O  
O           O     P(B)       O              O                  O  
O           O     END        O              O                  O  
OO         OOO              OOO            OOO                OO  
O           O                O              O                  O  
O           O              CCO              O                  O  
O           O                O              O                  O  
OO         OOO              OOO            OOO                OO  
O           O                O              O                  O  
O           O              CCO              O                  O  
O           O                O              O                  O  
OO         OOO              OOO            OOO                OO  
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  
                                                                  
                                                                  
                                                                  
                                                                  
