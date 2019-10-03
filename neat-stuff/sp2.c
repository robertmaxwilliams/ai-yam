#include "boxy.h"

OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO  
OO         OOO              OOO            OOO                OO  
O           O                O              O                  O  
O foo=4;    O    bar*=foo;   OBB bar--;     O                  O  
O P(foo)    O     P(bar)     O   P(bar)     O                  O  
OO   VVV   OOO     AAA      OOO    VVV     OOO                OO  
O           O                O              O                  O  
O foo++;    OBB  bar=foo-1;  O   bar--;     O                  O  
O P(foo)    O    P(bar)      O   P(bar)     O                  O  
OO         OOO              OOO   VVV      OOO                OO  
O           O  p(end)        O              O                  O  
O           O    END       CCO   bar++;     O                  O  
O           O                O  P(bar)      O                  O  
OO         OOO              OOO            OOO                OO  
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  
                                                                  
                                                                  
                                                                  
                                                                  
