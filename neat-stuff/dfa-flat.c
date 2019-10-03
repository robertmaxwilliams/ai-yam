#include <stdio.h>
// Making a VERY fast dfa in c using computed goto's

int maint() {
    int input_string[] = {0, 1, 1, 0, 0};
    int len = 5;
    int i = 0;
    int state = 0;
    // three states, two symbols each
    //                 symbol: 0  1
    int table[3][2] = {{2, 3}, // at state 1
                       {2, 3}, // at state 2
                       {3, 2}};// at state 3
    goto state_1;

    while (i < len) {
        state = table[state][intput_string[i++]];
    }
    return state;
}

int main () {
    printf("code: %d\n", maint());
    return 0;
}
