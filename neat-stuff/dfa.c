#include <stdio.h>
// Making a VERY fast dfa in c using computed goto's

int maint() {
    int input_string[] = {0, 1, 1, 0, 0};
    int len = 5;
    int i = 0;
    // three states, two symbols each
    //                        symbol 0    symbol 1
    void* goto_table[3][2] = {{&&state_2, &&state_3}, // at state 1
                              {&&state_2, &&state_3}, // at state 2
                              {&&state_3, &&state_2}};// at state 3
    goto state_1;

    state_1:
    if (i == len) return 1;
    goto *goto_table[0][input_string[i++]];

    state_2: 
    if (i == len) return 2;
    goto *goto_table[1][input_string[i++]];

    state_3:
    if (i == len) return 3;
    goto *goto_table[2][input_string[i++]];

}

int main () {
    printf("code: %d\n", maint());
    return 0;
}
