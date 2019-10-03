# 1 "sp2.c"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 1 "<command-line>" 2
# 1 "sp2.c"
# 12 "sp2.c"
int foo;
int bar;
# 31 "sp2.c"
int maint(int loc) { switch (loc) { case -1: GOBBLE((
)) break; case 0 : GOBBLE(( )) break; case 1 : GOBBLE(( )) break; case 2 : GOBBLE(( )) break; case 3 : GOBBLE(( )) break; case 4 : GOBBLE((
)) break; case 5 : GOBBLE(( )) break; case 6 : GOBBLE(( )) break; case 7 : GOBBLE(( )) break; case 8 : GOBBLE(( )) break; case 9 : GOBBLE((
)) break; case 10 : GOBBLE(( foo=4; )) break; case 11 : GOBBLE(( bar*=foo; goto L2; )) break; case 12 : GOBBLE(( bar--; )) break; case 13 : GOBBLE(( )) break; case 14 : GOBBLE((
)) break; case 15 : GOBBLE(( )) break; case 16 : GOBBLE(( )) break; case 17 : GOBBLE(( )) break; case 18 : GOBBLE(( )) break; case 19 : GOBBLE((
)) break; case 20 : GOBBLE(( goto L0; )) break; case 21 : GOBBLE(( goto L1 )) break; case 22 : GOBBLE(( goto L0; )) break; case 23 : GOBBLE(( )) break; case 24 : GOBBLE((
)) break; case 25 : GOBBLE(( )) break; case 26 : GOBBLE(( )) break; case 27 : GOBBLE(( )) break; case 28 : GOBBLE(( )) break; case 29 : GOBBLE((
)) break; case 30 : GOBBLE(( foo++; goto L2; )) break; case 31 : GOBBLE(( bar=foo-1; )) break; case 32 : GOBBLE(( bar--; )) break; case 33 : GOBBLE(( )) break; case 34 : GOBBLE((
)) break; case 35 : GOBBLE(( )) break; case 36 : GOBBLE(( )) break; case 37 : GOBBLE(( )) break; case 38 : GOBBLE(( )) break; case 39 : GOBBLE((
)) break; case 40 : GOBBLE(( )) break; case 41 : GOBBLE(( )) break; case 42 : GOBBLE(( goto L0; )) break; case 43 : GOBBLE(( )) break; case 44 : GOBBLE((
)) break; case 45 : GOBBLE(( )) break; case 46 : GOBBLE(( )) break; case 47 : GOBBLE(( )) break; case 48 : GOBBLE(( )) break; case 49 : GOBBLE((
)) break; case 50 : GOBBLE(( )) break; case 51 : GOBBLE(( return 0; )) break; case 52 : CCO_helper(( bar++; )) break; case 53 : GOBBLE(( )) break; case 54 : GOBBLE((
)) break; case 55 : GOBBLE(( )) break; case 56 : GOBBLE(( )) break; case 57 : GOBBLE(( )) break; case 58 : GOBBLE(( )) break; case 59 : GOBBLE((
)) break; case 60 : GOBBLE(( )) break; case 61 : GOBBLE(( )) break; case 62 : GOBBLE(( )) break; case 63 : GOBBLE(( )) break; case 64 : GOBBLE((
))}

int main() { maint(4); }
