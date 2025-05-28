{
    int x;
int y;
int[10] a;
read(x);
while (y < 100) { y++; }
if (x < 50) {
if (x > y) {
a[100] = 1; /* dead code: the bug cannot occur at runtime */
}
}
}
