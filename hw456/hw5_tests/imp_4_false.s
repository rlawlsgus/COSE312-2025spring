/* Test: IMP_FALSE_MV4 (Multi-Variable Assume - Difficulty 4) */
/* Desc: Complex condition involving division and multiple variables. Program ACTUALLY SAFE if cond holds (idx=7). */
/*       x = (y/z) * z + (y - (y/z)*z). (y - (y/z)*z) is y % z. */
/*       So, if x = y, then y % z must be 0 for the condition to hold IF y/z is integer division. */
/*       Assume for S: y/z is integer division. Then y_mod_z = y - (y/z)*z. */
/*       If x == y/z + y_mod_z, this implies x == y/z + (y - (y/z)*z). This isn't x==y. */
/*       Let's make a condition like: if (x*z + rem == y), which is the definition of division. */
/*       Then y/z (quotient) should be x. And y - (y/z)*z (remainder) should be rem. */
/*       idx = x - (y/z) is 0. idx = rem - (y - (y/z)*z) is 0. */
/*       This is getting too complex to be "actually safe" easily without specific domain constraints. */
/*       Let's simplify to a condition hard for intervals: */
/*       If x > 0, y > 0, z > 0. And if x + y == z. idx = z - y - x. (Actual idx = 0). */
/*       Intervals: x,y,z are [1,+inf]. z-y-x = [1,+inf] - [1,+inf] - [1,+inf] = [-inf,+inf] = Top. */
/* Expected: false */
{
  int[10] arr;
  int x;
  int y;
  int z;
  int idx;

  read(x); /* x is [-inf,+inf] */
  read(y); /* y is [-inf,+inf] */
  read(z); /* z is [-inf,+inf] */

  if (x > 0 && y > 0 && z > 0) { /* Assume x,y,z are [1,+inf] */
    if (x + y == z) { /* Assume x+y == z. */
                      /* Example: x=3, y=4, z=7. */
      idx = z - y - x;  /* Actual: 0. Intervals: [1,+inf]-[1,+inf]-[1,+inf] = Top. */
      arr[idx] = 7;   /* arr[Top] is unsafe. */
    }
  }
}