/* Test: HARD_FALSE_HQ4 */
/* Desc: Index becomes Bot due to potential division by zero in its calculation, leading to OOB. */
/* Expected: false */
{
    int[10] arr;
    int a;
    int b;
    int c;
    int idx;

    a = 10;
    read(b); /* b is [-inf, +inf], can be 0 */
    c = 5;

    idx = a / b; /* If b is 0, idx interval involves Bot or Top from div. Assume eval_e makes interval Bot. */
                 /* Or, if check_expression for DIV fails, this node is already unsafe. */
    arr[idx + c] = 0; /* If idx is Bot, Bot + [5,5] is Bot. arr[Bot] is an error. */
                      /* If idx is Top (e.g. from non-0 div), arr[Top] is error. */
    print(idx);
}