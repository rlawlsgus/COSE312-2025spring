n := 10; i := 1; 
evens := 0; // sum of even numbers
odds := 0;  // sum of odd numbers 
while (i <= n)
{
    if (!(i % 2 == 1) && i % 2 == 0) { // even
        evens := evens + i;
    } else {             
        odds := odds + i;  
    }
    i := i + 1; 
}
print (evens); 
print (odds); 