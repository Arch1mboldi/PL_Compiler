/* Test real number types */
program
    var x, y, z : real;
        i : integer;
begin
    x := 3.14;
    y := 2.5;
    i := 5;
    
    z := x + y;
    write(z);  /* 5.64 */
    
    z := x * y;
    write(z);  /* 7.85 */
    
    z := x / 2.0;
    write(z);  /* 1.57 */
    
    z := i * 1.5;
    write(z)   /* 7.5 */
end.
