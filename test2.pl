/* Test input/output statements */
program
    var x, y, sum : integer;
begin
    x := 5;
    y := 7;
    sum := x + y;
    
    write('The sum is: ', sum);
    write('x = ', x, ', y = ', y);
    write(42)
end.
