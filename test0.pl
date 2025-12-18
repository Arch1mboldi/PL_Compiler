/* Test if-else statements */
program
    var x, y : integer;
        result : integer;
begin
    x := 10;
    y := 20;
    
    if x > y then
        result := x
    else
        result := y;
        
    write(result);
    
    if x = 10 then
        write(1)
    else
        write(0)
end.
