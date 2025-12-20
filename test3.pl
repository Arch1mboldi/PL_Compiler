/* Test boolean types and operations */
program
    var a, b, c : boolean;
        x, y : integer;
begin
    a := true;
    b := false;
    x := 5;
    y := 10;
    
    c := a and b;
    write(c);  /* false */
    
    c := a or b;
    write(c);  /* true */
    
    c := not a;
    write(c);  /* false */
    
    c := x > y;
    write(c);  /* false */

end.
