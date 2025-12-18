/* Test semantic errors */
program
    var x, y : integer;
        b : boolean;
begin
    x := 5;
    y := true;        /* Type mismatch: boolean to integer */
    b := x + 2;      /* Type mismatch: integer to boolean */
    x := x and y;     /* Invalid operation on integers */
    
    if b then
        x := 'hello';  /* Type mismatch: string to integer */
        
    x := x * 'world'; /* Invalid operands */
    
    exit              /* Exit outside loop */
end.
