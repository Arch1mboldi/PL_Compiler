/* Test function calls */
program
    var x, y, result : integer;
    
    function add(a : integer; b : integer) : integer;
    begin
        add := a + b
    end;
    
    function multiply(a : integer; b : integer) : integer;
    begin
        multiply := a * b
    end;
    
    procedure swap(var a : integer; var b : integer);
    var temp : integer;
    begin
        temp := a;
        a := b;
        b := temp
    end;
    
begin
    x := 5;
    y := 3;
    
    result := add(x, y);
    write(result);  /* Should be 8 */
    
    result := multiply(x, y);
    write(result);  /* Should be 15 */
    
    call swap(x, y);
    write(x);      /* Should be 3 */
    write(y)       /* Should be 5 */
end.
