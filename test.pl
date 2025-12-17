/* Test Program for Extended PL/0 */
program
    const max = 10;
    type intArr = array [1 .. 10] of integer;
    var x, y : integer;
        arr : intArr;
        b : Boolean;

    /* Function Declaration */
    function gcd(a : integer; b : integer) : integer;
    begin
        if b = 0 then
            gcd := a
        else
            gcd := gcd(b, a mod b)
    end;

    /* Procedure Declaration */
    procedure initArr;
    var i : integer;
    begin
        i := 1;
        while i <= max do
        begin
            arr[i] := i * 2;
            i := i + 1
        end
    end;

begin
    call initArr;
    x := 8;
    y := 12;
    write(x, y);
    
    x := gcd(x, y);
    write(x); /* Should output 4 */
    
    b := true;
    if b and (x > 1) then
        write(1)
    else
        write(0);
        
    /* Test Exit */
    x := 0;
    while x < 100 do
    begin
        x := x + 1;
        if x > 5 then exit
    end;
    write(x) /* Should output 6 */
end.
