/* Comprehensive test */
program
    type intArray = array [1 .. 3] of integer;
    var numbers : intArray;
        i, max, temp : integer;
        found : boolean;
    
    function maxFunc(a : integer; b : integer) : integer;
    begin
        if a > b then
            maxFunc := a
        else
            maxFunc := b
    end;
    
begin
    /* Initialize array */
    numbers[1] := 10;
    numbers[2] := 25;
    numbers[3] := 15;
    
    /* Find maximum using function and exit */
    max := 0;
    i := 1;
    found := false;
    
    while i <= 3 do
    begin
        temp := maxFunc(max, numbers[i]);
        if temp > 20 then
        begin
            found := true;
            exit
        end;
        max := temp;
        i := i + 1
    end;
    
    write(max);
    write(found);
    
    /* Test boolean operations */
    if found and (max > 0) then
        write('Found large number')
    else
        write('No large number found')
end.
