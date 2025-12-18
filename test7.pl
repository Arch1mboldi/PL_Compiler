/* Test array operations */
program
    type intArray = array [1 .. 5] of integer;
    var numbers : intArray;
        i, sum : integer;
begin
    /* Initialize array */
    i := 1;
    while i <= 5 do
    begin
        numbers[i] := i * 2;
        i := i + 1
    end;
    
    /* Calculate sum */
    sum := 0;
    i := 1;
    while i <= 5 do
    begin
        sum := sum + numbers[i];
        i := i + 1
    end;
    
    write(sum);  /* Should be 30 */
    write(numbers[3])  /* Should be 6 */
end.
