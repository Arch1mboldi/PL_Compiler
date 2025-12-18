/* Test exit statement */
program
    var i : integer;
begin
    i := 1;
    while i <= 10 do
    begin
        if i > 5 then
            exit;
        write(i);
        i := i + 1
    end;
    write(999)  /* This should not be reached */
end.
