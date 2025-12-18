/* Test syntax errors */
program
    var x : integer;
begin
    x := 5 +;      /* Missing operand */
    if x > 0       /* Missing then */
        write(x);
    x := y;        /* Undefined variable */
    while x < 10    /* Missing do */
        x := x + 1;
    x :== 5;       /* Invalid operator */
    (x + 2)       /* Unmatched parenthesis */
end.
