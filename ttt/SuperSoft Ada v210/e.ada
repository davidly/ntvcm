pragma optimize(time);
procedure e is

h, n, x : integer;
a : Array(0..200) of integer;

begin
    h := 200;
    x := 0;
    n := h - 1;

    while n > 0 loop
        a( n ) := 1;
        n := n - 1;
    end loop;

    a( 1 ) := 2;
    a( 0 ) := 0;

    while h > 9 loop
        h := h - 1;
        n := h;
        while 0 /= n loop
            a( n ) := x REM n;
            x := 10 * a( n - 1 ) + ( x / n );
            n := n - 1;
        end loop;

        Put( x );
    end loop;

    New_line;
    Put( "done" );
    New_line;
end e;
