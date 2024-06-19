begin

integer function remainder( y, z );
    integer y, z;
begin
    integer result;

    result := y / z;
    remainder := y - ( z * result );
end;

procedure main;
begin
    integer array a[0:200];
    integer high, n, x;

    high := 200;
    x := 0;
    
    n := high - 1;
    while n > 0 do begin
        a[ n ] := 1;
        n := n - 1;
    end;
    
    a[ 1 ] := 2;
    a[ 0 ] := 0;
    
    while high > 9 do begin
        n := high;
        high := high - 1;
        while 0 <> n do begin
            a[ n ] := remainder( x, n );
            x := 10 * a[ n - 1 ] + x / n;
            n := n - 1;
        end;
        writeon( x );
    end;
    
    write( "done" );
end;

main;

end

