procedure get_time( var tt: timetype );

begin
  tt.h := 0;
  tt.m := 0;
  tt.s := 0;
  tt.l := 0;
  Bdos( 180, Addr( tt ) );
end;
