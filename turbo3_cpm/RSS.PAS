{ RSS reader app. Uses non-cp/m-standard NTVCM extensions for sleep and RSS }

program rss;

var
  feeds: array[ 0..4 ] of integer;
  buff: array[ 0..2047 ] of byte;
  do_sleep: boolean;

procedure bdos_sleep( ms: integer );
begin
  bdos( 106, ms );
end;

function bdos_feed_load : integer;
begin
  bdos_feed_load := BdosHL( 107, Addr( feeds ) );
end;

function bdos_feed_get( i: integer ) : integer;
begin
  Bdos( 26, Addr( buff ) ); { set dma address }
  bdos_feed_get := Bdos( 108, i );
end;

procedure halt_on_keystroke;
var ch: char;
begin
  if ( keypressed ) then
  begin
    while ( keypressed ) do
      Read( kbd, ch ); { consume the keystroke }
    halt;
  end;
end;

function chars_to_sp_or_end( o: integer ) : integer;
var
  start: integer;
begin
  start := o;
  while ( ( 0 <> buff[o] ) and ( 32 <> buff[o] ) ) do
    o := 1 + o;
  chars_to_sp_or_end := o - start;
end;

procedure show_item;
var
  sofar, o, col, tosp: integer;
  skip: boolean;

begin { show_item }
  sofar := 0;
  o := 0;
  repeat
    col := 1;
    while ( 0 <> buff[ o ] ) do
    begin
      skip := false;
      if ( 32 = buff[o] ) then
      begin
        tosp := chars_to_sp_or_end( 1 + o );
        if ( ( col + tosp ) > 80 ) then
        begin
          writeln;
          col := 1;
          skip := true;
        end;
      end;
      if ( skip = false ) then
      begin
        write( chr( buff[o] ) );
        halt_on_keystroke;
        if ( do_sleep ) then bdos_sleep( 33 );
        col := 1 + col;
      end;
      o := 1 + o;
    end;
    writeln;
    o := 1 + o;
    sofar := 1 + sofar;
  until ( 3 = sofar );
  writeln;
end;

var
  j, i, count, ok, shown: integer;
  feed1, feed2, feed3, feed4: string[128];
const
  items_shown_before_reload = 200;

begin { rss }
  feed1 := 'https://rss.nytimes.com/services/xml/rss/nyt/World.xml';
  feed2 := 'https://www.theguardian.com/world/rss';
  feed3 := 'https://feeds.washingtonpost.com/rss/world';
  feed4 := 'https://feeds.npr.org/1001/rss.xml';
  feeds[0] := 1 + Addr( feed1 );
  feeds[1] := 1 + Addr( feed2 );
  feeds[2] := 1 + Addr( feed3 );
  feeds[3] := 1 + Addr( feed4 );
  feeds[4] := 0; { last item must be 0 to indicate end }

  shown := items_shown_before_reload;
  count := mem[ 128 ];  { command tail length. paramcount not in v1.0 }
  do_sleep := ( 0 = count ); { if any arguments, don't sleep }

  repeat
    if ( shown >= items_shown_before_reload ) then
    begin
      writeln( '<loading rss feeds>' );
      count := bdos_feed_load;
      if ( 0 = count ) then
      begin
        writeln( 'no rss results' );
        halt;
      end;
    end;

    for i := 0 to count - 1 do
    begin
      ok := bdos_feed_get( i );
      if ( 0 <> ok ) then
      begin
        shown := 1 + shown;
        show_item;
        if do_sleep then
        begin
          for j := 1 to 20 do
          begin
            halt_on_keystroke;
            bdos_sleep( 100 );
          end;
        end;
      end;
    end;
  until false;
end. { rss }
