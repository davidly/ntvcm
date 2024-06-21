110 dim a%( 200 )
120 hi% = 200
130 x% = 0
140 n% = hi% - 1
150 if 1 > n% then goto 200
160 a%( n% ) = 1
170 n% = n% - 1
180 goto 150
200 a%( 1 ) = 2
210 a%( 0 ) = 0
230 hi% = hi% - 1
235 n% = hi%
244 rem no MOD so compute division then MOD manually
245 qu% = x% / n%
246 a%( n% ) = x% - ( n% * qu% )
250 n% = n% - 1
260 x% = ( a%( n% ) * 10 ) + qu%
280 if 0 <> n% then goto 245
300 print x%
330 if 10 <= hi% then goto 230
400 print " "
410 print "done"
420 system


