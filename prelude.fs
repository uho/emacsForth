cr
.( prelude.fs ) cr

testing stack

t{ 10 20 -> 10 20 }t

t{ 1 1 + -> 2 }t



testing conditional

: oneway if 42 then ;

t{ 0 oneway -> }t
t{ 1 oneway -> 42 }t

: twoway  if 42 else  43 then 44 ;

t{ 0 twoway -> 43 44 }t
t{ 1 twoway -> 42 44 }t

testing loops

: until-loop  begin dup 1 - dup 0= until drop ;

t{ 5 until-loop -> 5 4 3 2 1 }t

: while-loop  begin dup while dup 1 - repeat ;

t{ 5 while-loop -> 5 4 3 2 1 0 }t


: iterations  begin dup while 1 - repeat drop ; \ 500 times as slow as gforth 0.7.3


cr cr .( done )