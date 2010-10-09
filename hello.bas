#lang reader "lang/reader.rkt"
100 PRINT "Your Name?"
110 INPUT name
120 LET x = 0
130 PRINT x, "HELLO", name
140 IF x < 10 THEN 150 ELSE 170
150 LET x = x + 1
160 GOTO 130
170 END