       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC01A.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT Foo ASSIGN TO "ROW-FILE"
       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD Foo.
       01 Foo-FILE.
           05 Foo-CONTENT PIC A(256).

       WORKING-STORAGE SECTION.
       01 WS-Foo.
           05 WS-Foo-CONTENT PIC A(256).
       01 WS-EOF PIC A(1).
       01 WS-SUM PIC 9(30) VALUE 0.
       01 WS-IDX PIC 9(3) VALUE 0.
       01 WS-IDY PIC 9(3) VALUE 0.
       01 WS-IDZ PIC 9(3) VALUE 0.
       01 WS-IDW PIC 9(3) VALUE 0.
       01 WS-ANS PIC A(1).
       01 WS-CUR-CHAR PIC A(1).
       01 WS-NUM PIC 9(2).
       01 WS-LOOKUP PIC X(10) OCCURS 10 TIMES.
       01 WS-CUR-LOOK PIC X(10).

       PROCEDURE DIVISION.
       INITIALIZATION.
       MOVE "one " TO WS-LOOKUP (1).
       MOVE "two " TO WS-LOOKUP (2).
       MOVE "three " TO WS-LOOKUP (3).
       MOVE "four " TO WS-LOOKUP (4).
       MOVE "five " TO WS-LOOKUP (5).
       MOVE "six " TO WS-LOOKUP (6).
       MOVE "seven " TO WS-LOOKUP (7).
       MOVE "eight " TO WS-LOOKUP (8).
       MOVE "nine " TO WS-LOOKUP (9).

       MAIN-LOGIC.
       OPEN INPUT Foo.
           PERFORM UNTIL WS-EOF='Y'
               READ Foo INTO WS-Foo
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       DISPLAY WS-Foo-CONTENT

                       PERFORM VARYING WS-IDX FROM 1 BY 1
                               UNTIL WS-IDX > 255
                           MOVE WS-Foo-CONTENT(WS-IDX:1) TO WS-CUR-CHAR
                           IF WS-CUR-CHAR IS NUMERIC
                               MOVE WS-CUR-CHAR(1:1) TO WS-NUM(2:1)
                           ELSE
                               PERFORM VARYING WS-IDY FROM 1 BY 1
                                       UNTIL WS-IDY > 10
                                   MOVE 'Y' TO WS-ANS
                                   MOVE WS-IDX TO WS-IDW
                                   MOVE WS-LOOKUP(WS-IDY) TO WS-CUR-LOOK
                                   PERFORM VARYING WS-IDZ FROM 1 BY 1
                                           UNTIL WS-IDZ > 10
                                       IF WS-CUR-LOOK(WS-IDZ:1) = " "
                                           MOVE 11 TO WS-IDZ
                                       ELSE
                                           IF NOT WS-CUR-LOOK(WS-IDZ:1)
                                               =
                                               WS-Foo-CONTENT(WS-IDW:1)
                                               MOVE 'N' TO WS-ANS
                                               MOVE 11 TO WS-IDZ
                                           END-IF
                                           ADD 1 TO WS-IDW
                                       END-IF
                                   END-PERFORM

                                   IF WS-ANS = 'Y'
                                       MOVE WS-IDY(3:1) TO WS-NUM(2:1)
                                       MOVE 11 TO WS-IDY
                                   END-IF
                               END-PERFORM
                           END-IF
                       END-PERFORM

                       PERFORM VARYING WS-IDX FROM 256 BY -1
                               UNTIL WS-IDX < 1
                           MOVE WS-Foo-CONTENT(WS-IDX:1) TO WS-CUR-CHAR
                           IF WS-CUR-CHAR IS NUMERIC
                               MOVE WS-CUR-CHAR(1:1) TO WS-NUM(1:1)
                           ELSE
                               PERFORM VARYING WS-IDY FROM 1 BY 1
                                       UNTIL WS-IDY > 10
                                   MOVE 'Y' TO WS-ANS
                                   MOVE WS-IDX TO WS-IDW
                                   MOVE WS-LOOKUP(WS-IDY) TO WS-CUR-LOOK
                                   PERFORM VARYING WS-IDZ FROM 1 BY 1
                                           UNTIL WS-IDZ > 10
                                       IF WS-CUR-LOOK(WS-IDZ:1) = " "
                                           MOVE 11 TO WS-IDZ
                                       ELSE
                                           IF NOT WS-CUR-LOOK(WS-IDZ:1)
                                               =
                                               WS-Foo-CONTENT(WS-IDW:1)
                                               MOVE 'N' TO WS-ANS
                                               MOVE 11 TO WS-IDZ
                                           END-IF
                                           ADD 1 TO WS-IDW
                                       END-IF
                                   END-PERFORM

                                   IF WS-ANS = 'Y'
                                       MOVE WS-IDY(3:1) TO WS-NUM(1:1)
                                       MOVE 11 TO WS-IDY
                                   END-IF
                               END-PERFORM
                           END-IF
                       END-PERFORM

                       DISPLAY WS-NUM
                       ADD WS-NUM TO WS-SUM
               END-READ
           END-PERFORM
       CLOSE Foo.

       DISPLAY "FINAL ANSWER"
       DISPLAY WS-SUM
       STOP RUN.
