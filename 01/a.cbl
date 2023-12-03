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
       01 WS-CUR-CHAR PIC A(1).
       01 WS-NUM PIC 9(2).

       PROCEDURE DIVISION.
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
                           END-IF
                       END-PERFORM

                       PERFORM VARYING WS-IDX FROM 256 BY -1
                               UNTIL WS-IDX < 1
                           MOVE WS-Foo-CONTENT(WS-IDX:1) TO WS-CUR-CHAR
                           IF WS-CUR-CHAR IS NUMERIC
                               MOVE WS-CUR-CHAR(1:1) TO WS-NUM(1:1)
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
