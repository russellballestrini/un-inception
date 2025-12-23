       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIBONACCI.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 I PIC 99 VALUE 0.
       01 J PIC 99 VALUE 0.
       01 N-STR PIC X(4).
       01 VAL-STR PIC X(4).
       01 FIB-ARRAY.
          05 FIB-VALS PIC 9999 OCCURS 11 TIMES.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           MOVE 0 TO FIB-VALS(1).
           MOVE 1 TO FIB-VALS(2).

           PERFORM VARYING I FROM 3 BY 1 UNTIL I > 11
               COMPUTE FIB-VALS(I) = FIB-VALS(I - 1) + FIB-VALS(I - 2)
           END-PERFORM.

           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 11
               COMPUTE I = J - 1
               MOVE I TO N-STR
               INSPECT N-STR REPLACING LEADING "0" BY " "
               MOVE FIB-VALS(J) TO VAL-STR
               INSPECT VAL-STR REPLACING LEADING "0" BY " "
               DISPLAY "fib(" FUNCTION TRIM(N-STR) ") = "
                       FUNCTION TRIM(VAL-STR)
           END-PERFORM.

           STOP RUN.
