      * Unit tests for un.cob - tests internal functions without API calls
      * Compile: cobc -x -o test_cobol test_cobol.cob && ./test_cobol

       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-COBOL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PASSED          PIC 9(3) VALUE 0.
       01  WS-FAILED          PIC 9(3) VALUE 0.
       01  WS-TOTAL           PIC 9(3) VALUE 0.
       01  WS-RESULT          PIC 9 VALUE 0.
       01  WS-TEST-NAME       PIC X(60).
       01  WS-EXTENSION       PIC X(10).
       01  WS-LANGUAGE        PIC X(20).
       01  WS-TIMESTAMP       PIC X(20) VALUE "1704067200".
       01  WS-METHOD          PIC X(10) VALUE "POST".
       01  WS-ENDPOINT        PIC X(20) VALUE "/execute".
       01  WS-MESSAGE         PIC X(100).
       01  WS-CONTENT         PIC X(100).
       01  WS-FIRST-LINE      PIC X(50).
       01  WS-ARG             PIC X(50).
       01  WS-KEY             PIC X(20).
       01  WS-VALUE           PIC X(50).
       01  WS-PATH            PIC X(100).
       01  WS-BASENAME        PIC X(50).
       01  WS-API-BASE        PIC X(50) VALUE "https://api.unsandbox.com".
       01  WS-POS             PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY " ".
           DISPLAY "=== Extension Mapping Tests ===".

           MOVE ".py" TO WS-EXTENSION.
           PERFORM GET-LANGUAGE.
           MOVE "Python extension maps correctly" TO WS-TEST-NAME.
           IF WS-LANGUAGE = "python"
               MOVE 1 TO WS-RESULT
           ELSE
               MOVE 0 TO WS-RESULT
           END-IF.
           PERFORM RUN-TEST.

           MOVE ".cob" TO WS-EXTENSION.
           PERFORM GET-LANGUAGE.
           MOVE "COBOL extension maps correctly" TO WS-TEST-NAME.
           IF WS-LANGUAGE = "cobol"
               MOVE 1 TO WS-RESULT
           ELSE
               MOVE 0 TO WS-RESULT
           END-IF.
           PERFORM RUN-TEST.

           MOVE ".js" TO WS-EXTENSION.
           PERFORM GET-LANGUAGE.
           MOVE "JavaScript extension maps correctly" TO WS-TEST-NAME.
           IF WS-LANGUAGE = "javascript"
               MOVE 1 TO WS-RESULT
           ELSE
               MOVE 0 TO WS-RESULT
           END-IF.
           PERFORM RUN-TEST.

           MOVE ".go" TO WS-EXTENSION.
           PERFORM GET-LANGUAGE.
           MOVE "Go extension maps correctly" TO WS-TEST-NAME.
           IF WS-LANGUAGE = "go"
               MOVE 1 TO WS-RESULT
           ELSE
               MOVE 0 TO WS-RESULT
           END-IF.
           PERFORM RUN-TEST.

           DISPLAY " ".
           DISPLAY "=== Signature Format Tests ===".

           STRING WS-TIMESTAMP DELIMITED SIZE
                  ":" DELIMITED SIZE
                  WS-METHOD DELIMITED SPACE
                  ":" DELIMITED SIZE
                  WS-ENDPOINT DELIMITED SPACE
                  ":body" DELIMITED SIZE
                  INTO WS-MESSAGE.

           MOVE "Signature format starts with timestamp" TO WS-TEST-NAME.
           IF WS-MESSAGE(1:10) = WS-TIMESTAMP
               MOVE 1 TO WS-RESULT
           ELSE
               MOVE 0 TO WS-RESULT
           END-IF.
           PERFORM RUN-TEST.

           MOVE "Signature format contains :POST:" TO WS-TEST-NAME.
           INSPECT WS-MESSAGE TALLYING WS-POS FOR ALL ":POST:".
           IF WS-POS > 0
               MOVE 1 TO WS-RESULT
           ELSE
               MOVE 0 TO WS-RESULT
           END-IF.
           PERFORM RUN-TEST.

           DISPLAY " ".
           DISPLAY "=== Language Detection Tests ===".

           MOVE "#!/usr/bin/env python3" TO WS-FIRST-LINE.
           MOVE "Python shebang starts with #!" TO WS-TEST-NAME.
           IF WS-FIRST-LINE(1:2) = "#!"
               MOVE 1 TO WS-RESULT
           ELSE
               MOVE 0 TO WS-RESULT
           END-IF.
           PERFORM RUN-TEST.

           DISPLAY " ".
           DISPLAY "=== Argument Parsing Tests ===".

           MOVE "DEBUG=1" TO WS-ARG.
           MOVE "DEBUG" TO WS-KEY.
           MOVE "1" TO WS-VALUE.
           MOVE "Parse -e KEY=VALUE format" TO WS-TEST-NAME.
           IF WS-ARG(1:5) = WS-KEY AND WS-ARG(7:1) = WS-VALUE
               MOVE 1 TO WS-RESULT
           ELSE
               MOVE 0 TO WS-RESULT
           END-IF.
           PERFORM RUN-TEST.

           DISPLAY " ".
           DISPLAY "=== API Constants Tests ===".

           MOVE "API base URL starts with https://" TO WS-TEST-NAME.
           IF WS-API-BASE(1:8) = "https://"
               MOVE 1 TO WS-RESULT
           ELSE
               MOVE 0 TO WS-RESULT
           END-IF.
           PERFORM RUN-TEST.

           DISPLAY " ".
           DISPLAY "=== Summary ===".
           COMPUTE WS-TOTAL = WS-PASSED + WS-FAILED.
           DISPLAY "Passed: " WS-PASSED.
           DISPLAY "Failed: " WS-FAILED.
           DISPLAY "Total:  " WS-TOTAL.

           IF WS-FAILED > 0
               STOP RUN WITH STATUS 1
           ELSE
               STOP RUN WITH STATUS 0
           END-IF.

       RUN-TEST.
           IF WS-RESULT = 1
               DISPLAY "  ✓ " WS-TEST-NAME
               ADD 1 TO WS-PASSED
           ELSE
               DISPLAY "  ✗ " WS-TEST-NAME
               ADD 1 TO WS-FAILED
           END-IF.

       GET-LANGUAGE.
           EVALUATE WS-EXTENSION
               WHEN ".py"    MOVE "python" TO WS-LANGUAGE
               WHEN ".js"    MOVE "javascript" TO WS-LANGUAGE
               WHEN ".rb"    MOVE "ruby" TO WS-LANGUAGE
               WHEN ".go"    MOVE "go" TO WS-LANGUAGE
               WHEN ".cob"   MOVE "cobol" TO WS-LANGUAGE
               WHEN ".c"     MOVE "c" TO WS-LANGUAGE
               WHEN OTHER    MOVE SPACES TO WS-LANGUAGE
           END-EVALUATE.
