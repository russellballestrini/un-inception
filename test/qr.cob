       IDENTIFICATION DIVISION.
       PROGRAM-ID. QRCODE-TEST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 QR-PTR          USAGE POINTER.
       01 QR-VERSION      PIC S9(9) COMP-5.
       01 QR-WIDTH        PIC S9(9) COMP-5.
       01 QR-DATA-PTR     USAGE POINTER.
       01 WS-TEXT          PIC X(20) VALUE "unsandbox-qr-ok"
                           & X"00".
       01 WS-ZERO         PIC S9(9) COMP-5 VALUE 0.
       01 WS-ONE          PIC S9(9) COMP-5 VALUE 1.
       01 WS-TWO          PIC S9(9) COMP-5 VALUE 2.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           CALL "QRcode_encodeString"
               USING BY REFERENCE WS-TEXT
                     BY VALUE WS-ZERO
                     BY VALUE WS-ONE
                     BY VALUE WS-TWO
                     BY VALUE WS-ONE
               RETURNING QR-PTR.

           IF QR-PTR NOT = NULL
               SET ADDRESS OF QR-VERSION TO QR-PTR
               SET ADDRESS OF QR-WIDTH TO QR-PTR
               DISPLAY "QR:unsandbox-qr-ok:ROWS:" QR-WIDTH
               CALL "QRcode_free" USING BY VALUE QR-PTR
           ELSE
               DISPLAY "QR encode failed"
           END-IF.

           STOP RUN.
