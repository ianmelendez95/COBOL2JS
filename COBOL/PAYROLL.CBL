       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROL00.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WHO        PIC X(15).
       77  WHERE      PIC X(20).
       77  WHY        PIC X(30).
       77  RATE       PIC 9(3).
       77  HOURS      PIC 9(3).
       77  GROSS-PAY  PIC 9(5).
       PROCEDURE DIVISION.
           MOVE  "Captain COBOL" TO WHO.
           MOVE "San Jose, California" TO WHERE.
           MOVE "Learn to be a COBOL expert" TO WHY.
           MOVE 19 TO HOURS.
           MOVE 23 TO RATE.
           COMPUTE GROSS-PAY = HOURS * RATE.
           DISPLAY "Name: " WHO.
           DISPLAY "Location: " WHERE.
           DISPLAY "Reason: " WHY.
           DISPLAY "Hours Worked: " HOURS.
           DISPLAY "Hourly Rate: " RATE.
           DISPLAY "Gross Pay: " GROSS-PAY.
           DISPLAY WHY " from " WHO.
           GOBACK.
