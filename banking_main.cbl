IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKING-TRANSACTION-PROCESSOR.
       DATE-WRITTEN. 2024-01-15.
       REMARKS. DAILY BANKING TRANSACTION PROCESSING SYSTEM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE 
               ASSIGN TO "TRANSIN"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TRANSACTION-FILE-STATUS.
           
           SELECT ACCOUNT-MASTER-FILE 
               ASSIGN TO "ACCTMASTER"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS AM-CUSTOMER-ID
               FILE STATUS IS WS-ACCOUNT-FILE-STATUS.
           
           SELECT DAILY-REPORT-FILE 
               ASSIGN TO "DAILYREPORT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-REPORT-FILE-STATUS.
           
           SELECT ERROR-LOG-FILE 
               ASSIGN TO "ERRORLOG"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ERROR-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       * Transaction File Record Structure
       FD  TRANSACTION-FILE.
       01  TRANSACTION-RECORD.
           05  TR-CUSTOMER-ID         PIC X(10).
           05  TR-TRANSACTION-TYPE    PIC X.
               88  DEPOSIT-TRANSACTION    VALUE 'D'.
               88  WITHDRAWAL-TRANSACTION VALUE 'W'.
           05  TR-AMOUNT              PIC 9(7)V99.

       * Account Master File Record Structure
       FD  ACCOUNT-MASTER-FILE.
       01  ACCOUNT-RECORD.
           05  AM-CUSTOMER-ID         PIC X(10).
           05  AM-ACCOUNT-BALANCE     PIC 9(10)V99.
           05  AM-ACCOUNT-STATUS      PIC X(10).
               88  ACCOUNT-ACTIVE     VALUE 'ACTIVE'.
               88  ACCOUNT-SUSPENDED  VALUE 'SUSPENDED'.

       * Daily Report File Record Structure
       FD  DAILY-REPORT-FILE.
       01  REPORT-RECORD.
           05  RP-CUSTOMER-ID         PIC X(10).
           05  RP-TRANSACTION-TYPE    PIC X.
           05  RP-AMOUNT              PIC 9(7)V99.
           05  RP-STATUS              PIC X(10).

       * Error Log File Record Structure
       FD  ERROR-LOG-FILE.
       01  ERROR-RECORD.
           05  ER-ERROR-MESSAGE       PIC X(80).

       WORKING-STORAGE SECTION.
       * File Status Variables
       01  WS-FILE-STATUSES.
           05  WS-TRANSACTION-FILE-STATUS PIC XX.
           05  WS-ACCOUNT-FILE-STATUS     PIC XX.
           05  WS-REPORT-FILE-STATUS      PIC XX.
           05  WS-ERROR-FILE-STATUS       PIC XX.

       * Processing Statistics
       01  WS-PROCESSING-STATS.
           05  WS-TOTAL-TRANSACTIONS      PIC 9(5) COMP VALUE ZERO.
           05  WS-SUCCESSFUL-TRANSACTIONS PIC 9(5) COMP VALUE ZERO.
           05  WS-FAILED-TRANSACTIONS     PIC 9(5) COMP VALUE ZERO.

       * Work Areas and Flags
       01  WS-WORK-AREAS.
           05  WS-EOF-FLAG                PIC X VALUE 'N'.
               88  END-OF-FILE            VALUE 'Y'.
               88  NOT-END-OF-FILE        VALUE 'N'.
           
           05  WS-TRANSACTION-STATUS      PIC X.
               88  TRANSACTION-VALID      VALUE 'V'.
               88  TRANSACTION-INVALID    VALUE 'I'.

       * Date and Time Fields
       01  WS-CURRENT-DATE-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-YEAR            PIC 9(4).
               10  WS-MONTH           PIC 9(2).
               10  WS-DAY             PIC 9(2).
           05  WS-CURRENT-TIME.
               10  WS-HOUR            PIC 9(2).
               10  WS-MINUTE          PIC 9(2).
               10  WS-SECOND          PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-PROCESSING-ROUTINE.
           PERFORM INITIALIZATION-ROUTINE
           PERFORM PROCESS-TRANSACTIONS 
               UNTIL END-OF-FILE
           PERFORM GENERATE-SUMMARY-REPORT
           PERFORM CLEANUP-ROUTINE
           STOP RUN.

       INITIALIZATION-ROUTINE.
           * Open all required files
           OPEN INPUT  TRANSACTION-FILE
                I-O    ACCOUNT-MASTER-FILE
                OUTPUT DAILY-REPORT-FILE
                OUTPUT ERROR-LOG-FILE

           * Validate file openings
           PERFORM CHECK-FILE-STATUSES

           * Get current system date and time
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS
           
           * Display initialization message
           DISPLAY "BANKING TRANSACTION PROCESSOR"
           DISPLAY "DATE: " WS-YEAR "-" WS-MONTH "-" WS-DAY
           DISPLAY "TIME: " WS-HOUR ":" WS-MINUTE ":" WS-SECOND.

       CHECK-FILE-STATUSES.
           * Comprehensive file status checking
           IF WS-TRANSACTION-FILE-STATUS NOT = "00"
              MOVE "ERROR OPENING TRANSACTION FILE" TO ER-ERROR-MESSAGE
              WRITE ERROR-RECORD
              PERFORM ABNORMAL-TERMINATION
           END-IF.

           * Similar checks for other files...

       PROCESS-TRANSACTIONS.
           * Read next transaction record
           READ TRANSACTION-FILE
               AT END 
                   SET END-OF-FILE TO TRUE
                   GO TO EXIT-TRANSACTION-PROCESSING
           END-READ

           * Increment total transaction counter
           ADD 1 TO WS-TOTAL-TRANSACTIONS

           * Validate and process transaction
           PERFORM VALIDATE-TRANSACTION
           
           IF TRANSACTION-VALID
               PERFORM PROCESS-VALID-TRANSACTION
           ELSE
               PERFORM PROCESS-INVALID-TRANSACTION
           END-IF.

       EXIT-TRANSACTION-PROCESSING.
           EXIT.

       GENERATE-SUMMARY-REPORT.
           * Display processing summary
           DISPLAY "TRANSACTION PROCESSING SUMMARY"
           DISPLAY "Total Transactions:     " WS-TOTAL-TRANSACTIONS
           DISPLAY "Successful Transactions:" WS-SUCCESSFUL-TRANSACTIONS
           DISPLAY "Failed Transactions:    " WS-FAILED-TRANSACTIONS.

       CLEANUP-ROUTINE.
           * Close all files
           CLOSE TRANSACTION-FILE
                 ACCOUNT-MASTER-FILE
                 DAILY-REPORT-FILE
                 ERROR-LOG-FILE
           
           DISPLAY "BANKING TRANSACTION PROCESSING COMPLETE".

       ABNORMAL-TERMINATION.
           * Handle critical errors
           DISPLAY "CRITICAL ERROR: SYSTEM TERMINATING"
           MOVE "SYSTEM TERMINATED DUE TO CRITICAL ERROR" 
             TO ER-ERROR-MESSAGE
           WRITE ERROR-RECORD
           STOP RUN.

       COPY "transaction_processing.cbl".

       IDENTIFICATION DIVISION.
       * Optional declarative section for additional error handling
       DECLARATIVES.
       FILE-ERROR-HANDLER SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON 
               TRANSACTION-FILE 
               ACCOUNT-MASTER-FILE 
               DAILY-REPORT-FILE 
               ERROR-LOG-FILE.
           
           DISPLAY "FILE PROCESSING ERROR DETECTED"
           PERFORM ABNORMAL-TERMINATION.
       END DECLARATIVES.
