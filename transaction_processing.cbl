      *================================================================*
      * Transaction Processing Module                                 *
      * Contains routines for validating and processing transactions  *
      *================================================================*

       VALIDATE-TRANSACTION.
           * Reset transaction status
           MOVE 'V' TO WS-TRANSACTION-STATUS

           * Validate Customer ID exists
           MOVE TR-CUSTOMER-ID TO AM-CUSTOMER-ID
           READ ACCOUNT-MASTER-FILE
               INVALID KEY 
                   SET TRANSACTION-INVALID TO TRUE
                   MOVE "INVALID CUSTOMER ID" TO ER-ERROR-MESSAGE
                   WRITE ERROR-RECORD
           END-READ.

       PROCESS-VALID-TRANSACTION.
           * Handle Deposit Transaction
           IF DEPOSIT-TRANSACTION
               PERFORM PROCESS-DEPOSIT
           END-IF.

           * Handle Withdrawal Transaction
           IF WITHDRAWAL-TRANSACTION
               PERFORM PROCESS-WITHDRAWAL
           END-IF.

           * Update account master file
           REWRITE ACCOUNT-RECORD
               INVALID KEY 
                   MOVE "ACCOUNT UPDATE FAILED" TO ER-ERROR-MESSAGE
                   WRITE ERROR-RECORD
           END-REWRITE.

       PROCESS-DEPOSIT.
           * Add deposit amount to account balance
           ADD TR-AMOUNT TO AM-ACCOUNT-BALANCE
           
           * Log successful deposit to report
           MOVE TR-CUSTOMER-ID TO RP-CUSTOMER-ID
           MOVE TR-TRANSACTION-TYPE TO RP-TRANSACTION-TYPE
           MOVE TR-AMOUNT TO RP-AMOUNT
           MOVE "SUCCESS" TO RP-STATUS
           WRITE REPORT-RECORD

           ADD 1 TO WS-SUCCESSFUL-TRANSACTIONS.

       PROCESS-WITHDRAWAL.
           * Check if sufficient balance exists
           IF TR-AMOUNT > AM-ACCOUNT-BALANCE
               MOVE "INSUFFICIENT FUNDS" TO ER-ERROR-MESSAGE
               WRITE ERROR-RECORD
               
               * Log failed withdrawal
               MOVE TR-CUSTOMER-ID TO RP-CUSTOMER-ID
               MOVE TR-TRANSACTION-TYPE TO RP-TRANSACTION-TYPE
               MOVE TR-AMOUNT TO RP-AMOUNT
               MOVE "FAILED" TO RP-STATUS
               WRITE REPORT-RECORD
               
               ADD 1 TO WS-FAILED-TRANSACTIONS
           ELSE
               * Process valid withdrawal
               SUBTRACT TR-AMOUNT FROM AM-ACCOUNT-BALANCE
               
               * Log successful withdrawal
               MOVE TR-CUSTOMER-ID TO RP-CUSTOMER-ID
               MOVE TR-TRANSACTION-TYPE TO RP-TRANSACTION-TYPE
               MOVE TR-AMOUNT TO RP-AMOUNT
               MOVE "SUCCESS" TO RP-STATUS
               WRITE REPORT-RECORD
               
               ADD 1 TO WS-SUCCESSFUL-TRANSACTIONS
           END-IF.

       PROCESS-INVALID-TRANSACTION.
           * Log invalid transaction to error log
           MOVE "INVALID TRANSACTION DETECTED" TO ER-ERROR-MESSAGE
           WRITE ERROR-RECORD
           
           * Increment failed transaction counter
           ADD 1 TO WS-FAILED-TRANSACTIONS.
