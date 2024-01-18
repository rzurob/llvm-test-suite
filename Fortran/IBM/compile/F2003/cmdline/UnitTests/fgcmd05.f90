! *********************************************************************
!*
!*  ===================================================================
!*
!*  DIAGNOSES TESTED           : If argument keywords are not specified,
!*                             : the order of the argument is critical.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgcmd05

        character(10) :: ch1
        integer       :: i1, i2

        ! valid since arg kwd is spedified.
        call GET_COMMAND(LENGTH=i1, STATUS=i2, COMMAND=ch1)

        ! not valid.
        call GET_COMMAND(i1, i2, ch1)

      End Program fgcmd05

