! *********************************************************************
!*
!*  ===================================================================
!*
!*  DIAGNOSES TESTED           : If argument keywords are not specified,
!*                             : the order of the argument is critical.
!*
!*  REQUIRED COMPILER OPTIONS  : -qfixed
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgtenvar05

        character(10) :: ch1
        integer       :: i1, i2

        ! valid since arg kwd is spedified.
        call GET_ENVIRONMENT_VARIABLE(NAME="HOME", LENGTH=i1,
     +           STATUS=i2, VALUE=ch1)

        ! not valid.
        call GET_ENVIRONMENT_VARIABLE("HOME", i1, i2, ch1)

      End Program fgtenvar05

