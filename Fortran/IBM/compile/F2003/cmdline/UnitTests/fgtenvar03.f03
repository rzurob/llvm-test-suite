! *********************************************************************
!*
!*  ===================================================================
!*
!*  DIAGNOSES TESTED           : Constant cannot be passed as INTENT(OUT)
!*                             : argument.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgtenvar03

        integer :: i1, i2
        character(10) :: ch1, ch2

        call GET_ENVIRONMENT_VARIABLE(ch2, ch1, LENGTH=10, STATUS=i2)

      End Program fgtenvar03