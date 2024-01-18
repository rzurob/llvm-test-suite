! *********************************************************************
!*
!*  ===================================================================
!*
!*  DIAGNOSES TESTED           : The same argument keyword cannot be
!*                             : specified more than once.
!*
!*  REQUIRED COMPILER OPTIONS  : -qfixed
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgtenvar02

        integer :: i1, i2
        character(10) :: ch1, ch2

        call GET_ENVIRONMENT_VARIABLE(NAME=ch2, NAME=ch1,
     +           LENGTH=i1, STATUS=i2)

      End Program fgtenvar02