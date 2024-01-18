! *********************************************************************
!*
!*  ===================================================================
!*
!*  DIAGNOSES TESTED           : If one argument keyword is specified,
!*                             : the subsequent argument keywords must
!*                             : be specified as well.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgtenvar01

        integer :: i1, i2
        character(10) :: ch1, ch2

        call GET_ENVIRONMENT_VARIABLE(NAME=ch2, ch1, i1, i2)

      End Program fgtenvar01