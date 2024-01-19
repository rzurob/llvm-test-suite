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

      Program fgcmd01

        integer :: i1, i2
        character(10) :: ch1

        call GET_COMMAND(COMMAND=ch1, i1, i2)

      End Program fgcmd01