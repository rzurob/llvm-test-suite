! *********************************************************************
!*
!*  ===================================================================
!*
!*  DIAGNOSES TESTED           : The same argument keyword cannot be
!*                             : specified more than once.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgcmd02

        integer :: i1, i2
        character(10) :: ch1

        call GET_COMMAND(ch1, LENGTH=i1, LENGTH=i2)

      End Program fgcmd02