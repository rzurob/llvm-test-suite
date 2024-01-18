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

      Program fgcmd03

        integer :: i1
        character(10) :: ch1

        call GET_COMMAND(ch1, 5, i1)

      End Program fgcmd03