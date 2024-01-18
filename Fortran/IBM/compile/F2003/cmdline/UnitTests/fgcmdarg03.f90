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

      Program fgcmdarg03

        integer :: i1, i2, i3
        character(10) :: ch1

        i1=0
        call GET_COMMAND_ARGUMENT(i1, ch1, 5, i3)

      End Program fgcmdarg03