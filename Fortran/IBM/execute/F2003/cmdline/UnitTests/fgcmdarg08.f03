! *********************************************************************
!*
!*  ===================================================================
!*
!*  FUNCTIONALITY TESTED       : LENGTH is the length of the argument
!*                             : regardless the absence of VALUE.
!*                             : STATUS is assigned 0 in this case.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgcmdarg08

        integer :: i1, i2, i3

        i1=2
        call GET_COMMAND_ARGUMENT(NUMBER=i1, LENGTH=i2, STATUS=i3)

        if (i2 .ne. 3) error stop 1
        if (i3 .ne. 0) error stop 2

      End Program fgcmdarg08