! *********************************************************************
!*
!*  ===================================================================
!*
!*  FUNCTIONALITY TESTED       : If the NUMBER is invalid, VALUE is
!*                             : filled with blanks. LENGTH is assigned
!*                             : value 0, and STATUS is assigned value 1.
!*
!*  REQUIRED COMPILER OPTIONS  : -qfixed
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgcmdarg10

        integer :: i1, i2, i3
        character(len=2) :: ch1

        i1=10
        call GET_COMMAND_ARGUMENT(NUMBER=i1, VALUE=ch1, LENGTH=i2,
     +           STATUS=i3)

        if (ch1 .ne. "  ") error stop 1
        if (i2 .ne. 0) error stop 2
        if (i3 .ne. 1) error stop 3

      End Program fgcmdarg10