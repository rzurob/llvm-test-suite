! *********************************************************************
!*
!*  ===================================================================
!*
!*  FUNCTIONALITY TESTED       : Specifing an empty string as COMMAND.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgcmd11

        integer :: i1, i2
        character(0) :: ch1

        call GET_COMMAND(COMMAND=ch1, LENGTH=i1, STATUS=i2)
        print*, len(ch1)
        if (len(ch1) .ne. 0) error stop 1
        if (i1 .ne. 16) error stop 2
        if (i2 .ne. -1) error stop 3

      End Program fgcmd11


