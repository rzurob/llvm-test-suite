! *********************************************************************
!*
!*  ===================================================================
!*
!*  FUNCTIONALITY TESTED       : COMMAND takes the value of the entire
!*                             : command line. LENGTH is the length
!*                             : of the command line. STATUS is 0
!*                             : when the length of COMMAND is greater
!*                             : or equal to LENGTH, and -1 otherwise.
!*                             : STATUS is assigned value 1 if the
!*                             : retrieval fails.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgcmd07

        integer :: i1, i2
        character(14) :: ch1
        character(16) :: ch2

        call GET_COMMAND(COMMAND=ch1, LENGTH=i1, STATUS=i2)
        if (ch1 .ne. "./fgcmd07 aa b") error stop 1_4
        if (i1 .ne. 16) error stop 2_4
        if (i2 .ne. -1) error stop 3_4

        call GET_COMMAND(COMMAND=ch2, LENGTH=i1, STATUS=i2)
        if (ch2 .ne. "./fgcmd07 aa bbb") error stop 4_4
        if (i1 .ne. 16) error stop 5_4
        if (i2 .ne. 0) error stop 6_4

      End Program fgcmd07
