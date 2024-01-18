! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qintsize=8
! %GROUP: fgcmd08.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS: aa bbb
! %POSTCMD:
! %END
! *********************************************************************
!*
!*  ===================================================================
!*
!*                             :
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  FUNCTIONALITY TESTED       : LENGTH is the length of the argument
!*                             : regardless the absence of COMMAND.
!*                             : STATUS is assigned 0 in this case.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : The executable is named ./file
!*                             : It is ./fgcmd08 for this test case.
!*                             : It affects the LENGTH of the COMMAND
!*                             : if the name of the file is changed.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgcmd08

        integer :: i1, i2

        call GET_COMMAND(LENGTH=i1, STATUS=i2)

        if (i1 .ne. 16) error stop 1_4
        if (i2 .ne. 0) error stop 2_4

      End Program fgcmd08
