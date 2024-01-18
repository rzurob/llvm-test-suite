! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qintsize=4
! %GROUP: fgcmd12.f
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
!*  FUNCTIONALITY TESTED       : Has COMMAND only.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgcmd12

        character(14) :: ch1

        call GET_COMMAND(COMMAND=ch1)
        if (ch1 .ne. "./fgcmd12 aa b") error stop 1

      End Program fgcmd12
