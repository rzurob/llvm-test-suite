! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qintsize=4
! %GROUP: fgcmdarg09.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS: aa bbb cccc
! %POSTCMD:
! %END
! *********************************************************************
!*
!*  ===================================================================
!*
!*                             :
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  FUNCTIONALITY TESTED       : STATUS is assigned value -1 if the
!*                             : length of VALUE is less than the
!*                             : length of the argument regardless
!*                             : the absence of LENGTH.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgcmdarg09

        integer :: i1, i2, i3
        character(len=2) :: ch1

        i1=2
        call GET_COMMAND_ARGUMENT(NUMBER=i1, VALUE=ch1, STATUS=i3)

        if (ch1 .ne. "bb") error stop 1
        if (i3 .ne. -1) error stop 2

      End Program fgcmdarg09