! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qdebug=intmsg -qfixed
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp fgcmdarg05.f
! %END
! *********************************************************************
!*
!*  ===================================================================
!*
!*                             :
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  DIAGNOSES TESTED           : If argument keywords are not specified,
!*                             : the order of the argument is critical.
!*
!*  REQUIRED COMPILER OPTIONS  : -qfixed
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgcmdarg05

        character(10) :: ch1
        integer       :: i1, i2, i3

        ! valid since arg kwd is spedified.
        call GET_COMMAND_ARGUMENT(NUMBER=i1, LENGTH=i2, STATUS=i3,
     +           VALUE=ch1)

        ! not valid.
        call GET_COMMAND_ARGUMENT(i1, i2, i3, ch1)

      End Program fgcmdarg05

