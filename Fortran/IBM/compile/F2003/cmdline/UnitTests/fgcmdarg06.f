! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qdebug=intmsg -qintsize=8 -qfixed
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp fgcmdarg06.f
! %END
! *********************************************************************
!*
!*  ===================================================================
!*
!*                             :
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  DIAGNOSES TESTED           : The NUMBER, LENGTH and STATUS must be
!*                             : of type default integer.
!*
!*  REQUIRED COMPILER OPTIONS  : -qfixed
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgcmdarg06

        character(10)   :: ch1
        integer(kind=2) :: i1, i2
        integer         :: i3

        ! Default integer type is 8 (specified by -qintsize=8)
        ! NUMBER and LENGTH are not default integer type while STATUS is.
        call GET_COMMAND_ARGUMENT(NUMBER=i1, LENGTH=i2, STATUS=i3,
     +           VALUE=ch1)

      End Program fgcmdarg06
