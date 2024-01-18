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
! %POSTCMD: dcomp fgtenvar06.f
! %END
! *********************************************************************
!*
!*  ===================================================================
!*
!*                             :
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  DIAGNOSES TESTED           : The LENGTH and STATUS must be
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

      Program fgtenvar06

        character(10)   :: ch1
        integer(kind=2) :: i1
        integer         :: i3

        ! Default integer type is 8 (specified by -qintsize=8)
        ! LENGTH is not default integer type while STATUS is.
        call GET_ENVIRONMENT_VARIABLE(NAME='HOME', LENGTH=i1,
     +           STATUS=i2, VALUE=ch1)

      End Program fgtenvar06
