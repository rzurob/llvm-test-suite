! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qdebug=intmsg -qintsize=8
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp fgcmd06.f
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
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgcmd06

        character(10)   :: ch1
        integer(kind=2) :: i1
        integer         :: i2

        ! Default integer type is 8 (specified by -qintsize=8)
        ! LENGTH is not default integer type while STATUS is.
        call GET_COMMAND(LENGTH=i1, STATUS=i2, COMMAND=ch1)

      End Program fgcmd06
