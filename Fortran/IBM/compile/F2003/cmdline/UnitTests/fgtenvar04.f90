! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qdebug=intmsg
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp fgtenvar04.f
! %END
! *********************************************************************
!*
!*  ===================================================================
!*
!*                             :
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  DIAGNOSES TESTED           : A variable that is INTENT(IN) cannot
!*                             : be passed as INTENT(OUT) argument.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgtenvar04

        character(10) :: ch1

        ch1 = 'constant'
        call sub(ch1)

      End Program fgtenvar04

      Subroutine sub(ch1)

         character*(*) :: ch1
         intent(in)    :: ch1

         integer :: i1, i2

         call GET_ENVIRONMENT_VARIABLE(ch1, ch1, LENGTH=i1, STATUS=i2)

      end subroutine sub