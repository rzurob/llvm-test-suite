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
! %POSTCMD: dcomp fgcmdarg04.f 
! %END
! *********************************************************************
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : For GET_COMMAND_ARGUMENT intrinsic.
!*                             : 
!*  PROGRAMMER                 : Daniel Chen
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  DIAGNOSES TESTED           : A variable that is INTENT(IN) cannot 
!*                             : be passed as INTENT(OUT) argument.
!*
!*  DRIVER STANZA              : 
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgcmdarg04
      
        character(10) :: ch1
        
        ch1 = 'constant'
        call sub(ch1)
      
      End Program fgcmdarg04
      
      Subroutine sub(ch1)
         
         character*(*) :: ch1
         intent(in)    :: ch1
         
         integer :: i1, i2, i3
         
         call GET_COMMAND_ARGUMENT(i1, ch1, i2, i3)
         
      end subroutine sub 