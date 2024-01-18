!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: errimagenum01.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : errimagenum01
!*
!*  PROGRAMMER                 : Xing Xue
!*  DATE                       : August 10, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Allow users to choose to prepend image number
!*                               to the traceback info
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : *_r ( thread-safe )
!*  REQUIRED COMPILER OPTIONS  : -qcaf -qflttrap=EN:ZERO -qsigtrap
!*  REQUIRED RUNTIME OPTIONS   : ERRIMAGENUM
!*
!*  DESCRIPTION                : Test behaviour of the traceback
!*                               when ERRIMAGENUM runtime option is set to yes.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      PROGRAM ERRIMAGENUM01
! floating-point divide by zero
      volatile res
      open ( unit=16,                                                 &
             file='fxxd0001.out',                                     &
             status='replace',                                            &
             form='formatted',                                        &
             access='sequential')

      do 987 i=1,1000
         res=1.0/(i-500)
         write(16,"(I10)") i
987   continue
      print *, res ! added to ensure divide by zero not opted out
      end
