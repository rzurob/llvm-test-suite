!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: errimagenum02.f
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
!*  TEST CASE TITLE            : errimagenum02
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
!*  REQUIRED COMPILER OPTIONS  : -qcaf
!*  REQUIRED RUNTIME OPTIONS   : ERRIMAGENUM
!*
!*  DESCRIPTION                : Test behaviour in issueing runtime messages
!*                               when ERRIMAGENUM runtime option is set to yes.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      PROGRAM errimagenum02
! floating-point divide by zero
      open ( unit=16,                                                 &
             file='errimagenum02.dummy',                                     &
             status='old',                                            &
             form='formatted',                                        &
             access='sequential')
      end
