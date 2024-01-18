!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign008.f
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
!*  TEST CASE TITLE            : ptrassign008
!*
!*  PROGRAMMER                 : Michael Selvanayagam
!*  DATE                       : March 31, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                :C724
!*                              
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  integer, pointer :: ptr1(:)
  

  !function returns integer, not okay
  ptr1(5:)=>func1()
  ptr1(5:15)=>func1()

  contains
    
  
    function func1()
      integer, target :: func1
      
      func1=0
    end function
end
  
