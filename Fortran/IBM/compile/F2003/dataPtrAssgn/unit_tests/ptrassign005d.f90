!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign005.f
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
!*  TEST CASE TITLE            : ptrassign005
!*
!*  PROGRAMMER                 : Michael Selvanayagam
!*  DATE                       : March 31, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                :C719
!*                              
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  type dt
    integer :: num1
  end type
  
  type(dt) , pointer :: ptr1(:,:,:)
  
  type(dt), target :: arr2(1:10)
  
  !incorrect rank for pointer
  ptr1(4:8,5:9,6:10,7:11)=>arr1
  
  ptr1(4:8,5:9)=>arr1
  
  ptr1(4:8,5:9,6:10,7:11)=>arr1
  
  ptr1(4:8,5:9)=>arr1
  
  
end
  
