!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign007.f
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
!*  TEST CASE TITLE            : ptrassign007
!*
!*  PROGRAMMER                 : Michael Selvanayagam
!*  DATE                       : March 31, 2007
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                :subscriptorder directive not allowed 
!*                              with bounds-spec or bounds-remapping
!*                              
!*
!234567890123456789012345678901234567890123456789012345678901234567890

!IBM* subscriptorder(ptr1(3,2,1))

  real, pointer :: ptr1(:,:,:)
  
  real, target :: arr1(1:5,2:6,3:7), arr2(1:10)
  
  ptr1(4:,5:,6:) => arr1
  
  ptr1(1:1,1:5,1:2)=>arr2
    
end
  
