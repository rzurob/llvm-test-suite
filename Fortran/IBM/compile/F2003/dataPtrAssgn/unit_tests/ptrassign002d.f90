!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign013.f
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
!*  TEST CASE TITLE            : ptrassign013
!*
!*  PROGRAMMER                 : Michael Selvanayagam
!*  DATE                       : March 31, 2006s
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=polymorphic
!*
!*  DESCRIPTION                :C717 - diagnostic
!*                              
!*
!234567890133456789013345678901334567890133456789013345678901334567890


  type dt1
    real :: num1
  end type
  
   
  class(dt1), pointer :: ptr1(:)
  class(*), target, allocatable :: arr1(:)
  class(*), pointer :: ptr2(:)
  type(dt1), pointer :: ptr3(:)
  
  allocate(arr1(5), source=(/1,1,1,1,1/))
  
  ptr1(5:)=>arr1
  ptr1(10:14)=>ptr2
  ptr3(15:19)=>arr1
  ptr3(15:)=>ptr2


end
