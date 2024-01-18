!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign006.f
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
!*  TEST CASE TITLE            : ptrassign006
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
!*  DESCRIPTION                :C720
!*                              
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  type dtptr
    integer, pointer :: ptr1(:,:,:,:)
    integer, pointer :: ptr2(:,:)
  end type
  
  integer, target :: arr1(1:5,2:6,3:7)

  type(dtptr) :: dtptr1
  
  !rank or pointer does not match rank of target
  dtptr1%ptr1(4:8,5:9,6:10,11:11)=>arr1
  
  !rank of pointer does not match rank of traget
  dtptr1%ptr2(4:8,5:9)=>arr1

    
end
  
