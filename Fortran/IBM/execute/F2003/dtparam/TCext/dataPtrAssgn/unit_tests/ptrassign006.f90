! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign006.f
! opt variations: -qnol -qnodeferredlp

!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign071.f
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
!*  TEST CASE TITLE            : ptrassign071
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
!*  DESCRIPTION                :functional testing of bounds-remapping and bounds-spec
!*                              
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  module m
  
    type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: data
      contains
        procedure, nopass :: assign=>ptrassign
    end type
    contains
      
      subroutine ptrassign(ptr, tar)
        type(base(:,4)), pointer :: ptr(:,:)
        type(base(*,4)), target  :: tar(:,:)
        
        ptr(6:,6:)=>tar(1:5,1:5)
        
        if(lbound(ptr, dim=1).ne. 6) error stop 1
        if(lbound(ptr, dim=2).ne. 6) error stop 2
        if(ubound(ptr, dim=1).ne. 10) error stop 3
        if(ubound(ptr, dim=2).ne. 10) error stop 4
        if(any(shape(ptr).ne.(/5,5/))) error stop 5
        if(.not.associated(ptr,tar(1:5,1:5))) error stop 6
        
      end subroutine
    
  
  end module

  use m
  
  type(base(:,4)), pointer :: ptr1(:,:)
  type(base(:,4)), target, allocatable  :: tar1(:,:)
  
  allocate(tar1(8,6), source=base(20,4)(1))
  
  call tar1%assign(ptr1,tar1)
  
  if(lbound(ptr1, dim=1).ne. 6) error stop 7
  if(lbound(ptr1, dim=2).ne. 6) error stop 8
  if(ubound(ptr1, dim=1).ne. 10) error stop 9
  if(ubound(ptr1, dim=2).ne. 10) error stop 10
  if(any(shape(ptr1).ne.(/5,5/))) error stop 11
  if(.not.associated(ptr1,tar1(1:5,1:5))) error stop 12
  
  
  
end
