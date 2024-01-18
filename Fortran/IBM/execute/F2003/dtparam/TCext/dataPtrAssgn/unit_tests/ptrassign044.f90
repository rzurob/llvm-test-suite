! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign044.f
! opt variations: -qnol -qnodeferredlp

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign044.f
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
!*  TEST CASE TITLE            : ptrassign044
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

  type dt(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)   :: data
  end type
  
  type(dt(:,4)), pointer :: ptr1(:,:), ptr2(:)
  
  type(dt(20,4)), target :: tar1(100)=(/(dt(20,4)(i),i=1,100)/)
  
  ptr1(22:21,22:31)=>tar1

  if(lbound(ptr1,	dim=1).ne. 1) error stop 1
  if(lbound(ptr1, dim=2).ne. 22) error stop 2
 if(ubound(ptr1, dim=1).ne. 0) error stop 3
  if(ubound(ptr1, dim=2).ne. 31) error stop 4
  if(any(shape(ptr1).ne.(/0,10/))) error stop 5
  
  ptr2(25:24)=>tar1
  if(lbound(ptr2,	dim=1).ne. 1) error stop 6
  if(ubound(ptr2, dim=1).ne. 0) error stop 7
  if(any(shape(ptr2).ne.(/0/))) error stop 8
  
  
end
