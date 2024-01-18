! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign063.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign063.f
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
!*  TEST CASE TITLE            : ptrassign063
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
    end type
    
    type base2(k2,n2)    ! (4,20)
      integer, kind             :: k2
      integer, len              :: n2
      type(base(:,k2)), pointer :: ptr(:)
    end type
    
    type container(k3,n3)    ! (4,20)
      integer, kind      :: k3
      integer, len       :: n3
      type(base2(k3,n3)) :: b2
    end type
    
  end module
  
  use m
  
  type(container(4,20)) :: c1
  type(base(20,4)), target :: tar(50)=(/(base(20,4)(i),i=1,50)/)
  integer :: lowerb, upperb
  
  lowerb=25
  upperb=sum(ubound(tar))-1
  c1%b2%ptr(lowerb:upperb)=>tar
  
  associate(x=>c1%b2)
    
    if(lbound(x%ptr, dim=1).ne. 25) error stop 1
    if(ubound(x%ptr, dim=1).ne. 49) error stop 2
    if(any(shape(x%ptr).ne.(/25/))) error stop 3
    if(.not.associated(x%ptr,tar(1:25))) error stop 4
    
  end associate
  
    if(lbound(c1%b2%ptr, dim=1).ne. 25) error stop 5
    if(ubound(c1%b2%ptr, dim=1).ne. 49) error stop 6
    if(any(shape(c1%b2%ptr).ne.(/25/))) error stop 7
    if(.not.associated(c1%b2%ptr,tar(1:25))) error stop 8
  
end program

    
