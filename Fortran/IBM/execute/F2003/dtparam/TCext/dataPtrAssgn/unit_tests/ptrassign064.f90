! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign064.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign064.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :functional testing of bounds-remapping and bounds-spec
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
      type(base(:,k2)), pointer :: ptr(:,:)
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
  type(base(:,4)), pointer :: ptr2
  integer :: num, lowerb1,lowerb2,upperb1,upperb2

  lowerb1=int(25.0)
  lowerb2=lowerb1+int(sqrt(real(lowerb1)))
  upperb1=lowerb2+4
  upperb2=upperb1
  c1%b2%ptr(lowerb1:upperb1,lowerb2:upperb2)=>tar

  associate(x=>c1%b2)
    num=1
    if(lbound(x%ptr, dim=1).ne. 25) error stop 1
    if(lbound(x%ptr, dim=2).ne. 30) error stop 2
    if(ubound(x%ptr, dim=1).ne. 34) error stop 3
    if(ubound(x%ptr, dim=2).ne. 34) error stop 4
    if(any(shape(x%ptr).ne.(/10,5/))) error stop 5

    do i=30,34
      do j=25,34
        ptr2=>x%ptr(j,i)
        if(.not.associated(ptr2,tar(num))) error stop 6
        num=num+1
      end do
    end do
    num=1
  end associate

    if(lbound(c1%b2%ptr, dim=1).ne. 25) error stop 7
    if(lbound(c1%b2%ptr, dim=2).ne. 30) error stop 8
    if(ubound(c1%b2%ptr, dim=1).ne. 34) error stop 9
    if(ubound(c1%b2%ptr, dim=2).ne. 34) error stop 10
    if(any(shape(c1%b2%ptr).ne.(/10,5/))) error stop 11

    do i=30,34
      do j=25,34
        ptr2=>c1%b2%ptr(j,i)
        if(.not.associated(ptr2,tar(num))) error stop 12
        num=num+1
      end do
    end do

end program


