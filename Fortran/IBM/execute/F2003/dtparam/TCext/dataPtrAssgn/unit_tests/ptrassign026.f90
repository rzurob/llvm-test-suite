! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign026.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign026.f
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

  type base(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)   :: num1
  end type

  type ,extends(base) :: child    ! (20,4)
    integer(k1) :: num2
  end type

  integer :: num=1

  type(child(:,4)), allocatable, target :: tar1(:)

  class(base(:,4)), pointer :: ptr(:,:), ptr2

  allocate(tar1(25),source=(/(child(20,4)(i,i),i=1,25)/))

  ptr(-10:-6,-5:-1)=>tar1

  select type (ptr)
    type is (child(*,4))

     if(lbound(ptr, dim=1).ne. -10) error stop 1
     if(lbound(ptr, dim=2).ne. -5) error stop 2
     if(ubound(ptr, dim=1).ne. -6) error stop 3
     if(ubound(ptr, dim=2).ne. -1) error stop 4
     if(any(shape(ptr).ne.(/5,5/))) error stop 5

     do i=-5,-1
       do j=-10,-6
           ptr2=>ptr(j,i)
           if(.not.associated(ptr2,tar1(num))) error stop 6
           num=num+1
       end do
     end do
   class default
     error stop 7
  end select


end

