! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign027.f
! opt variations: -ql -qreuse=none

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :functional testing of bounds-remapping and bounds-spec
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  type base(k1)    ! (4)
    integer, kind :: k1
    integer(k1)   :: num1
  end type

  type ,extends(base) :: child    ! (4)
    integer(k1) :: num2
  end type

  integer :: num=1

  class(base(4)), allocatable, target :: tar1(:)

  class(base(4)), pointer :: ptr(:,:), ptr2

  allocate(tar1(30),source=(/(base(4)(i),i=1,30)/))


  ptr(1:4,1:5)=>tar1(1:20)

  select type (ptr)
    type is (base(4))

     if(lbound(ptr, dim=1).ne. 1) error stop 1
     if(lbound(ptr, dim=2).ne. 1) error stop 2
     if(ubound(ptr, dim=1).ne. 4) error stop 3
     if(ubound(ptr, dim=2).ne. 5) error stop 4
     if(any(shape(ptr).ne.(/4,5/))) error stop 5

     do i=1,5
       do j=1,4
           ptr2=>ptr(j,i)
           if(.not.associated(ptr2,tar1(num))) error stop 6
           num=num+1
       end do
     end do
   class default
     error stop 7
  end select


end



