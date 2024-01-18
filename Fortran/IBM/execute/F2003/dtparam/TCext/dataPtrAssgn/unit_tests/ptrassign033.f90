! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign033.f
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

  class(*), pointer :: ptr1(:), ptr2(:)

  allocate(tar1(30))


  ptr1(15:)=>tar1

  select type (ptr1)
    type is (base(4))

     if(lbound(ptr1, dim=1).ne. 15) error stop 1
     if(ubound(ptr1, dim=1).ne. 44) error stop 2
     if(any(shape(ptr1).ne.(/30/))) error stop 3
   class default
     error stop 5
  end select
  if(.not.associated(ptr1,tar1)) error stop 4

  ptr2(10:)=>ptr1

  select type (ptr2)
    type is (base(4))

     if(lbound(ptr2, dim=1).ne. 10) error stop 6
     if(ubound(ptr2, dim=1).ne. 39) error stop 7
     if(any(shape(ptr2).ne.(/30/))) error stop 8
   class default
     error stop 10
  end select
   if(.not.associated(ptr2,tar1)) error stop 9


end
