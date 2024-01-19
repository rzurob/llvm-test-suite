! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign025.f
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

  class(base(4)), pointer :: ptr(:,:,:), ptr2

  allocate(tar1(27),source=(/(child(4)(i,i),i=1,27)/))


  ptr(0:2,3:5,6:8)=>tar1

  select type (ptr)
    type is (child(4))

     if(lbound(ptr, dim=1).ne. 0) error stop 1
     if(lbound(ptr, dim=2).ne. 3) error stop 2
     if(lbound(ptr, dim=3).ne. 6) error stop 3
     if(ubound(ptr, dim=1).ne. 2) error stop 4
     if(ubound(ptr, dim=2).ne. 5) error stop 5
     if(ubound(ptr, dim=3).ne. 8) error stop 6
     if(any(shape(ptr).ne.(/3,3,3/))) error stop 7
     if(loc(ptr).ne.loc(tar1)) error stop 8
     do i=6,8
       do j=3,5
         do k=0,2
           ptr2=>ptr(k,j,i)
           if(.not.associated(ptr2,tar1(num))) error stop 10
           num=num+1
         end do
       end do
     end do
   class default
     error stop 9
  end select


end

