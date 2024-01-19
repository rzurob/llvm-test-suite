!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 5 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Dummy Argument with deferred length
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. If actual argument is scalar, the corresponding dummy argument shall be scalar
!*  2. following is illegal, since actual argument is scalar allocatable or pointer , but dummy argument is array.
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(k1,l1)
    integer,kind :: k1=2
    integer,len  :: l1=3
  end type
  type,extends(base) :: child(k2,l2)
    integer,kind :: k2=4
    integer,len  :: l2=2
  end type
  contains
    subroutine sub1(arg)
       type(base(2,:)),allocatable :: arg(:)
    end subroutine
    subroutine sub2(arg)
       type(base(2,:)),pointer :: arg(:,:)
    end subroutine
end module

program dummyArgDeferDiag05

  use m
  implicit none

  type(base(2,:)),allocatable          :: base1
  type(base(2,:)),pointer              :: base2=>null()
  class(child(2,:,4,:)),allocatable    :: child1
  class(child(2,:,4,:)),pointer        :: child2=>null()

  call sub1(base1)
  call sub2(base2)
  call sub3(child1)
  call sub4(child2)

  contains
     subroutine sub3(arg)
         class(child(2,:,4,:)),allocatable :: arg(:)
     end subroutine

     subroutine sub4(arg)
         class(child(2,:,4,:)),pointer :: arg(:,:)
     end subroutine

end program
