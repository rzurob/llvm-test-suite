!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 5 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  defect 359547
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type inner1(l1)
     integer,len  :: l1
     integer :: i1(l1)
  end type
  type inner2(l2)
     integer,len  :: l2
     type(inner1(l2+1)) :: comp1
  end type

  type outer(l3)
     integer,len  :: l3
     type(inner2(l3+1)) :: comp2
  end type

  contains
    subroutine check(arg)
      type(outer(*)),intent(in) :: arg(:)
      print *,arg
      print *,arg%l3,arg%comp2%l2,arg%comp2%comp1%l1
    end subroutine
end module

program d359547_2
  use m
  implicit none

  type(outer(1)) :: outer1(2)

  outer1=[outer(1)(comp2= &
         inner2(2)(comp1=inner1(3)(i1=[404,505,606])) ), &
          outer(1)(comp2= &
       inner2(2)(comp1=inner1(3)(i1=[-404,-505,-606])) ) ]

  print *,outer1
  call check(outer1)

end program
