!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 27 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  DEFECT 358038
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1)
    integer,len :: l1
  end type
  type,extends(base) :: child(l2)
     integer,len     :: l2
     class(base(l2)),pointer :: poly2(:)=>null()
  end type

end module
program d358038
  use m
  implicit none

  type(child(2,2)) :: child1=child2,2)() !<== wrong syntax

end program

