!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 24 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. DEFECT 356964
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type inner(l2)
     integer,len  :: l2
  end type
  type outer(l1)
     integer,len  :: l1
     type(inner(l1)),allocatable :: inner1
  end type
end module
program d356964_2
  use m
  implicit none

  type(outer(:)),allocatable :: outer1
  allocate(outer(1) :: outer1)
  allocate(outer1%inner1)
  print *,outer1%inner1%l2

end program
