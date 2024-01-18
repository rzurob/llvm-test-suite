!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 13 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  DEFECT 358876
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type inner(l2)
     integer,len  :: l2
     character(l2),allocatable :: c2(:)
  end type
  type outer(l1)
     integer,len  :: l1
     character(l1),allocatable   :: c1
     type(inner(:)),allocatable  :: inner1
  end type
end module

program d358876

use m
implicit none

type(outer(:)),allocatable :: outer1

allocate(outer1,source=&
  outer(3)("xlf",inner(5)(["test1","test2"]) ))

  if(outer1%c1 /= "xlf")                           stop 10
  if(any(outer1%inner1%c2 /= ["test1","test2"]))   stop 11

end program
