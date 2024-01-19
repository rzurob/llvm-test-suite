!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 18 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  DEFECT 359005
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(l1)
     integer,len     :: l1
     character(l1+5) :: c
  end type
end module

program d359005
use m
implicit none

type(A(:)),pointer       :: a1
allocate(A(-100)         :: a1)

if(a1%l1 /= -100)        stop

end
