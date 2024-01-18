!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d359004.f
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
!*  DEFECT 359004
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(l1)
     integer,len     :: l1
     character(l1+5) :: c
  end type
end module

program d359004
use m
implicit none

type(A(:)),pointer     :: a1
allocate(A(-3)         :: a1)

if(a1%l1 /= -3)                                   stop 1
if(a1%c%len /= len(a1%c) .or. a1%c%len /= 2)      stop 2

end
