!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d357831_2.f
!*
!*  DATE                       : Oct. 22 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. DEFECT 357831
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type B(k1)
    integer,kind :: k1=4
  end type

  type C
    type(B(4))  :: b1(2)=spread(b(4)(),1,2)
  end type
end module

program d357831_2
  use m
  implicit none

end program

