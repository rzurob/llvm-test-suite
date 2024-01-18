!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d357859.f
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
!*  1. DEFECT 357859
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type B(k1)
    integer,kind :: k1=4
  end type

  type C(k2)
    integer,kind :: k2=4
    type(B(k2))  :: b1(2)=[spread(b(k2)(),1,2)]
  end type
end module

program d357859
  use m
  implicit none

end program

