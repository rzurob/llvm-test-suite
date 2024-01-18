!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d358633.f
!*
!*  DATE                       : Nov. 10 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  DEFECT 358633
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A
     integer   :: i1(2)
  end type
end module

program d358633

use m
implicit none

type(A) ::  a1=A(null())

end program
