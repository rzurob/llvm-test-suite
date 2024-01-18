!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d357831_1.f
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
    integer,kind :: k1
  end type

  type C(k2)
    integer,kind :: k2
    type(B(k2))  :: b1(2)=spread(b(k2)(),1,2)
  end type
end module

program d357831_1
  use m
  implicit none

  type(C(4)) :: c1(2:3)=spread(c(4)(),1,2)

  if(c1%k2 /= 4)                                  error stop 10_4
  if(c1(2)%b1%k1 /= 4)                            error stop 11_4
  if(c1(3)%b1%k1 /= 4)                            error stop 12_4

end program

