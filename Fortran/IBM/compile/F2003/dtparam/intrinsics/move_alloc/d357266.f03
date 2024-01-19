!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 8 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. DEFECT 357266
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type B
     procedure(),nopass,pointer :: procptr=>null()
  end type
  contains
     subroutine sub()
     end subroutine
end module

program d357266

  use m
  implicit none
  type(B),allocatable :: b1

  allocate(b1,source=B(-2))  !<== wrong

end program

