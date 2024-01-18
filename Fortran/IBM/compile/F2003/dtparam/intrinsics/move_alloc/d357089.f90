!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d357089.f
!*
!*  DATE                       : Oct. 6 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. DEFECT 357089
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
      integer,len :: l1
      character(3),pointer :: ch1=>null()
   end type
   type,extends(base) :: child
   end type
end module

program d357089
  use m
  implicit none

  class(base(3)),target,allocatable :: from1

  allocate(child(3,4) :: from1) !<= wrong

end program

