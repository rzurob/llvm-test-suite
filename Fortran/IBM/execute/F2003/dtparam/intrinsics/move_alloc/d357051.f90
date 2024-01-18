!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 3 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. DEFECT 357051
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(l)
      integer,len :: l
      character(l),pointer  :: c2=>null()
   end type
end module

program d357051
  use m
  implicit none
  type(dtp(:)),allocatable  :: from1

  allocate(dtp(5) :: from1)
  allocate(from1%c2,source="IBM")

  if(from1%l /= 5)                               error stop 10_4
  if(from1%c2 /= "IBM")                          error stop 11_4

end program

