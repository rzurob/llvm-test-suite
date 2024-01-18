!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 13 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Dummy Argument with deferred length
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  defect 358727
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp
     character(6)   :: name
  end type
end module

program d358727
  use m
  implicit none

  type(dtp),allocatable      :: dtp1
  allocate(dtp,source=dtp("Robert"))

end program
