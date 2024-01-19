!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 5 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  defect 359800
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1)
     integer,len :: l1
     character(l1) :: c1(l1:l1)
  end type
end module

program d359800
  use m
  implicit none

  type(base(:)),allocatable :: tbase2

  tbase2=base(3)(c1=["boo"])
  print *, tbase2

end program
