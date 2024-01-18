!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d359972_2.f
!*
!*  DATE                       : Dec. 10 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  defect 359972
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1)
     integer,len   :: l1
     character(l1) :: c(3)
  end type
end module

program d359972_2

  use m
  implicit none

  type(base(:)),allocatable :: base1

  allocate(base(3) :: base1)

  base1%c=["xlf","ibm","xlc"]
  print *,"|",base1,"|"
  print *,"|",base1%c,"|"

end program
