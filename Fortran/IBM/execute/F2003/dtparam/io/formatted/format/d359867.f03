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
!*  defect 359867
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1)
    integer,len  :: l1
    integer      :: i1(l1:l1+2)
  end type
end module

program d359867
  use m
  implicit none

  class(base(3)),allocatable :: base1(:)
  allocate(base1(2),source=[base(3)(i1=[11,12,13]),base(3)(i1=[23,24,25] ) ])
  select type(base1)
    type is(base(*))
      print *,base1
    class default
      stop
  end select

end program

