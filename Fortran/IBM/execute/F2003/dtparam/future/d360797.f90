!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d360797.f
!*
!*  DATE                       : Jan. 9 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type :: dtp(l1)
     integer,len :: l1
     integer  :: i
     contains
       procedure :: modDTP
  end type
  contains
   function modDTP(this)
      class(dtp(*)),intent(in) :: this
      class(dtp(3)),allocatable :: modDTP
      allocate(modDTP,source=this)
      modDTP%i=-this%i
   end function

end module

program d360797
  use m
  implicit none

  class(dtp(3)),allocatable :: dtp1
  allocate(dtp1,source=dtp(3)(i=1))
  associate (x=>dtp1%modDTP)
     print *,"|",x%i,"|"
     select type(x)
       type is(dtp(*))
          print *,"|",x,"|"
       class default
         stop
     end select
  end associate

end program

