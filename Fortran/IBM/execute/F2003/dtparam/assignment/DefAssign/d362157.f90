!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 11 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. defect 362157
!234567490123456749012345674901234567490123456749012345674901234567490
module m1
  type A(l1)
     integer,len :: l1
     character(l1) :: c3(l1)
  end type
end module

module m2
  use m1,TA=>A
end module

module m3
  use m2,XA=>TA
  type C(l3)
    integer,len :: l3
    type(XA(l3))  :: a3comp
  end type

  contains
    subroutine assignC(this,dt)
       class(C(*)),intent(inout) :: this(:)
       type(C(*)),intent(in)     :: dt

    end subroutine
end module

program d362157
   use m1; use m2; use m3,XC=>C
   implicit none

   type(XC(3)) :: cobj1

end program

