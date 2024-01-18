!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d362165.f
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
!* 1. defect 362165
!234567490123456749012345674901234567490123456749012345674901234567490
module m1
  type A(l1)
     integer,len :: l1
     character(l1) :: c3="A"
  end type
end module

module m2
  use m1,XA=>A

  interface assignment(=)
    module procedure assignA
  end interface

  contains
    subroutine assignA(this,dt)
       class(XA(*)),intent(inout) :: this
       type(XA(*)),intent(in)     :: dt
       print *,"in assignA"

       this%c3=dt%c3
    end subroutine
end module

program d362165
   use m2
   implicit none

   type(XA(1)) :: a1

   ! call assignA
   a1=XA(1)()

   print *,a1%c3
end program
