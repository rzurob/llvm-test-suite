!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 5 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. defect 361908
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type A(l1)
      integer,len :: l1
      character(l1),allocatable :: c1(:)
   end type
   interface assignment(=)
       module procedure assignA
   end interface
   contains
       elemental subroutine assignA(this,dt)
           class(A(*)),intent(inout) :: this
           class(A(*)),intent(in)  :: dt

           this%c1=dt%c1
       end subroutine
end module

program d361908

     use m
     type(A(3)) :: a1(2)

     a1 =[A(3)(c1=["cup","hat"]),&
          A(3)(c1=["AB","CD","EF"])]

     print *,a1(1)%c1,a1(2)%c1

end program
