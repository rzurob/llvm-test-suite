!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 19 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. defect 362335
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type A(l1)
      integer,len  :: l1 ! l1=3

      integer :: i1(l1)=-99
      procedure(ifun),nopass,pointer :: iptr=>null()
   end type

   type B(l2)
      integer,len   :: l2 ! l2=2
      character(l2) :: c1(l2)="***"
      type(A(l2+1)) :: acomp=A(3)()
   end type
   interface assignment(=)
      module procedure assignB
   end interface

  contains
      integer function ifun(int)
        integer,intent(in) :: int(:)
        allocatable :: ifun(:)

        ifun=int
      end function

      subroutine assignB(this,dt)
         class(B(*)),intent(inout) :: this
         class(B(2)),intent(in)    :: dt

         print *,"in assignB"

         this%c1=dt%c1
         this%acomp%i1=dt%acomp%i1
         this%acomp%iptr=>dt%acomp%iptr
      end subroutine
end module

program d362335
     use m
     type(B(2)) :: b1

     b1=B(2)(["ab","cd"],A(3)([11,12,13],ifun))
     print *,b1%c1
     print *,b1%acomp%i1
     print *,associated(b1%acomp%iptr,ifun)
     print *,b1%acomp%iptr(b1%acomp%i1)

end program
