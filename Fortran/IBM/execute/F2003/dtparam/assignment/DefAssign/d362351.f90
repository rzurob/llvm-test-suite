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
!* 1. defect 362351
!234567490123456749012345674901234567490123456749012345674901234567490
module mA
   type A(l1)
     integer,len :: l1
   end type
end module

module mB
   use mA
    interface assignment(=)
     subroutine assignA(this,dt)
        import A
        class(*),intent(inout) :: this
        class(A(*)),intent(in) :: dt
      end subroutine
    end interface

    type B(l2)
     integer,len :: l2
     type(A(l2)),pointer :: acomp=>null()
   end type
end module

program d362351
   use mB
   implicit none

   interface assignment(=)
     subroutine assignB(this,dt)
       import B
        class(B(1)),intent(inout) :: this
        class(B(*)),intent(in) :: dt
      end subroutine
    end interface

   type(A(1)),target :: aobj1

   type(B(1)) :: bobj1

   bobj1=B(1)(aobj1)

   print *,bobj1%acomp%l1
end program

subroutine assignA(this,ta)
  use mA
  class(*),intent(inout) :: this
  class(A(*)),intent(in)    :: ta

  print *,"in assignA"
end subroutine

subroutine assignB(this,tb)
  use mB
  class(B(1)),intent(inout) :: this
  class(B(*)),intent(in) :: tb

  print *,"in assignB"
  select type(this)
     type is(B(*))
        print *,this%l2
        allocate(A(1) :: this%acomp)
        this%acomp=tb%acomp
     class default
         stop 1
  end select
end subroutine
