!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 12 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. defect 362249
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type A(l1)
      integer,len :: l1
   end type
  type B(l2)
    integer,len   :: l2

    type(A(l2+1))       :: a1comp(l2)
    type(B(l2)),pointer :: next=>null()
  end type

  interface assignment(=)
     module procedure  assignB
  end interface

  contains
     recursive subroutine assignB(this,dt)
        class(B(3)),intent(inout) :: this
        type(B(*)),intent(in)     :: dt

        print *,"in assignB"

        this%a1comp=dt%a1comp

        if(associated(this%next)) nullify(this%next)

        print *,associated(dt%next)

        if(associated(dt%next))  then
          allocate(B(dt%l2) :: this%next)
          this%next = dt%next !recursive call
        end if

     end subroutine

end module

program d362249
    use m
    implicit none

    type(A(4)),target :: aobj1=A(4)()

    type(B(3)),pointer :: bobj1

    type(B(:)),pointer :: bobj2=>null()

    allocate(bobj1)

    allocate(B(3) :: bobj2)

    allocate(bobj1%next,source=B(3)(aobj1,null()) )
    allocate(bobj1%next%next,source=B(3)(aobj1,null()))
    allocate(bobj1%next%next%next,source=B(3)(aobj1,null()))

    ! invoke assignB
    bobj2=bobj1

end program

