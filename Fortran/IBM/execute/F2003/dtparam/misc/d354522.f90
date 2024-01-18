! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2008/09/09
!*
!*  DESCRIPTION                : miscellaneous (defect 354522)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type :: base
      integer :: i1(2)
   end type

   type, extends(base) :: child (k)
      integer, kind :: k = 4
      integer(4)   :: i2
   end type

   contains

    subroutine myAsgn(a)
        class(base), intent(out) :: a

        type(child) :: temp

        temp = child(1,2)

        if (.not. (same_type_as (a, child(1,2) ))) error stop 2
        if (.not. (same_type_as (a, temp ))) error stop 3

        a%i1 = temp%i1
    end subroutine
end module

   use m
   class(base), allocatable :: b3

   allocate ( child :: b3 )

   call myAsgn(b3)

   if (any(b3%i1 /= 1)) error stop 10
end program

