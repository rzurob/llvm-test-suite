! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  C503 The TYPE(derived-type-spec) shall not specify an abstract type
!*                               VALUE attribute (only want to make sure it works with abstract type, will not go into detail testing on optional attribute)
!*                               a) normal subroutine or function
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m

   type, abstract :: base
      integer :: id
   end type

   type, extends(base) :: child
   end type

contains

    integer function fooChild (a)
        type(child), value :: a
        allocatable fooChild

        fooChild = a%id + 1

        a%id = a%id * 10
    end function

   integer function foo(a)
      class(base) :: a

      select type (a)
        class is (child)
            foo = fooChild (a)

        class default
            stop 10
      end select
   end function

end module

program dummy012
   use m

   class(base), allocatable :: b1
   class(child), allocatable :: c1

   allocate (b1, source = child(4))
   allocate (c1, source = child(3))

   if ((foo(b1) .ne. 5) .or. (b1%id .ne. 4) ) error stop 1_4
   if ((foo(c1) .ne. 4) .or. (c1%id .ne. 3) ) error stop 2_4

end program