! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Function subprogram (Section 12.5.2.1), class(abstract type)
!*                                        returns polymorphic scalar abstract type
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type, abstract :: base
      integer :: id
   end type

   type, extends(base) :: child
   end type

contains

   class(base) function foo(a)
      pointer :: foo
      class(base), intent(in) :: a
      allocate(foo, source=a )
   end function

   function foo1(a) result (boo)
      class(base), pointer :: boo
      class(base), intent(in) :: a
      allocate(boo, source=a)
   end function

end module

program funcRetrn001
   use m

   class(base), pointer :: c
   class(base), allocatable :: b

   allocate (b, source = child(4))

   c => foo(b)
   if (c%id .ne. 4) error stop 1_4
   deallocate (c)

   allocate ( c, source = foo1(child(5)) )
   if (c%id .ne. 5) error stop 2_4

end program

