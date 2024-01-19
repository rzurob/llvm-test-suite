! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Function subprogram (Section 12.5.2.1), class(abstract type)
!*                                        returns polymorphic scalar abstract non-base type
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

   type, abstract:: base
      integer :: id
   end type

   type, extends(base) :: child
   end type

   type, extends(child), abstract :: gen3
   end type

   type, extends(gen3) :: gen4
   end type

contains

   class(gen3) function foo(a)
      pointer :: foo
      class(gen3), intent(in) :: a
      allocate(foo, source = a)
   end function

   function foo1(a) result (boo)
      class(gen3), pointer :: boo
      class(gen3), intent(in) :: a
      allocate(boo, source=a)
   end function

end module

program funcRetrn002
   use m

   class(base), pointer :: c
   class(gen3), allocatable :: g1

   allocate(g1, source = gen4(5))

   c => foo1(g1)
   if (c%id .ne. 5) error stop 1_4

   deallocate(c)

   allocate ( c, source = foo(gen4(4)))
   if (c%id .ne. 4) error stop 2_4

end program

