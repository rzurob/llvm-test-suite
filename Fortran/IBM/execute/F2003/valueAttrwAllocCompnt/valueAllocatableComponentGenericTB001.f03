!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: derived type with inttrinsic allocatable components
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable)
!*                                 - dummy arg: non-polymorphic with value attribute
!*                                 - with generic type bound
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

   type base
      integer, allocatable :: i
      contains
         procedure, pass :: assgn
         procedure, pass :: assgnint
         generic :: myassgn => assgn, assgnint
   end type

   contains

   subroutine assgn( a, b )
      class(base), intent(inout) :: a
      type(base), value, intent(in) :: b

      if (allocated(a%i) ) deallocate ( a%i )
      allocate ( a%i, source = b%i )

   end subroutine

   subroutine assgnint ( a, b )
      class(base), intent(inout) :: a
      integer, value, intent(in) :: b

      if (allocated(a%i) ) deallocate ( a%i )
      allocate ( a%i, source = b )

   end subroutine

end module

program valueAllocatableComponentGenericTB001
   use m

   type(base) :: b1

   b1 = base(-999)

   print *, b1%i
   call b1%myassgn(base(100))
   print *, b1%i

   call b1%myassgn(200)
   print *, b1%i

end program
