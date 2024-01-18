!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: derived type with derived allocatable components
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable) with optional attribute
!*                                 - dummy arg: non-polymorphic with value attribute
!*                                 - function is type bound procedure
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

   type inner
      integer, allocatable :: i
      contains
         procedure :: geti
   end type

   type base
      type(inner), allocatable :: in
      contains
         procedure, nopass :: myfoo => foo
   end type

   contains

   integer function geti(a)
      class(inner), intent(in) :: a

      geti = a%i
   end function

   integer function foo ( a, b )
      type(base), value :: a
      type(base), value :: b

      print *, 'foo:'
      print *, a%in%geti(), b%in%geti()
      foo = a%in%geti() + b%in%geti()
      a%in%i = -999
      b%in%i = -999
      print *, a%in%geti(), b%in%geti()
   end function

end module

program valueTypeBoundAllocatableComponent002
   use m

   type(base) :: b1
   type(base), allocatable :: b2
   integer :: i

   b1 = base(inner(100))
   allocate ( b2, source = base(inner(200)) )

   i = b1%myfoo(b1, b2)
   print *, b1%in%i, b2%in%i, i

   i = b2%myfoo(b2,b1)
   print *, b1%in%i, b2%in%i, i

   i = foo(b1, b2)
   print *, b1%in%i, b2%in%i, i

   i = foo(b2,b1)
   print *, b1%in%i, b2%in%i, i

end program
