!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: derived type with intrinsic allocatable components
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

   type base
      integer, allocatable :: i
      contains
         procedure, nopass :: myfoo => foo
   end type

   type, extends(base) :: child
      integer, allocatable :: j
   end type

   contains

   subroutine foo ( a, b )
      type(base), value :: a
      type(child), value :: b

      print *, 'foo:'
      print *, a%i, b%i, b%j
      a%i = -999
      b%i = -999
      b%j = -999
      print *, a%i, b%i, b%j

   end subroutine

end module

program valueTypeBoundAllocatableComponent001
   use m

   type(base) :: b1
   type(base), allocatable :: b2

   type(child) :: c1
   type(child), pointer :: c2

   b1 = base(100)
   allocate ( b2, source = base(200) )

   c1 = child(300,400)
   allocate ( c2, source = child(500,600) )

   call b1%myfoo(b1, c1)
   print *, b1%i, c1%i, c1%j

   call c1%myfoo(b2, c2)
   print *, b2%i, c2%i, c2%j

   call foo(b1, c1)
   print *, b1%i, c1%i, c1%j

   call foo(b2, c2)
   print *, b2%i, c2%i, c2%j

end program
