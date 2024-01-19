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
!*                                 - with generic interface
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

   use ISO_C_BINDING

   type base
      character(3,kind=C_CHAR), allocatable :: c
   end type

   interface foo
      procedure foo1
      procedure bar
   end interface

   contains

      subroutine foo1(a)
         type(base), value :: a

         print *, 'start foo', ' ', a%c
         a%c = 'foo'
         print *, 'end foo', ' ', a%c

      end subroutine

      subroutine bar(a)
         character(3,kind=C_CHAR) :: a
         value :: a

         print *, 'start bar', ' ', a
         a = 'bar'
         print *, 'end bar', ' ', a

      end subroutine

end module

program valueAllocatableComponentGenericInterface001
   use m

   type(base) :: b1

   allocate ( b1%c, source = 'ibm' )

   print *, b1%c
   call foo(b1)
   print *, b1%c

   call foo(b1%c)
   print *, b1%c

   call foo('ftn')

end program
