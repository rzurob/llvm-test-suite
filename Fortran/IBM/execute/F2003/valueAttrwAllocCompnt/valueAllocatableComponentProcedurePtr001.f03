!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: derived type with intrinsic allocatable components
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable)
!*                                 - dummy arg: non-polymorphic with value attribute
!*                                 - with procedure pointer
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
      integer, allocatable :: i1
   end type

   contains

   subroutine foo1 ( a )
      type(base), value  :: a

      print *, 'foo1:',a%i1
      a = base(100)
      print *, 'end foo1:',a%i1

   end subroutine

   subroutine foo2 ( a )
      type(base), value  :: a

      print *, 'foo2:',a%i1

      a = base(200)
      print *, 'end foo2:',a%i1

   end subroutine

end module


program valueAllocatableComponentProcedurePtr001
   use m

   abstract interface
      subroutine xxx(a )
         import base
         type(base), value :: a
      end subroutine
   end interface

   procedure(xxx), pointer :: p1
   type(base), allocatable :: b1

   allocate ( b1, source = base(99) )

   p1 => foo1

   print *, 'start', b1%i1
   call p1(b1)
   print *, 'after foo1', b1%i1

   p1 => foo2

   print *, b1%i1
   call p1(b1)
   print *, 'after foo2', b1%i1

end program