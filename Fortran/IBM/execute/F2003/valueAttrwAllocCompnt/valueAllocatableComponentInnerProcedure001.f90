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
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable)
!*                                 - dummy arg: non-polymorphic with value attribute
!*                                 - with inner procedure
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
      contains
         procedure, nopass :: foo
   end type

   contains

   subroutine foo ( a )
      type(base), value :: a

      print *, a%i1
      a = innerfoo ( a, a )
      print *, a%i1

      contains

         type(base) function innerfoo( ia, ib )
            type(base), value :: ia
            type(base) :: ib

            ia = base ( 1000000 )
            print *, 'innerfoo', ia%i1, ib%i1
            ib = ia
            print *, ia%i1, ib%i1

            print *, a%i1
 
            innerfoo = base( ia%i1 + ib%i1 )

         end function

   end subroutine

end module

program valueAllocatableComponentInnerProcedure001
   use m

   implicit type(base) (a-z)
   allocatable :: p2

   p1 = base(50)

   allocate ( p2, source = base(200) )

   call foo ( p1 )

   call foo ( p2 )

end program
