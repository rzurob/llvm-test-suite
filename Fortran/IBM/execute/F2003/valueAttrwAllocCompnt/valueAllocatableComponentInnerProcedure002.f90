!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: derived type with unlimited polymorphic allocatable components
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

   type inner
      integer, allocatable :: i
   end type

   type base
      class(*), allocatable :: i1
      contains
         procedure, nopass :: foo
   end type

   contains

   subroutine foo ( a )
      type(base), value :: a

      select type ( g => a%i1 )
         type is ( integer )
            print *, g
         type is ( inner )
            print *, g%i
      end select

      a = innerfoo ( a, a )

      select type ( g => a%i1 )
         type is ( integer )
            print *, g
         type is ( inner )
            print *, g%i
      end select

      contains

         type(base) function innerfoo( ia, ib )
            type(base), value :: ia
            type(base) :: ib

            ia = base ( 1000000 )
            print *, 'innerfoo'

            select type ( g => ia%i1 )
               type is ( integer )
                  print *, g
               type is ( inner )
                  print *, g%i
            end select

            select type ( g => ib%i1 )
               type is ( integer )
                  print *, g
               type is ( inner )
                  print *, g%i
            end select

            ib = ia
            select type ( g => ia%i1 )
               type is ( integer )
                  print *, g
               type is ( inner )
                  print *, g%i
            end select

            select type ( g => ib%i1 )
               type is ( integer )
                  print *, g
               type is ( inner )
                  print *, g%i
            end select

            select type ( g => a%i1 )
               type is ( integer )
                  print *, g
               type is ( inner )
                  print *, g%i
            end select

            innerfoo = base( 100 - 200 + 400 )

         end function

   end subroutine

end module

program valueAllocatableComponentInnerProcedure002
   use m

   implicit type(base) (a-z)
   allocatable :: p2

   p1 = base(50)

   allocate ( p2, source = base(inner(200)) )

   call foo ( p1 )
   call foo ( p2 )

end program
