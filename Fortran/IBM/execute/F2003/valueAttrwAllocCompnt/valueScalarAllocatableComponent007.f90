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
!*                                 - type: polymorphic scalar allocatable components
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable)
!*                                 - dummy arg: non-polymorphic with value attribute
!*                                 - try the pointer association status with polymorphic pointers in module and dummy-arg has target attribute
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

   type, extends(inner) :: cinner
      integer, allocatable :: j
   end type

   type base
      class(inner), allocatable :: u1
   end type

   type, extends(base) :: child
      class(inner), allocatable :: u2
   end type

   class(base), pointer :: b1

   contains

      subroutine foo( dtv )
         type(base), target, value :: dtv

         select type ( g => dtv%u1 )
            type is ( inner )
               print *, g%i
            type is ( cinner )
               print *, g%i, g%j
         end select

         print *, associated ( b1, dtv )

         dtv = base ( inner(-999) )
         b1 => dtv

         print *, associated ( b1, dtv )

         select type ( g => b1%u1 )
            type is ( inner )
               print *, g%i
            type is ( cinner )
               print *, g%i, g%j
         end select

      end subroutine

end module

program valueScalarAllocatableComponent007
   use m

   allocate ( b1, source = base (inner(100)) )
   call foo ( b1 )

   ! b1 is undefined

   allocate ( b1, source = base (cinner (1000, 2000 ) ) )

   call foo ( b1 )

   ! b1 is undefined

   allocate ( b1, source = child (inner(200), cinner(1111111,1111111) ) )
   call foo ( b1 )

   ! b1 is undefined

   allocate ( b1, source = child (cinner (3000, 4000), inner(111111) ) )

   call foo ( b1 )

   ! b1 is undefined

end program
