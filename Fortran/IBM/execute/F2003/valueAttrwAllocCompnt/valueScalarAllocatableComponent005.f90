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
!*                                 - type: unlimited polymorphic scalar allocatable components
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable)
!*                                 - dummy arg: non-polymorphic with value attribute
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
      real, allocatable :: j
   end type

   type base
      class(*), allocatable :: u1
      integer :: i1
   end type

   type(base), pointer :: b3

   contains

      subroutine foo( dtv )
         type(base), value :: dtv

         select type ( g => dtv%u1 )
            type is ( integer )
               print *, g
            type is ( real )
               print *, g
            type is ( inner )
               print *, g%i
            type is ( cinner )
               print *, g%i, g%j
         end select

         print *, dtv%i1

         dtv = base( -999, -999 )
         
         select type ( g => dtv%u1 )
            type is ( integer )
               print *, g
         end select

         print *, dtv%i1

      end subroutine

end module

program valueScalarAllocatableComponent005
   use m

   type(base) :: b1
   type(base), allocatable :: b2

   b1 = base( 10, 20 )
   
   call foo ( b1 )
   
   select type ( g => b1%u1 )
      type is ( integer )
         print *, g
      type is ( real )
         print *, g
      type is ( inner )
         print *, g%i
      type is ( cinner )
         print *, g%i, g%j
   end select

   print *,  b1%i1
   
   allocate ( b2, source = base ( 100.0, 200 ) )
   
   call foo ( b2 )
   
   select type ( g => b2%u1 )
      type is ( integer )
         print *, g
      type is ( real )
         print *, g
      type is ( inner )
         print *, g%i
      type is ( cinner )
         print *, g%i, g%j
   end select

   print *,  b2%i1
   
   allocate ( b3, source = base ( inner(1000), 2000 ) )
   
   call foo ( b3 )
   
   select type ( g => b3%u1 )
      type is ( integer )
         print *, g
      type is ( real )
         print *, g
      type is ( inner )
         print *, g%i
      type is ( cinner )
         print *, g%i, g%j
   end select

   print *,  b3%i1

    b1 = base( cinner(100, 200.00), 30 )
   
   call foo ( b1 )
   
   select type ( g => b1%u1 )
      type is ( integer )
         print *, g
      type is ( real )
         print *, g
      type is ( inner )
         print *, g%i
      type is ( cinner )
         print *, g%i, g%j
   end select

   print *,  b1%i1
   
end program
