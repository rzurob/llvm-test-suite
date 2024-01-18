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
!*                                 - type: derived type scalar polymorphic allocatable components
!*                                 - actual arg: (non-)polymorphic data arg (non-pointer non-allocatable, pointer, allocatable)
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
      integer, allocatable :: j
   end type

   type base
      class(inner), allocatable :: in
      character(3), allocatable :: c
   end type

   interface
      subroutine foo( dtv )
         import base
         type(base), value :: dtv
      end subroutine
   end interface

end module

program valueScalarAllocatableComponent004
   use m

   type(base) :: b1
   class(base), target, allocatable :: b2
   class(base), pointer :: b3

   b1 = base( inner( 10 ), 'abc' )

   call foo ( b1 )
   print *, b1%in%i, b1%c

   allocate ( b2, source = base( cinner( 20, 30 ), 'def' ) )

   call foo ( b2 )

   select type ( h => b2%in )
      type is ( cinner )
         print *, h%i, h%j, b2%c
   end select

   b3 => b2

   call foo ( b3 )

   select type ( h => b2%in )
      type is ( cinner )
         print *, h%i, h%j, b2%c
   end select

   select type ( h => b3%in )
      type is ( cinner )
         print *, h%i, h%j, b3%c
   end select

end program


subroutine foo( dtv )
   use m, only: base, inner, cinner
   type(base), value :: dtv

   select type ( h => dtv%in )
      type is ( inner )
         print *, h%i, dtv%c
         h%i = -999
         dtv%c='xxx'
         print *, h%i, dtv%c
      type is ( cinner )
         print *, h%i, h%j, dtv%c
         h%i = -999
         h%j = -999
         dtv%c='xxx'
         print *, h%i, h%j, dtv%c
   end select

end subroutine
