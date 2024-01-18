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
!*                                 - type: derived type scalar allocatable components
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

   type base
      type(inner), allocatable :: in
      character(3), allocatable :: c
   end type

   interface
      subroutine foo( dtv )
         import base
         type(base), value :: dtv
      end subroutine
   end interface

end module

program valueScalarAllocatableComponent003
   use m

   type(base) :: b1
   class(base), target, allocatable :: b2
   class(base), pointer :: b3

   b1 = base( inner( 10 ), 'abc' )

   call foo ( b1 )
   print *, b1%in%i, b1%c

   allocate ( b2, source = base( inner( 30 ), 'def' ) )

   call foo ( b2 )

   print *, b2%in%i, b2%c

   b3 => b2

   call foo ( b3 )

   print *, b2%in%i, b2%c
   print *, b3%in%i, b3%c

end program


subroutine foo( dtv )
   use m, only: base, inner
   type(base), value :: dtv

   print *, dtv%in%i, dtv%c

   dtv%in%i = -999
   dtv%c = 'xxx'

   print *, dtv%in%i, dtv%c

end subroutine
