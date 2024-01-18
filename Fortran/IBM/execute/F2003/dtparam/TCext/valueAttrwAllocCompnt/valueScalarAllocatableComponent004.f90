! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qdeferredlp -qreuse=self -qreuse=base /tstdev/F2003/valueAttrwAllocCompnt/valueScalarAllocatableComponent004.f
! opt variations: -qck -qnok -ql -qdefaultpv -qnodeferredlp -qreuse=none

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

   type inner(k1)    ! (4)
      integer, kind            :: k1
      integer(k1), allocatable :: i
   end type

   type, extends(inner) :: cinner    ! (4)
      integer(k1), allocatable :: j
   end type

   type base(k2,n1)    ! (4,3)
      integer, kind                 :: k2
      integer, len                  :: n1
      class(inner(k2)), allocatable :: in
      character(n1), allocatable    :: c
   end type

   interface
      subroutine foo( dtv )
         import base
         type(base(4,3)), value :: dtv
      end subroutine
   end interface

end module

program valueScalarAllocatableComponent004
   use m

   type(base(4,3)) :: b1
   class(base(4,:)), target, allocatable :: b2
   class(base(4,:)), pointer :: b3

   b1 = base(4,3)( inner(4)( 10 ), 'abc' )

   call foo ( b1 )
   print *, b1%in%i, b1%c

   allocate ( b2, source = base(4,3)( cinner(4)( 20, 30 ), 'def' ) )

   call foo ( b2 )

   select type ( h => b2%in )
      type is ( cinner(4) )
         print *, h%i, h%j, b2%c
   end select

   b3 => b2

   call foo ( b3 )

   select type ( h => b2%in )
      type is ( cinner(4) )
         print *, h%i, h%j, b2%c
   end select

   select type ( h => b3%in )
      type is ( cinner(4) )
         print *, h%i, h%j, b3%c
   end select

end program


subroutine foo( dtv )
   use m, only: base, inner, cinner
   type(base(4,3)), value :: dtv

   select type ( h => dtv%in )
      type is ( inner(4) )
         print *, h%i, dtv%c
         h%i = -999
         dtv%c='xxx'
         print *, h%i, dtv%c
      type is ( cinner(4) )
         print *, h%i, h%j, dtv%c
         h%i = -999
         h%j = -999
         dtv%c='xxx'
         print *, h%i, h%j, dtv%c
   end select

end subroutine
