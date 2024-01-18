! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/valueAttrwAllocCompnt/valueScalarAllocatableComponent003.f
! opt variations: -qck -qnok -ql -qdefaultpv -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
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

   type inner(k1)    ! (4)
      integer, kind            :: k1
      integer(k1), allocatable :: i
   end type

   type base(k2,n1)    ! (4,3)
      integer, kind                :: k2
      integer, len                 :: n1
      type(inner(k2)), allocatable :: in
      character(n1), allocatable   :: c
   end type

   interface
      subroutine foo( dtv )
         import base
         type(base(4,3)), value :: dtv
      end subroutine
   end interface

end module

program valueScalarAllocatableComponent003
   use m

   type(base(4,3)) :: b1
   class(base(4,:)), target, allocatable :: b2
   class(base(4,:)), pointer :: b3

   b1 = base(4,3)( inner(4)( 10 ), 'abc' )

   call foo ( b1 )
   print *, b1%in%i, b1%c

   allocate ( b2, source = base(4,3)( inner(4)( 30 ), 'def' ) )

   call foo ( b2 )

   print *, b2%in%i, b2%c

   b3 => b2

   call foo ( b3 )

   print *, b2%in%i, b2%c
   print *, b3%in%i, b3%c

end program


subroutine foo( dtv )
   use m, only: base, inner
   type(base(4,3)), value :: dtv

   print *, dtv%in%i, dtv%c

   dtv%in%i = -999
   dtv%c = 'xxx'

   print *, dtv%in%i, dtv%c

end subroutine
