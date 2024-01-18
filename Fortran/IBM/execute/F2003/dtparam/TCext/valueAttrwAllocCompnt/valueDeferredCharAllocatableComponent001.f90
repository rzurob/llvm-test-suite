! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self /tstdev/F2003/valueAttrwAllocCompnt/valueDeferredCharAllocatableComponent001.f
! opt variations: -qck -qnok -ql -qdefaultpv -qreuse=none

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
!*                                 - with deferred character allocatable component
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
      integer, kind :: k1
      character(:), allocatable :: cc
   end type

   type base(k2)    ! (4)
      integer, kind                :: k2
      character(:), allocatable ::c
      type(inner(k2)), allocatable :: in
   end type

end module

type(base(4)) function foo ( a )
   use m, only: base
   type(base(4)), value :: a

   print *, 'foo:'
   print *, len(a%c), a%c
   print *, len(a%in%cc), a%in%cc

   deallocate ( a%c, a%in%cc )
   allocate (a%c, source = 'zzzzz')
   allocate (a%in%cc, source = '' )

   print *, len(a%c), a%c
   print *, len(a%in%cc), a%in%cc

   foo = a

   print *, 'end:'

end function

program valueDeferredCharAllocatableComponent001
   use m

   type(base(4)) :: b1, b2
   allocatable :: b2

   interface
      type(base(4)) function foo ( a )
         import base
         type(base(4)), value :: a
      end function
   end interface

   b1 = foo ( base(4)('', inner(4)('')) )

   print *, b1%c
   print *, b1%in%cc

   allocate ( b2, source = foo ( b1 ) )

   print *, b1%c
   print *, b1%in%cc

   print *, b2%c
   print *, b2%in%cc

   b2 = foo ( base(4)( 'ibm', inner(4)('ftn2003')) )

   print *, b2%c
   print *, b2%in%cc

   b2 = base(4)( 'ibm', inner(4)('ftn2003'))

   b1 = foo ( b2 )

   print *, b2%c
   print *, b2%in%cc

   print *, b1%c
   print *, b1%in%cc

end program
