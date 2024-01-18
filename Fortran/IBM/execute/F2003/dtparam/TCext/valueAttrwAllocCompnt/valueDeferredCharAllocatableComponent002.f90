! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/valueAttrwAllocCompnt/valueDeferredCharAllocatableComponent002.f
! opt variations: -qck -qnok -ql

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
!*                                 - with deferred character allocatable array component
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

   type base(k1)    ! (4)
      integer, kind :: k1
      character(:), allocatable ::c(:)
   end type

end module

type(base(4)) function foo ( a )
   use m, only: base
   type(base(4)), value :: a

   print *, 'foo:'
   print *, len(a%c), a%c

   deallocate ( a%c )
   allocate (a%c(3), source = (/ 'xxx', 'xxx', 'xxx' /) )

   print *, len(a%c), a%c

   foo = a

   print *, 'end:'

end function

program valueDeferredCharAllocatableComponent002
   use m

   type(base(4)) :: b1, b2
   allocatable :: b2

   interface
      type(base(4)) function foo ( a )
         import base
         type(base(4)), value :: a
      end function
   end interface

   b1 = foo ( base(4)((/''/)) )
   print *, 'b1:', len(b1%c), size(b1%c), b1%c

   allocate ( b2, source = foo ( b1 ) )
   print *, 'b2:', len(b2%c), size(b2%c), b2%c

   b2 = base(4)( (/ 'abcdef', 'ghijkl', 'mnopqr' /) )

   b1 = foo ( b2 )
   print *, 'b1:', len(b1%c), size(b1%c), b1%c

   b2 = foo ( b2 )
   print *, 'b2:', len(b2%c), size(b2%c), b2%c

end program
