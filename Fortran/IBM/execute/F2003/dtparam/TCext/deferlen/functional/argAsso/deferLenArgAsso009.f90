! GB DTP extension using:
! ftcx_dtp -qck -qk -qnol /tstdev/F2003/deferlen/functional/argAsso/deferLenArgAsso009.f
! opt variations: -qnock -qnok -ql

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 05/01/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : derived type containing array character with deferred length with
!*                               argument association with pointer/target
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module n
   type base(k1)    ! (4)
       integer, kind :: k1
      character(:), pointer :: c(:)
   end type

   type(base(4)) :: b1, b2

end module

program deferLenArgAsso009
   use n

   character(1), target :: c(26) = (/ ( char(i), i=65,90 ) /)

   b1 = base(4)( null() )

   b1%c => c

   b2 = b1

   print *, b1%c, len(b1%c)
   print *, b2%c, len(b2%c)

   call foo ( b1 )

   print *, b1%c, len(b1%c)
   print *, b2%c, len(b2%c)

   contains

      subroutine foo ( c )
         type(base(4)) :: c
         print *, c%c, len(c%c)

         c%c = 'x'

      end subroutine

end program
