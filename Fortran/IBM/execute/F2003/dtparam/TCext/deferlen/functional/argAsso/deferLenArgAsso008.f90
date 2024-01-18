! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/F2003/deferlen/functional/argAsso/deferLenArgAsso008.f
! opt variations: -qnock -qnok -qnol

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
!*                               argument association
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
   type base(k1,n1)    ! (4,20)
       integer, kind :: k1
       integer, len  :: n1
      character(:), allocatable :: c(:)
   end type

   type(base(4,20)) :: b1

end module

program deferLenArgAsso008
   use n

   b1 = base(4,20)( (/ ( char(i), i=65,90 ) /) )

   call foo ( b1 )

   print *, b1%c, len(b1%c)

   contains

      subroutine foo ( c )
         type(base(4,*)) :: c
         print *, c%c, len(c%c)

      end subroutine

end program
