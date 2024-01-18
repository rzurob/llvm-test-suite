! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/operator/functional/genericOperatorArray009.f
! opt variations: -qnol -qnodeferredlp

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
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Operator: operators with interface block
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

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         procedure :: combine
         generic :: operator(+) => combine
   end type

   interface operator(+)
      module procedure atoacombine
   end interface

   contains

      type(base(:,4)) function combine ( a, b )
         pointer :: combine(:)
         class(base(*,4)), intent(in) :: a
         class(base(*,4)), intent(in) :: b(:)

         allocate ( combine(size(b)), source = (/ ( base(20,4) (  b(i)%i + a%i ) , i = 1, size(b)) /) )
         print *, 'stoa'
      end function

      type(base(:,4)) function atoacombine ( a, b )
         pointer :: atoacombine(:)
         class(base(*,4)), intent(in) :: a(:)
         class(base(*,4)), intent(in) :: b(:)

         allocate ( atoacombine(size(a)), source = (/ ( base(20,4) (  b(i)%i + a(i)%i ) , i = 1, size(a)) /) )
         print *, 'atoa'
      end function

end module

program genericOperatorArray009
   use m

   type(base(20,4)) :: b1(4) 
   type(base(:,4)) :: b2(:), b3(:)
   pointer :: b2, b3

   b1 = (/ base(20,4)(1), base(20,4)(2), base(20,4)(3), base(20,4)(4) /)
   allocate ( base(20,4):: b2(4), b3(4) )

   b2 = (/ base(20,4)(1), base(20,4)(2), base(20,4)(3), base(20,4)(4) /)
   b3 = (/ base(20,4)(1), base(20,4)(2), base(20,4)(3), base(20,4)(4) /)

   b1 = b1(1) + b2 + b3 + b1
   print *, b1

   b2 = b2(1) + b2 + ( b2(2) + b3 )
   print *, b2

   b3 = b3(1) + b3( (/1,4,2,3/) ) + ( b3(2) + b3( (/4,4,4,1/) ) + b3(:) + b3(4:1:-1) )
   print *, b3

end program
