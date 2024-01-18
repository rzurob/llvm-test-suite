! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/operator/resolve/genericOperatorResolve004.f
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
!*  DESCRIPTION                : Operator: 12.4.5 Resolving type-bound procedure references
!*                                         iv ) contains both assumed-shape array and elemental references
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


module m

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)      i
      contains
         procedure :: add
         procedure :: elementaladd
         generic :: operator (+) => add, elementaladd
   end type

   contains

   type(base(20,4)) function add (a,b)
      class(base(*,4)), intent(in) :: a, b(:)

      add%i = a%i

      do j = 1, size(b)
         add%i = add%i + b(j)%i
      end do
      print *, 'add'

   end function

   type(base(20,4)) elemental function elementaladd (a,b)
      class(base(*,4)), intent(in) :: a, b

      elementaladd%i = a%i + b%i

   end function

end module

program genericOperatorResolve004
   use m

   type(base(20,4)) :: b1, b2(4)
   type(base(:,4)), allocatable :: b3(:)
   
   allocate ( base(20,4):: b3(4) )

   b1 = base(20,4)(5) + base(20,4)(5)
   print *, b1%i

   b2 = b1 + (/ base(20,4)(2) /)
   print *, b2%i

   b3 = b2(1) + (/ base(20,4)(2), base(20,4)(2), base(20,4)(2) , base(20,4)(2), base(20,4)(2) /)
   print *, b3%i

   b2 = b2 + b3
   print *, b2%i

   associate ( g =>  reshape( source = b2, shape = (/2,2/) ) + reshape( source = b3, shape = (/2,2/) )  )
      print *, g%i
   end associate

end program
