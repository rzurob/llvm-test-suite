! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/operator/functional/genericOperatorAssociateName003.f
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
!*  DESCRIPTION                : Operator: use associate name and see if
!*                                         operator of generic tb can be used for part of the object
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
         procedure :: add
         generic :: operator ( + ) => add
   end type

   contains

      type(base(20,4)) function add ( a, b )
         class(base(*,4)), intent(in) :: a, b

         add%i = a%i + b%i

      end function

end module

program genericOperatorAssociateName003
   use m

   type(base(20,4)) :: b1(5), b3

   type(base(:,4)), allocatable :: b2(:)


   allocate ( b2(3), source = (/ ( base(20,4)(i),  i = 11, 13 ) /) )

   b1 = (/ ( base(20,4)(i),  i = 1,5 ) /)

   associate ( g => b1(1:5:2), h => b2 )
      b3 = g(1) + h(2)
      print *, b3
      associate ( i => g(2) , j => h(3) )
         b3 = i + j
         print *, b3
      end associate
   end associate

end program
