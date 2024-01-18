! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/operator/functional/genericOperatorArrayComp001.f
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
!*  DESCRIPTION                : Operator: Scalar with array components
!*                                         using intrinsic operator inside defined operator function
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
      integer(k1)   :: i(4)
      contains
         procedure, pass :: add
         generic :: operator ( + ) => add
   end type

   contains

   type(base(20,4)) function add ( a, b )
      class(base(*,4)), intent(in) :: a, b

      add%i = a%i + b%i

   end function

end module

program genericOperatorArrayComp001
   use m
   
   type (base(20,4)) :: b1
   type (base(:,4)), allocatable :: b2
   
   b1 = base(20,4) ( (/ 1,2,3,4/) )
   
   allocate ( base(20,4):: b2 )
   b2 = base(20,4) ( (/ 5,6,7,8 /) )
   
   print *, b1 + b2
   print *, b1 + b1 + b2 + b2

end program
