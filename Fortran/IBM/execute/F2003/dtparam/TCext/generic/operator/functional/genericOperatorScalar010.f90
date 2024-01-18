! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/generic/operator/functional/genericOperatorScalar010.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

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
!*  DESCRIPTION                : Operator: Scalar with polymorphic inner type within types
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

   type inner1(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         generic, private :: operator (+) => sub1
         procedure :: sub1
   end type

   type inner2(n2,k2)    ! (20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: i
      contains
         private
         generic :: operator (+) => sub2
         procedure :: sub2
   end type

   type container(k3,n3)    ! (4,20)
      integer, kind       :: k3
      integer, len        :: n3
      type(inner1(n3,k3)) :: i1
      type(inner2(n3,k3)) :: i2
      contains
         generic :: operator (-) => xsub
         procedure :: xsub
   end type

   contains

   type(container(4,20)) function xsub (a, b)
      class(container(4,*)), intent(in) :: a, b
      xsub%i1 = a%i1 + b%i2
      xsub%i2 = a%i2 + b%i1
   end function

   type(inner1(20,4)) function sub1 (a, b)
      class(inner1(*,4)), intent(in) :: a
      type(inner2(*,4)), intent(in) :: b

      sub1%i = a%i - b%i

   end function

   type(inner2(20,4)) function sub2 (a, b)
      class(inner2(*,4)), intent(in) :: a
      type(inner1(*,4)), intent(in) :: b

      sub2%i = a%i - b%i

   end function

end module

program genericOperatorScalar010
   use m

   type(container(4,20)) :: b1 = container(4,20) ( inner1(20,4)(10), inner2(20,4)(20) )
   class(container(4,:)), pointer :: b2

   if ( ( b1%i1%i /= 10 ) .or. ( b1%i2%i /= 20 ) )   error stop 1_4
   b1 = ( container(4,20) ( inner1(20,4)(10), inner2(20,4)(20) ) - container(4,20) ( inner1(20,4)(10), inner2(20,4)(5) ) )

   if ( ( b1%i1%i /= 5 ) .or. ( b1%i2%i /= 10 ) )    error stop 2_4

   allocate ( b2, source = b1 - b1 )
   if ( ( b2%i1%i /= -5 ) .or. ( b2%i2%i /= 5 ) )    error stop 3_4

   b1 = b2 - b2
   if ( ( b1%i1%i /= -10 ) .or. ( b1%i2%i /= 10 ) )  error stop 4_4

   b1 = b2 - b1
   if ( ( b1%i1%i /= -15 ) .or. ( b1%i2%i /= 15 ) )  error stop 5_4

end program
