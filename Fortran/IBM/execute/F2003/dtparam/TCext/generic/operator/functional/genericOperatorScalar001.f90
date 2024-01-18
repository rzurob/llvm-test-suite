! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=self /tstdev/F2003/generic/operator/functional/genericOperatorScalar001.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

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
!*  DESCRIPTION                : Binary Operator: Scalar to Scalar (+,-)
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
      integer(k1)   :: x = -999
      integer(k1)   :: y = -999
      contains
         procedure, pass :: mybadd
         procedure, pass :: mybsub
         generic :: operator(+) => mybadd
         generic :: operator(-) => mybsub
   end type

   contains

   function mybadd ( a, b )
      class(base(*,4)), intent(in) :: a
      type(base(*,4)), intent(in)  :: b

      type(base(20,4)) :: mybadd

      mybadd%x = a%x + b%x
      mybadd%y = a%y + b%y

      print *, 'mybadd'

   end function

   function mybsub ( a, b )
      class(base(*,4)), intent(in) :: a
      type(base(*,4)), intent(in)  :: b

      type(base(20,4)) :: mybsub

      mybsub%x = a%x - b%x
      mybsub%y = a%y - b%y

      print *, 'mybsub'

   end function

end module

program genericOperatorScalar001
   use m

   type(base(20,4)), target      :: b1
   type(base(:,4)), pointer     :: b2
   type(base(:,4)), allocatable :: b3

   b1 = base(20,4) ( 100, 200 )

   allocate ( b2, source = base(20,4) ( 101, 201 ) )
   allocate ( b3, source = base(20,4) () )

   b3 = b1 + b2
   if ( ( b3%x /= 201 ) .or. ( b3%y /= 401 ) ) error stop 1_4

   b2 => b1

   b2 = b3 + b3
   if ( ( b2%x /= 402 ) .or. ( b2%y /= 802 ) .or. ( b1%x /= 402 ) .or. ( b1%y /= 802 ) ) error stop 2_4

   allocate ( b2, source = base(20,4) ( 0, 0 ) )

   b2 = b1 - b3

   if ( ( b2%x /= 201 ) .or. ( b2%y /= 401 ) ) error stop 3_4

   b2 = b1 - b3

   if ( ( b2%x /= 201 ) .or. ( b2%y /= 401 ) ) error stop 4_4

end program
