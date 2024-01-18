! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/operator/functional/genericOperatorScalar002.f
! opt variations: -ql -qreuse=self

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
!*  DESCRIPTION                : Binary Operator: Scalar to Scalar (*,/)
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

   type base(k1,k2)    ! (4,4)
      integer, kind :: k1,k2
      real(k1)      :: x = -999.9999
      real(k2)      :: y = -999.9999
      contains
         procedure, pass :: mybmul
         procedure, private :: mulwint
         procedure, pass :: mybdiv
         procedure, private :: divwint
         generic :: operator(*) => mybmul, mulwint
         generic :: operator(/) => mybdiv, divwint
   end type

   contains

   function mybmul ( a, b )
      class(base(4,4)), intent(in) :: a
      class(base(4,4)), intent(in)  :: b

      class(base(4,4)), allocatable :: mybmul
      allocate ( mybmul, source = base(4,4)() )

      mybmul%x = a%x * b%x
      mybmul%y = a%y * b%y

      print *, 'mybmul'

   end function

   function mybdiv ( a, b )
      class(base(4,4)), intent(in) :: a
      class(base(4,4)), intent(in) :: b

      class(base(4,4)), pointer :: mybdiv

      allocate ( base(4,4) :: mybdiv )

      mybdiv%x = a%x / b%x
      mybdiv%y = a%y / b%y

      print *, 'mybdiv'

   end function

   function mulwint ( a, b )
      class(base(4,4)), intent(in) :: a
      integer(4), intent(in)  :: b

      class(base(4,4)), allocatable :: mulwint
      allocate ( mulwint, source = base(4,4)() )

      mulwint%x = a%x * b
      mulwint%y = a%y * b

      print *, 'mulwint'

   end function

   function divwint ( a, b )
      class(base(4,4)), intent(in) :: a
      integer(4), intent(in) :: b

      class(base(4,4)), pointer :: divwint

      allocate ( base(4,4) :: divwint )

      divwint%x = a%x / b
      divwint%y = a%y / b

      print *, 'divwint'

   end function

end module

program genericOperatorScalar002
   use m

   class(base(4,4)), pointer            :: b1, b2
   type(base(4,4)), pointer :: b3, b4

   logical precision_r4

   allocate ( b1, b2 )

   b1%x = 1.1
   b1%y = 1.2
   b2%x = 2.2
   b2%y = 2.4

   allocate ( b3, source = ( b1 * b2 ) )
   if ( ( .not. precision_r4 ( b3%x , ( 1.1 * 2.2 ) ) ) .or. ( ( .not. precision_r4 ( b3%y , ( 1.2 * 2.4 ) ) ) ) ) error stop 1_4

   allocate ( b4, source = ( b1 * 2_4 ) )
   if ( ( .not. precision_r4 ( b4%x , ( 2.2 ) ) ) .or. ( ( .not. precision_r4 ( b4%y , ( 2.4 ) ) ) ) )             error stop 2_4

   allocate ( b1, source = ( b4 / ( 6 + ( -2_4 * 2 ) ) ) )
   if ( ( .not. precision_r4 ( b1%x , ( 1.1 ) ) ) .or. ( ( .not. precision_r4 ( b1%y , ( 1.2 ) ) ) ) )             error stop 3_4

   allocate ( b2, source = ( b2 / b1 ) )
   if ( ( .not. precision_r4 ( b2%x , ( 2.0 ) ) ) .or. ( ( .not. precision_r4 ( b2%y , ( 2.0 ) ) ) ) )             error stop 4_4

end program
