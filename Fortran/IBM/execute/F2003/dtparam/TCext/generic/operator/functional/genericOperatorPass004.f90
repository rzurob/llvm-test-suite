! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/operator/functional/genericOperatorPass004.f
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
!*  DESCRIPTION                : Binary Operator: with pass attribute with another derived type
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
      integer(k1)   :: i = -999
      contains
         procedure, pass(b) :: int_base1_base
         generic :: operator(*) => int_base1_base
   end type

   type base1(n2,k2)    ! (20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: i = -999
      contains
         procedure, pass(b) :: int_base_base1
         generic :: operator(*) => int_base_base1
   end type

   contains

   integer function int_base1_base ( a, b )
      type(base1(*,4)), intent(in) :: a
      class(base(*,4)), intent(in) :: b

      int_base1_base = a%i * b%i
      print *, 'int_base1_base'

   end function

   integer function int_base_base1 ( a, b )
      type(base(*,4)), intent(in) :: a
      class(base1(*,4)), intent(in) :: b

      int_base_base1 = a%i * b%i
      print *, 'int_base_base1'

   end function

end module

program genericOperatorPass004
   use m

   type(base(20,4)) :: b1
   class(base1(:,4)), pointer :: b11
   integer :: i

   allocate ( b11, source = base1(20,4)( base(20,4)(10) * base1(20,4)(20) ) )
   b1 = base(20,4)( ( base(20,4)(2) * base1(20,4)(3) ) + ( base1(20,4)(4) * base(20,4)(5) ) )

   print *, b11%i, b1%i

   i =( base(20,4)(3) * base1(20,4)(4) ) * 5 * ( base1(20,4)(6) * base(20,4)(7) )
   print *,i

   i = base(20,4)( base(20,4)( base(20,4)(2) * base1(20,4)(3) * 2 ) * base1(20,4) ( base1(20,4)(4) * base(20,4)(5) * 2 ) ) * base1(20,4)( base1(20,4)( base1(20,4)(2) * base(20,4)(3) * 2 ) * base(20,4) ( base(20,4)(4) * base1(20,4)(5) * 2 ) )
   print *,i

end program
