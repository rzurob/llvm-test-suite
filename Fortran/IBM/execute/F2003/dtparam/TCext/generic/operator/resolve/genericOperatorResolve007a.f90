! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/operator/resolve/genericOperatorResolve007a.f
! opt variations: -ql

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
!*                                         vii ) contains array of different ranks and elemental interface
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

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)      i
      contains
         procedure, pass, private :: add3
         procedure :: adde
         generic :: operator(+) => add3
   end type

   interface operator(+)
      module procedure adde
   end interface

   contains

   type(base(4)) elemental function adde (a,b)
      class(base(4)), intent(in) :: a, b

      adde%i = a%i + b%i

   end function

   type(base(4)) function add3 (a,b)
      class(base(4)), intent(in) :: a, b(:,:,:)

      add3%i = a%i
      do k = 1, size(b,3)
         add3%i = add3%i + b(1,1,k)%i
      end do
      print *, 'add3'

   end function

end module

type(base(4)) function add1 (a,b)
   use m, only: base
   class(base(4)), intent(in) :: a, b(:)

   add1%i = a%i
   do k = 1, size(b)
      add1%i = add1%i + b(k)%i
   end do
   print *, 'add1'

end function

program genericOperatorResolve007a
   use m

   type(base(4)) :: b0, b1(:), b2(:,:), b3(:,:,:)
   allocatable :: b1, b2, b3

   interface operator(+)
      type(base(4)) function add1 (a,b)
         import base
         class(base(4)), intent(in) :: a, b(:)
      end function
   end interface

   b0 = base(4)(10) + base(4)(20)
   print *, b0

   allocate ( b1(3), source = (/ (base(4)(j),j=10,30,10) /) )

   b0 = base(4)(10) + b1
   print *, b0

   allocate ( b2(2,2), source = reshape ( source =  (/ (base(4)(j),j=10,40,10) /) , shape = (/2,2/) ) )
   b2 = b2 + b2
   print *, b2

   allocate ( b3(1,1,4), source = reshape ( source =  (/ (base(4)(j),j=10,40,10) /) , shape = (/1,1,4/) ) )
   b0 = b0 + b3
   print *, b0

end program
