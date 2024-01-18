! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/operator/resolve/genericOperatorResolve007.f
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
!*                                         vii ) contains array of different ranks and elemental
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
         generic :: operator(+) => adde
         procedure, pass, private :: add3
         procedure, pass :: add1
         procedure :: adde
         generic :: operator(+) => add3, add1
   end type

   contains

   type(base(20,4)) elemental function adde (a,b)
      class(base(*,4)), intent(in) :: a, b

      adde%i = a%i + b%i

   end function

   type(base(20,4)) function add3 (a,b)
      class(base(*,4)), intent(in) :: a, b(:,:,:)

      add3%i = a%i
      do k = 1, size(b,3)
         add3%i = add3%i + b(1,1,k)%i
      end do
      print *, 'add3'

   end function

   type(base(20,4)) function add1 (a,b)
      class(base(*,4)), intent(in) :: a, b(:)

      add1%i = a%i
      do k = 1, size(b)
         add1%i = add1%i + b(k)%i
      end do
      print *, 'add1'

   end function

end module

program genericOperatorResolve007
   use m

   type(base(20,4)) :: b0 
   type(base(:,4)) :: b1(:), b2(:,:), b3(:,:,:)
   allocatable :: b1, b2, b3

   b0 = base(20,4)(10) + base(20,4)(20)
   print *, b0

   allocate ( b1(3), source = (/ (base(20,4)(j),j=10,30,10) /) )

   b0 = base(20,4)(10) + b1
   print *, b0

   allocate ( b2(2,2), source = reshape ( source =  (/ (base(20,4)(j),j=10,40,10) /) , shape = (/2,2/) ) )
   b2 = b2 + b2
   print *, b2

   allocate ( b3(1,1,4), source = reshape ( source =  (/ (base(20,4)(j),j=10,40,10) /) , shape = (/1,1,4/) ) )
   b0 = b0 + b3
   print *, b0

end program
