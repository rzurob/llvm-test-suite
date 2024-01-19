! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/operator/resolve/genericOperatorResolve006.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: 12.4.5 Resolving type-bound procedure references
!*                                         vi ) contains both assumed-size array of different ranks
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
         procedure :: add1
         procedure :: add2
         generic :: operator (+) => add1, add2
   end type

   contains

   type(base(20,4)) function add1 (a,b)
      class(base(*,4)), intent(in) :: a, b(*)

      add1%i = a%i

      do j = 1, 4
         add1%i = add1%i + b(j)%i
      end do

      print *, 'add1'

   end function

   type(base(20,4)) function add2 (a,b)
      class(base(*,4)), intent(in) :: a, b(2,*)

      add2%i = a%i

      do j = 1, 2
         do k = 1, 2
            add2%i = add2%i + b(j,k)%i
         end do
      end do

      print *, 'add2'

   end function

end module

program genericOperatorResolve006
   use m

   type(base(20,4)) :: b
   type(base(20,4)) :: b1(4), b2(4), b3(2,2), b4(2,2)

   b1 = (/ ( base(20,4)(j), j = 1, 4 ) /)
   b2 = (/ ( base(20,4)(j+4), j = 1, 4 ) /)

   b3 = reshape ( source = b1, shape = (/2,2/) )
   b4 = reshape ( source = b2, shape = (/2,2/) )

   b = base(20,4)(10) + b1
   print *, b%i

   b = base(20,4)(10) + b2
   print *, b%i

   b = base(20,4)(10) + b3
   print *, b%i

   b = base(20,4)(10) + b4
   print *, b%i

end program
