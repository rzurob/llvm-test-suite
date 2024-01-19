! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/operator/functional/genericOperatorPass012.f
! opt variations: -qnol

!+  ===================================================================
!+
!+  DATE                       : 11/01/2005
!+                             :
!+
!+  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!+                             :
!+  SECONDARY FUNCTIONS TESTED : with Operator( )
!+
!+  DESCRIPTION                : Binary Operator: Arrays and with pass attribute
!+  KEYWORD(S)                 :
!+  TARGET(S)                  :
!+ ===================================================================
!+
!+  REVISION HISTORY
!+
!+  MM/DD/YY:  Init:  Comments:
!+ ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i = -999
      contains
         procedure, pass(b) ::  base_basearray_base
         procedure, pass ::  base_base_basearray
         procedure, pass :: base_base_base
         generic :: operator(+) => base_base_basearray, base_basearray_base, base_base_base
   end type

   contains

   type(base(20,4)) function base_basearray_base ( a, b )
      class(base(*,4)), intent(in) :: a(:)
      class(base(*,4)), intent(in) :: b

      base_basearray_base = b

      do i = 1, size(a)
         base_basearray_base%i = a(i)%i + base_basearray_base%i
      end do

   end function

   type(base(20,4)) function base_base_basearray ( a, b )
      class(base(*,4)), intent(in) :: a
      class(base(*,4)), intent(in) :: b(:)

      base_base_basearray = a

      do i = 1, size(b)
         base_base_basearray%i = base_base_basearray%i + b(i)%i
      end do

   end function

   type(base(20,4)) function base_base_base ( a, b )
      class(base(*,4)), intent(in) :: a
      class(base(*,4)), intent(in) :: b

      base_base_base%i = a%i + b%i

   end function

end module

program genericOperatorPass012
   use m

   type(base(20,4)) :: b1, b2, b3(4), b4(5)

   b1 = base(20,4)(10) + base(20,4)(20) + (/ base(20,4)(1), base(20,4)(2), base(20,4)(3), base(20,4)(4) /)

   b2 = b1 + (/ base(20,4)(1), base(20,4)(2), base(20,4)(3), base(20,4)(4) /) + (/ base(20,4)(1), base(20,4)(2), base(20,4)(3), base(20,4)(4) /) + (/ base(20,4)(1), base(20,4)(2), base(20,4)(3), base(20,4)(4) /)

   b3 = (/ base(20,4)(1), base(20,4)(2), base(20,4)(3), base(20,4)(4) /)

   b4 = b3 + b1 + b2 + b3 + b1 + base(20,4)(5)

   print *,b1
   print *,b2
   print *,b3
   print *,b4

end program
