! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/operator/functional/genericOperatorPass010.f
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
!+  DESCRIPTION                : Binary Operator: multiple spec-binding pointing to the same procedure and all being put in the same generic binding
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
         procedure, pass(b) :: base1 => base_base_base
         procedure, pass(b) :: base2 => base_base_base
         procedure, pass(b) :: base3 => base_base_base
         procedure, pass(b) :: base4 => base_base_base
         procedure, pass(b) :: base5 => base_base_base
         generic :: operator(+) => base1, base2, base3, base4, base5
   end type

   contains

   type(base(20,4)) function base_base_base ( a, b )
      class(base(*,4)), intent(in) :: a
      class(base(*,4)), intent(in) :: b

      base_base_base%i = a%i + b%i
   end function

end module

program genericOperatorPass010
   use m

   type(base(20,4)) :: b1, b2

   b1 = base(20,4)(10) + base(20,4)(20)
   b2 = b1 + base(20,4)(30)

   print *, b1, b2

   b1 = b1 + b1 + b1 + b1 + b2 + b2 + b2
   print *, b1

   b1 = b1%base1(b2)
   print *, b1

   b2 = b2%base1(b2) + b2%base2(b2) + b2%base3(b2) + b2%base4(b2) + b2%base5(b2)
   print *, b2

end program
