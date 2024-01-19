! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/operator/functional/genericOperatorPass008.f
! opt variations: -qnol -qnodeferredlp

!+  ===================================================================
!+
!+  DATE                       : 11/01/2005
!+                             :
!+
!+  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!+                             :
!+  SECONDARY FUNCTIONS TESTED : with Operator( )
!+
!+  DESCRIPTION                : Binary Operator: with pass attribute with two different types pointing to same functions
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
         procedure, pass(b) :: g => int_base1_base
         generic :: operator(+) => g
   end type

   type base1(n2,k2)    ! (20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: j  = -999
      contains
         procedure, pass :: f => int_base1_base
         generic :: operator(+) => f
   end type

   contains

   integer function int_base1_base ( a, b )
      class(base1(*,4)), intent(in) :: a
      class(base(*,4)), intent(in) :: b

      int_base1_base = a%j + b%i
   end function

end module

program genericOperatorPass008
   use m

   class(base(:,4)), allocatable :: b1
   class(base1(:,4)), pointer :: b11

   allocate ( b1, source = base(20,4) ( base1(20,4)(10) + base(20,4)(20) ) )
   allocate ( b11, source = base1(20,4)( base1(20,4)(30) + base(20,4)(40) ) )

   print *, b11+b1
   print *, base1(20,4)(b11+b1) + base(20,4)(base1(20,4)(100) +base(20,4)(200))

end program
