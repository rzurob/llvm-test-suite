! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/operator/functional/genericOperatorPass006.f
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
!+  DESCRIPTION                : Binary Operator: with pass attribute with unlimited poly dummy arg
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
         procedure, pass(b) :: int_U_base
         generic :: operator(+) => int_U_base
   end type

   type, extends(base) :: child    ! (20,4)
   end type

   contains

   integer function int_U_base ( a, b )
      class(*), intent(in) :: a
      class(base(*,4)), intent(in) :: b

      select type ( a )
         type is ( integer )
            int_U_base = a + b%i
         type is ( real )
            int_U_base = int(a, kind(b%i) ) + b%i
         class is ( base(*,4) )
            int_U_base = a%i + b%i
      end select
   end function

end module

program genericOperatorPass006
   use m

   class(base(:,4)), pointer :: b1, b2

   allocate ( b2, source = base(20,4)( 10 + base(20,4)(20) ) )
   print *, b2%i

   b1 => b2

   print *, 10.5+b2+10+base(20,4)(20)+b1

   associate ( g => b1 + b2 + child(20,4)(30) + 40 + child(20,4)(50) )
      print *,g
      associate ( h => g + 40 )
         print *,h
      end associate
   end associate

   nullify ( b1, b2 )

   allocate ( b1, source = child(20,4)(100) )
   allocate ( b2, source = child(20,4) ( base(20,4)(10) + b1 + 100.20 + child(20,4)(20) ) )

   print *, b1%i, b2%i

end program