! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/operator/functional/genericOperatorArrayComp001a2.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Scalar with array components
!*                                         using intrinsic operator inside defined operator function with allocatable components
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
      integer, kind           :: k1
      integer(k1), allocatable:: i(:)
      contains
         procedure, pass :: add
         generic :: operator ( + ) => add
   end type

   contains

   type(base(4)) function add ( a, b )
      class(base(4)), intent(in) :: a, b

      if ( ( .not. allocated (a%i)) .or. ( .not. allocated (b%i)) ) error stop 1_4
      if ( size(b%i) .lt. size (a%i) ) error stop 2_4

      allocate (add%i (size(a%i)))
      add%i = a%i + b%i(1:size(a%i))

   end function

end module

program genericOperatorArrayComp001a2
   use m

   type (base(4)) :: b1
   type (base(4)), allocatable :: b2

   b1 = base(4) ( (/ 1,2,3,4/) )

   allocate ( b2 )
   b2 = base(4) ( (/ 5, 6, 7, 8, 9, 10 /) )

   associate ( g => b2 + b2 + b2 )
      print *, g%i
   end associate

   associate ( g => b1 + b1 + b1 )
      print *, g%i
   end associate

   associate ( g => b1 + b2 + base(4)((/5,6,7,8,9,10,11/)) )
      print *, g%i
   end associate

end program
