! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/operator/functional/genericOperatorScalar015.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: different type in the second argument in the same type
!*                                         such that the 2nd arg types are parent and extended types
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
      integer(k1)   :: i
      contains
         procedure :: withbase
         procedure :: withchild
         generic :: operator(-) => withbase, withchild
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j
   end type

   contains

      class(base(:,4)) function withbase ( a, b )
         allocatable :: withbase
         class(base(*,4)), intent(in) :: a
         type(base(*,4)) , intent(in) :: b
         allocate ( withbase , source = base(20,4) ( a%i - b%i ) )

      end function

      class(base(:,4)) function withchild ( a, b )
         allocatable :: withchild
         class(base(*,4)), intent(in) :: a
         type(child(*,4)) , intent(in) :: b

         allocate ( withchild , source = child(20,4) ( base = a - b%base, j = -b%j ) )

      end function

end module

program genericOperatorScalar015
   use m

   type(base(20,4)) :: b1 = base(20,4)(100)
   type(base(20,4)) :: b2
   class(base(:,4)), pointer :: b3

   type(child(20,4)) :: c1 = child(20,4) ( 300, 400 )

   allocate ( b3 , source = base(20,4) (200) )

   b2 = b1 - b3
   if ( b2%i /= -100 ) error stop 1_4

   b2 = b3 - b1
   if ( b2%i /= 100 )  error stop 2_4

   associate ( g => b1 - c1 )
      select type ( g )
         type is ( child(*,4) )
            if ( ( g%i /= -200 ) .or. ( g%j /= -400 ) ) error stop 3_4
         class default
            error stop 4_4
      end select
   end associate

   select type ( g => c1 - b1 )
      type is ( base(*,4) )
         if ( g%i /= 200 ) error stop 5_4
      class default
         error stop 6_4
   end select

   select type ( g => c1 - c1 )
      type is ( child(*,4) )
         if ( ( g%i /= 0 ) .or. (g%j /= -400 ) ) error stop 7_4
      class default
         error stop 8_4
   end select

end program
