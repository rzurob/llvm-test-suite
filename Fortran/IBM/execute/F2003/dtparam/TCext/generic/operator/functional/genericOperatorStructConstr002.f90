! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/operator/functional/genericOperatorStructConstr002.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Scalar to Scalar with structure constructors with class hierarchy
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
         procedure, pass, private :: mulwint
         procedure, pass, private :: mulwtype
         generic :: operator(*) => mulwint, mulwtype
   end type

  type, extends(base) :: child    ! (20,4)
      integer(k1) j
      contains
         procedure, pass, private :: mulwint => mulchildwint
         procedure, pass, private :: mulwtype => mulchildwtype
   end type

   contains

   function mulwint ( a, b )
      class(base(*,4)), intent(in) :: a
      integer, intent(in) :: b
      class(base(:,4)), allocatable :: mulwint

      allocate ( base(20,4):: mulwint )

      mulwint%i = a%i * b

   end function

   function mulwtype ( a, b )
      class(base(*,4)), intent(in) :: a, b
      class(base(:,4)), allocatable :: mulwtype
      allocate ( base(20,4):: mulwtype )
      mulwtype%i = a%i * b%i

   end function

   function mulchildwint ( a, b )
      class(child(*,4)), intent(in) :: a
      integer, intent(in) :: b
      class(base(:,4)), allocatable :: mulchildwint

      allocate ( mulchildwint , source = child(20,4) ( base = a%base * b, j = a%j * b) )

   end function

   function mulchildwtype ( a, b )
      class(child(*,4)), intent(in) :: a
      class(base(*,4)) , intent(in) :: b
      class(base(:,4)), allocatable :: mulchildwtype

      select type ( g => b )
         type is (base(*,4))
            allocate ( mulchildwtype , source = child(20,4) ( base = a%base * g, j = a%j) )
         type is (child(*,4))
            allocate ( mulchildwtype , source = child(20,4) ( base = a%base * g%base, j = a%j * g%j) )
      end select

   end function

end module


program genericOperatorStructConstr002
   use m

   type (base(20,4)) :: b1
   type(child(20,4)) :: c1

   class(base(:,4)), allocatable :: b2


   b1 = base(20,4)(10) * 4
   if ( b1%i /= 40 ) error stop 1_4

   b1 = base(20,4)(20) * base(20,4)(-20)
   if ( b1%i /= -400 ) error stop 2_4

   c1%base = base(20,4)(-2) * (-10)
   if ( c1%i /= 20 ) error stop 3_4

   allocate ( b2, source = ( child(20,4)(10,20) * 3 ) )

   select type ( b2 )
      type is (child(*,4))
         if ( ( b2%i /= 30 ) .or. ( b2%j /= 60 ) ) error stop 4_4
      class default
         error stop 5_4
   end select

   deallocate ( b2)
   allocate ( b2, source = ( child(20,4) (5,8) * base(20,4) (10) ) )

   select type ( b2 )
      type is (child(*,4))
         if ( ( b2%i /= 50 ) .or. ( b2%j /= 8 ) ) error stop 6_4
      class default
         error stop 7_4
   end select

   deallocate ( b2)
   allocate ( b2, source = ( child(20,4) (2,4) * child(20,4) (6,8) ) )

   select type ( b2 )
      type is (child(*,4))
         if ( ( b2%i /= 12 ) .or. ( b2%j /= 32 ) ) error stop 8_4
      class default
         error stop 9_4
   end select

end
