! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/generic/operator/functional/genericOperatorStructConstr003.f
! opt variations: -ql -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Scalar to Scalar with structure constructors
!*                                         with class hierarchy but only generic defined in parent types
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
      integer(k1)   :: i
      contains
         procedure, pass :: mysub
         generic :: operator(-) => mysub
   end type

   type, extends(base) :: child    ! (4)
      integer(k1) :: j
   end type

   type, extends(child) :: gen3    ! (4)
      integer(k1) :: k
   end type

   contains

   class(base(4)) function mysub(a, b)
      class(base(4)), intent(in) :: a, b
      allocatable :: mysub
      select type ( a )
         type is ( base(4) )
            allocate ( mysub, source = base(4) ( a%i - b%i ) )
         type is ( child(4) )
            if ( .not. extends_type_of ( b, a ) ) then !<- this b is type base(4)
               allocate ( mysub, source = child(4) ( i = ( a%i - b%i ), j = a%j ) )
            else
               select type ( b )
                  class is (child(4))
                     allocate ( mysub, source = child(4) ( i = ( a%i - b%i ), j = a%j - b%j ) )
                  class default
                     error stop 1_4
               end select
            end if
         type is ( gen3(4) )
            select type ( b )
               type is ( base(4) )
                  allocate ( mysub, source = gen3(4) ( i = a%i - b%i , j = a%j, k = a%k ) )
               type is ( child(4) )
                  allocate ( mysub, source = gen3(4) ( i = a%i - b%i , j = a%j - b%j, k = a%k ) )
               type is ( gen3(4) )
                  allocate ( mysub, source = gen3(4) ( i = a%i - b%i , j = a%j - b%j, k = a%k - b%k ) )
            end select
      end select

   end function

end module

program genericOperatorStructConstr003
   use m

   class(base(4)), pointer :: b1

   allocate ( b1, source = ( base(4)(10) - base(4)(20) ) )
   if ( b1%i /= -10 ) error stop 2_4

   nullify (b1)
   allocate ( b1, source = ( base(4)(10) - child(4)(10,20) ) )
   if ( b1%i /= 0 ) error stop 3_4

   nullify (b1)
   allocate ( b1, source = ( base(4)(10) - gen3(4)(100, 200, 300) ) )
   if ( b1%i /= -90 ) error stop 4_4

   nullify (b1)
   allocate ( b1, source = ( child(4)(20,40) - base(4)(10) ) )
   select type ( b1 )
      type is ( child(4) )
         if ( ( b1%i /= 10 ) .or. ( b1%j /= 40 ) ) error stop 5_4
      class default
         error stop 6_4
   end select

   nullify (b1)
   allocate ( b1, source = ( child(4)(20,40) - child(4)(100, 200) ) )
   select type ( b1 )
      type is ( child(4) )
         if ( ( b1%i /= -80 ) .or. ( b1%j /= -160 ) ) error stop 7_4
      class default
         error stop 8_4
   end select

   nullify (b1)
   allocate ( b1, source = ( child(4)(20,40) - gen3(4)(50, 100, 200) ) )
   select type ( b1 )
      type is ( child(4) )
         if ( ( b1%i /= -30 ) .or. ( b1%j /= -60) ) error stop 9_4
      class default
         error stop 10_4
   end select

   nullify (b1)
   allocate ( b1, source = ( gen3(4)(100, 200, 300) - base(4)(10) ) )
   select type ( b1 )
      type is ( gen3(4) )
         if ( ( b1%i /= 90 ) .or. ( b1%j /= 200 ) .or. ( b1%k /= 300 )) error stop 11_4
      class default
         error stop 12_4
   end select

   nullify (b1)
   allocate ( b1, source = ( gen3(4)(100, 200, 300) - child(4)(-100, 200) ) )
   select type ( b1 )
      type is ( gen3(4) )
         if ( ( b1%i /= 200 ) .or. ( b1%j /= 0 ) .or. ( b1%k /= 300 )) error stop 13_4
      class default
         error stop 14_4
   end select

   nullify (b1)
   allocate ( b1, source = ( gen3(4)(100, 200, 300) - gen3(4)(50, 100, 200) ) )
   select type ( b1 )
      type is ( gen3(4) )
         if ( ( b1%i /= 50 ) .or. ( b1%j /= 100 ) .or. ( b1%k /= 100 )) error stop 15_4
      class default
         error stop 16_4
   end select

end
