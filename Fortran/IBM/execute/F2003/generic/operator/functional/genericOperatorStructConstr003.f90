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

   type base
      integer :: i
      contains
         procedure, pass :: mysub
         generic :: operator(-) => mysub
   end type

   type, extends(base) :: child
      integer :: j
   end type

   type, extends(child) :: gen3
      integer :: k
   end type

   contains

   class(base) function mysub(a, b)
      class(base), intent(in) :: a, b
      allocatable :: mysub
      select type ( a )
         type is ( base )
            allocate ( mysub, source = base ( a%i - b%i ) )
         type is ( child )
            if ( .not. extends_type_of ( b, a ) ) then !<- this b is type base
               allocate ( mysub, source = child ( i = ( a%i - b%i ), j = a%j ) )
            else
               select type ( b )
                  class is (child)
                     allocate ( mysub, source = child ( i = ( a%i - b%i ), j = a%j - b%j ) )
                  class default
                     error stop 1_4
               end select
            end if
         type is ( gen3 )
            select type ( b )
               type is ( base )
                  allocate ( mysub, source = gen3 ( i = a%i - b%i , j = a%j, k = a%k ) )
               type is ( child )
                  allocate ( mysub, source = gen3 ( i = a%i - b%i , j = a%j - b%j, k = a%k ) )
               type is ( gen3 )
                  allocate ( mysub, source = gen3 ( i = a%i - b%i , j = a%j - b%j, k = a%k - b%k ) )
            end select
      end select

   end function

end module

program genericOperatorStructConstr003
   use m

   class(base), pointer :: b1

   allocate ( b1, source = ( base(10) - base(20) ) )
   if ( b1%i /= -10 ) error stop 2_4

   nullify (b1)
   allocate ( b1, source = ( base(10) - child(10,20) ) )
   if ( b1%i /= 0 ) error stop 3_4

   nullify (b1)
   allocate ( b1, source = ( base(10) - gen3(100, 200, 300) ) )
   if ( b1%i /= -90 ) error stop 4_4

   nullify (b1)
   allocate ( b1, source = ( child(20,40) - base(10) ) )
   select type ( b1 )
      type is ( child )
         if ( ( b1%i /= 10 ) .or. ( b1%j /= 40 ) ) error stop 5_4
      class default
         error stop 6_4
   end select

   nullify (b1)
   allocate ( b1, source = ( child(20,40) - child(100, 200) ) )
   select type ( b1 )
      type is ( child )
         if ( ( b1%i /= -80 ) .or. ( b1%j /= -160 ) ) error stop 7_4
      class default
         error stop 8_4
   end select

   nullify (b1)
   allocate ( b1, source = ( child(20,40) - gen3(50, 100, 200) ) )
   select type ( b1 )
      type is ( child )
         if ( ( b1%i /= -30 ) .or. ( b1%j /= -60) ) error stop 9_4
      class default
         error stop 10_4
   end select

   nullify (b1)
   allocate ( b1, source = ( gen3(100, 200, 300) - base(10) ) )
   select type ( b1 )
      type is ( gen3 )
         if ( ( b1%i /= 90 ) .or. ( b1%j /= 200 ) .or. ( b1%k /= 300 )) error stop 11_4
      class default
         error stop 12_4
   end select

   nullify (b1)
   allocate ( b1, source = ( gen3(100, 200, 300) - child(-100, 200) ) )
   select type ( b1 )
      type is ( gen3 )
         if ( ( b1%i /= 200 ) .or. ( b1%j /= 0 ) .or. ( b1%k /= 300 )) error stop 13_4
      class default
         error stop 14_4
   end select

   nullify (b1)
   allocate ( b1, source = ( gen3(100, 200, 300) - gen3(50, 100, 200) ) )
   select type ( b1 )
      type is ( gen3 )
         if ( ( b1%i /= 50 ) .or. ( b1%j /= 100 ) .or. ( b1%k /= 100 )) error stop 15_4
      class default
         error stop 16_4
   end select

end
