! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/generic/operator/functional/genericOperatorScalar012.f
! opt variations: -ql -qreuse=none

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
!*  DESCRIPTION                : Operator: Scalar function return with polymorphic pointer attribute
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
         generic :: operator (-) => sub1, sub2
         procedure, private :: sub1
         procedure, private :: sub2
   end type

   contains

   function sub1 (a, b)
      class(base(4)), intent(in) :: a, b

      class(base(4)), pointer :: sub1
      allocate ( sub1, source = base(4) ( a%i - b%i ) )

   end function sub1

   function sub2 (a, b)
      class(base(4)), intent(in) :: a
      integer, intent(in) :: b

      class(base(4)), pointer :: sub2
      allocate ( sub2, source = base(4) ( a%i - b ) )

   end function sub2

end module

module n
   use m, only: base

   type, extends(base) :: child    ! (4)
     integer(k1) :: j
      contains
         procedure, private :: sub1 => subc1
         procedure, private :: sub2 => subc2
   end type

   contains

   function subc1 (a, b)
      class(child(4)), intent(in) :: a
      class(base(4)), intent(in)  :: b

      class(base(4)), pointer :: subc1

      select type ( b )
         type is ( base(4) )
            allocate ( subc1, source = child(4) ( a%i - b%i, j = a%j ) )
         type is ( child(4) )
            allocate ( subc1, source = child(4) ( a%i - b%i, j = a%j - b%j ) )
      end select

   end function subc1

   function subc2 (a, b)
      class(child(4)), intent(in) :: a
      integer, intent(in) :: b

      class(base(4)), pointer :: subc2
      allocate ( subc2, source = child(4) ( a%i - b, j = a%j - b ) )

   end function subc2

end module

program genericOperatorScalar012
   use n

   class(base(4)), pointer  :: b1
   class(base(4)), pointer :: b2

   b1 => base(4) (100) - base(4) (90)
   b2 => child(4) (30,40) - b1

   if ( b1%i /= 10 ) error stop 1_4

   select type ( b2 )
      type is ( child(4) )
         if ( ( b2%i /= 20 ) .or. ( b2%j /= 40 ) )  error stop 2_4
      class default
         error stop 3_4
   end select

   b1 => b1 - b1
   if ( b1%i /= 0 ) error stop 3_4

   b1 => base(4)(10) - (-100)
   if ( b1%i /= 110 ) error stop 4_4

   b1 => child(4) ( 200, 400 ) - 150

   select type ( b1 )
      type is ( child(4) )
         if ( ( b1%i /= 50 ) .or. ( b1%j /= 250 ) )  error stop 5_4
      class default
         error stop 6_4
   end select

   b2 => child(4) (30,40) - b1

   select type ( b2 )
      type is ( child(4) )
         if ( ( b2%i /= -20 ) .or. ( b2%j /= -210 ) )  error stop 7_4
      class default
         error stop 8_4
   end select

end program
