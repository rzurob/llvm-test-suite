! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/generic/operator/functional/genericOperatorDeferred001.f
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
!*  DESCRIPTION                : Operator: Deferred specific binding
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

   type, abstract :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i = -999
      contains
         procedure(i), deferred, pass :: add
         procedure(j), deferred, pass :: addtype
         generic :: operator(+) => add
   end type

   type, extends(base) :: child    ! (4)
      integer(k1) :: j = -999
      contains
         procedure, pass :: add
         procedure, pass :: addtype
         generic :: operator(+) => addtype
   end type

   interface
      function i ( a, b )
         import base
         class(base(4)), allocatable :: i
         class(base(4)), intent(in) :: a
         integer, intent(in) :: b
      end function
   end interface

   interface
      function j ( a, b )
         import base, child
         type(child(4)) :: j
         class(base(4)), intent(in) :: a
         class(base(4)), intent(in) :: b
      end function
   end interface

   contains

   function add ( a, b )
      class(base(4)), allocatable :: add
      class(child(4)), intent(in) :: a
      integer, intent(in) :: b

      allocate ( add, source = child(4) ( i = a%i + b , j = a%j + b ) )

   end function

   function addtype ( a, b )
      class(base(4)) :: b
      class(child(4)), intent(in) :: a
      type(child(4)) :: addtype
      intent(in) :: b

      select type ( b )
         class is ( child(4) )
            addtype = child(4) ( i = a%i + b%i , j = a%j + b%j )
         class default
            error stop 1_4
      end select

   end function

end module

program genericOperatorDeferred001
   use m

   class(base(4)), pointer :: b1, b2

   type(child(4)) :: c1, c2

   c1 = child(4) ( 10, 20 )
   c2 = child(4) ( 20, 40 )

   allocate ( b1, source = child(4) ( 10, 20 ) )
   allocate ( b2, source = ( child(4) ( 14, 19 ) + child(4) ( 6, 21 ) ) )

   select type ( g => b1 )
      type is ( child(4) )
         if ( ( g%i /= 10 ) .or. ( g%j /= 20 ) )  error stop 2_4
      class default
         error stop 3_4
   end select

   select type ( g => b2 )
      type is ( child(4) )
         if ( ( g%i /= 20 ) .or. ( g%j /= 40 ) )  error stop 4_4
      class default
         error stop 5_4
   end select

   allocate ( b1, source = c2 + 10 )
   allocate ( b2, source = ( c2 + child(4)( 5, 10 )  ) )

   select type ( g => b1 )
      type is ( child(4) )
         if ( ( g%i /= 30 ) .or. ( g%j /= 50 ) )  error stop 6_4
      class default
         error stop 7_4
   end select

   select type ( g => b2 )
      type is ( child(4) )
         if ( ( g%i /= 25 ) .or. ( g%j /= 50 ) )  error stop 8_4
      class default
         error stop 9_4
   end select

   nullify ( b1 )

   allocate ( b1, source = c1 + c1 + c2 )

   select type ( g => b1 )
      type is ( child(4) )
         if ( ( g%i /= 40 ) .or. ( g%j /= 80 ) )  error stop 10_4
      class default
         error stop 11_4
   end select

end program
