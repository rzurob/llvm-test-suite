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
!*  DESCRIPTION                : Binary Operator: Scalar to Scalar (**,//)
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

   type pow
      integer :: i
   end type

   type base
      integer :: i = -999
      contains
         procedure, pass :: powwpow
         procedure, pass :: powwbase
         procedure, pass :: powwint
         generic :: operator ( ** ) => powwpow, powwbase, powwint
   end type

   type, extends(base) :: child
      character(3) :: c = 'xxx'
      contains
         generic :: operator ( // ) => concatwchild, concatwchar
         procedure, pass :: concatwchild
         procedure, pass :: concatwchar
   end type

   contains

      type(base) function powwpow ( a, b )
         class(base), intent(in) :: a
         class(pow), intent(in)  :: b

         powwpow%i = a%i ** b%i
         print *, 'powwpow'

      end function

      type(base) function powwbase ( a, b )
         class(base), intent(in) :: a
         class(base), intent(in)  :: b

         powwbase%i = a%i ** b%i
         print *, 'powwbase'

      end function

      type(base) function powwint ( a, b )
         class(base), intent(in) :: a
         integer, intent(in)  :: b

         powwint%i = a%i ** b
         print *, 'powwint'

      end function

      type(child) function concatwchild ( a, b )
         class(child), intent(in)  :: a
         class(child), intent(in)  :: b

         concatwchild%i = a%i
         concatwchild%c = a%c(1:1) // b%c(2:3)
         print *, 'concatwchild'

      end function

      type(child) function concatwchar ( a, b )
         class(child), intent(in)   :: a
         character(3), intent(in)  :: b

         concatwchar%i = a%i
         concatwchar%c = a%c(1:1) // b(2:3)
         print *, 'concatwchar'

      end function

end module

program genericOperatorScalar003
   use m

   type(pow) :: p0 = pow(0)

   class(base), pointer  :: b1, b2, b3
   class(child), pointer :: c1, c2

   allocate ( b1, source = base  ( 2 ) )
   allocate ( b2, source = child ( 3, 'ABC' ) )

   allocate ( c1, source = child ( 4, 'abc') )
   allocate ( c2, source = child ( 5, 'def') )

   allocate ( b3, source = b1 ** b2 )
   if ( b3%i /= 8 ) error stop 1_4

   nullify ( b3 )

   allocate ( b3, source = b1 ** p0 )
   if ( b3%i /= 1 ) error stop 2_4

   nullify ( b3 )

   allocate ( b3, source = b2 ** 3 )
   if ( b3%i /= 27 ) error stop 3_4

   nullify ( b3 )

   allocate ( b3, source = ( b1 ** b2 ) ** c1 )
   if ( b3%i /= 4096 ) error stop 4_4

   allocate ( c1, source = ( c1 // c2 ) )
   if ( ( c1%i /= 4 ) .or. ( c1%c /= 'aef' ) ) error stop 5_4

   allocate ( c2, source = c1 // 'xBC' )
   if ( ( c2%i /= 4 ) .or. ( c2%c /= 'aBC' ) ) error stop 6_4

   select type ( g => b2 )
      type is ( child )
         allocate ( c2, source = ( c1 // g ) )
   end select

   if ( ( c2%i /= 4 ) .or. ( c2%c /= 'aBC' ) ) error stop 7_4

end program
