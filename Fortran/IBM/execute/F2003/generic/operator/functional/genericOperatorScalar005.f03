!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Binary Operator: Scalar to Scalar (.not., .and., .or., .eqv., .neqv.)
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
      logical :: l
      contains
         generic :: operator(.not.)  => mynot
         generic :: operator(.and.)  => myand
         generic :: operator(.or.)   => myor
         generic :: operator(.eqv.)  => myeqv
         generic :: operator(.neqv.) => myneqv

         procedure :: mynot
         procedure :: myand
         procedure :: myor
         procedure :: myeqv
         procedure :: myneqv

   end type

   type, extends(base) :: child
      integer :: i = -999
      contains
         procedure :: mynot => mycnot
         procedure :: myand => mycand
         procedure :: myor  => mycor
   end type

   contains

      class(base) function mynot ( a )
         class( base ), intent(in) :: a
         allocatable :: mynot
         allocate ( mynot, source = base ( .not. a%l ) )

      end function

      class(base) function mycnot ( a )
         class( child ), intent(in) :: a
         allocatable :: mycnot

         allocate ( mycnot, source = child ( base = base (.not. a%l ), i= -1*a%i ) )

      end function

      logical function myor ( a, b )
         class( base ), intent(in) :: a, b

         myor = ( a%l .or. b%l )

      end function

      logical function mycor ( a, b )
         class( child ), intent(in) :: a
         class( base ), intent(in) :: b

         select type (b)
            class is (child)
                mycor = ( a%base .or. b%base ) .and. ( ( a%i /= -999 ) .or. ( b%i /= -999 ) )
            class default
                mycor = .false.
        end select

      end function

      logical function myand ( a, b )
         class( base ), intent(in) :: a, b

         myand = ( a%l .and. b%l )

      end function

      logical function mycand ( a, b )
         class( child ), intent(in) :: a
         class(base), intent(in) :: b

         select type (b)
            class is (child)
                mycand = ( a%base .and. b%base ) .and. ( a%i == b%i )
            class default
                mycand = .false.
        end select

      end function

     logical function myeqv ( a, b )
        class(base), intent (in) :: a, b

        myeqv = a%l .eqv. b%l

     end function

     logical function myneqv ( a, b )
        class(base), intent (in) :: a, b

        myneqv = .not. ( a .eqv. b )

     end function

end module

program genericOperatorScalar005
   use m

   class(base), pointer :: b1, b2

   class(base), allocatable:: c1

   allocate ( b1, source = base ( .true. ) )
   allocate ( b2, source = base ( .false. ) )

   if ( b1 .eqv. b2 ) error stop 1_4
   if ( .not. ( b1 .neqv. b2 ) ) error stop 2_4
   if ( .not. ( b1 .or. b2 ) ) error stop 3_4
   if ( b1 .and. b2 ) error stop 4_4
   if ( .not. ( ( .not. b1 ) .eqv. b2 ) ) error stop 5_4

   nullify ( b2 )
   allocate ( b2, source = child ( .true. , 123 ) )

   if ( b1 .neqv. b2 ) error stop 6_4
   if ( .not. ( b1 .eqv. b2 ) ) error stop 7_4
   if ( .not. ( b1 .or. b2 ) )  error stop 8_4
   if ( .not. ( b1 .and. b2 ) ) error stop 9_4
   if ( .not. ( ( .not. b1 ) .eqv. ( .not. b2 ) ) ) error stop 10_4

   nullify ( b2 )
   allocate ( b2 , source = child ( .false.,  -123 ) )

   if ( b1 .eqv. b2 ) error stop 11_4
   if ( ( .not. b1 ) .eqv. ( .not. ( b2 ) ) ) error stop 12_4

   nullify ( b1 )
   allocate ( b1, source = child ( .true.,  123 ) )

   if (.not.( b1 .and. (.not. b2 ) ) ) error stop 13_4
   if (.not. ( b1 .or. b2 ) ) error stop 14_4
   if ( b1 .eqv. b2 ) error stop 15_4
   if ( .not. ( b1 .neqv. b2 ) ) error stop 16_4
   if ( b1 .neqv. ( .not. b2 ) ) error stop 17_4

end program
