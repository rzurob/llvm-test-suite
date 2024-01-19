! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/operator/functional/genericOperatorScalar005.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

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

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      logical(k1)   :: l
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

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: i = -999
      contains
         procedure :: mynot => mycnot
         procedure :: myand => mycand
         procedure :: myor  => mycor
   end type

   contains

      class(base(:,4)) function mynot ( a )
         class( base(*,4) ), intent(in) :: a
         allocatable :: mynot
         allocate ( mynot, source = base(20,4) ( .not. a%l ) )

      end function

      class(base(:,4)) function mycnot ( a )
         class( child(*,4) ), intent(in) :: a
         allocatable :: mycnot

         allocate ( mycnot, source = child(20,4) ( base = base(20,4) (.not. a%l ), i= -1*a%i ) )

      end function

      logical function myor ( a, b )
         class( base(*,4) ), intent(in) :: a, b

         myor = ( a%l .or. b%l )

      end function

      logical function mycor ( a, b )
         class( child(*,4) ), intent(in) :: a
         class( base(*,4) ), intent(in) :: b

         select type (b)
            class is (child(*,4))
                mycor = ( a%base .or. b%base ) .and. ( ( a%i /= -999 ) .or. ( b%i /= -999 ) )
            class default
                mycor = .false.
        end select

      end function

      logical function myand ( a, b )
         class( base(*,4) ), intent(in) :: a, b

         myand = ( a%l .and. b%l )

      end function

      logical function mycand ( a, b )
         class( child(*,4) ), intent(in) :: a
         class(base(*,4)), intent(in) :: b

         select type (b)
            class is (child(*,4))
                mycand = ( a%base .and. b%base ) .and. ( a%i == b%i )
            class default
                mycand = .false.
        end select

      end function

     logical function myeqv ( a, b )
        class(base(*,4)), intent (in) :: a, b

        myeqv = a%l .eqv. b%l

     end function

     logical function myneqv ( a, b )
        class(base(*,4)), intent (in) :: a, b

        myneqv = .not. ( a .eqv. b )

     end function

end module

program genericOperatorScalar005
   use m

   class(base(:,4)), pointer :: b1, b2

   class(base(:,4)), allocatable:: c1

   allocate ( b1, source = base(20,4) ( .true. ) )
   allocate ( b2, source = base(20,4) ( .false. ) )

   if ( b1 .eqv. b2 ) error stop 1_4
   if ( .not. ( b1 .neqv. b2 ) ) error stop 2_4
   if ( .not. ( b1 .or. b2 ) ) error stop 3_4
   if ( b1 .and. b2 ) error stop 4_4
   if ( .not. ( ( .not. b1 ) .eqv. b2 ) ) error stop 5_4

   nullify ( b2 )
   allocate ( b2, source = child(20,4) ( .true. , 123 ) )

   if ( b1 .neqv. b2 ) error stop 6_4
   if ( .not. ( b1 .eqv. b2 ) ) error stop 7_4
   if ( .not. ( b1 .or. b2 ) )  error stop 8_4
   if ( .not. ( b1 .and. b2 ) ) error stop 9_4
   if ( .not. ( ( .not. b1 ) .eqv. ( .not. b2 ) ) ) error stop 10_4

   nullify ( b2 )
   allocate ( b2 , source = child(20,4) ( .false.,  -123 ) )

   if ( b1 .eqv. b2 ) error stop 11_4
   if ( ( .not. b1 ) .eqv. ( .not. ( b2 ) ) ) error stop 12_4

   nullify ( b1 )
   allocate ( b1, source = child(20,4) ( .true.,  123 ) )

   if (.not.( b1 .and. (.not. b2 ) ) ) error stop 13_4
   if (.not. ( b1 .or. b2 ) ) error stop 14_4
   if ( b1 .eqv. b2 ) error stop 15_4
   if ( .not. ( b1 .neqv. b2 ) ) error stop 16_4
   if ( b1 .neqv. ( .not. b2 ) ) error stop 17_4

end program
