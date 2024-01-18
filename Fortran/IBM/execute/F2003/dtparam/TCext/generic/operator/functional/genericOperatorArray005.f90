! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/operator/functional/genericOperatorArray005.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Binary Operator: Scalar to Array (.not., .and., .or., .eqv., .neqv.)
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

   contains

      type(base(20,4)) elemental function mynot ( a )
         class( base(*,4) ), intent(in) :: a

         mynot%l = .not. a%l

      end function

      logical function myor ( a, b )
         class( base(*,4) ), intent(in) :: a, b(:)

         do i = 1, size(b)
            myor = myor .or. ( a%l .or. b(i)%l )
         end do

      end function

      logical function myand ( a, b )
         class( base(*,4) ), intent(in) :: a, b(:)

         myand = .true.
         do i=1, size(b)
            myand = myand .and. ( a%l .and. b(i)%l )
         end do

      end function

     logical function myeqv ( a, b )
        class(base(*,4)), intent (in) :: a, b(:)

        myeqv = .true.
        do i = 1, size(b)
           myeqv = myeqv .and. ( a%l .eqv. b(i)%l )
        end do

     end function

     logical function myneqv ( a, b )
        class(base(*,4)), intent (in) :: a, b(:)

        myneqv = .not. ( a .eqv. b )

     end function

end module

program genericOperatorArray005
   use m, only: base

   type(base(20,4)) :: b1, b2(4)

   b1 = base(20,4)(.true.)
   b2 = (/ base(20,4)(.true.), base(20,4)(.true.) , base(20,4)(.true.), base(20,4)(.true.) /)

   b1 = .not. b1
   b2 = .not. b2

   print *, b1
   print *, b2

   print *, ( .not. b1 ) .or. b2
   print *, ( .not. b1 ) .and. b2

   print *, b1 .eqv. b2
   print *, ( .not. b1 ) .neqv. b2


end program
