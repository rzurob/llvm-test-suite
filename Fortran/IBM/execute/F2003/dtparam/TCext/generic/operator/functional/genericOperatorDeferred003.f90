! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/F2003/generic/operator/functional/genericOperatorDeferred003.f
! opt variations: -qnok -ql -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Deferred specific binding with arrays
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
      contains
         procedure(i), deferred :: equal
         generic :: operator(.eq.) => equal
   end type

   type, extends(base) :: child    ! (4)
      integer(k1), allocatable :: x(:)
      contains
         procedure :: equal
         procedure :: equalint

         generic :: operator(.eq.) => equalint

   end type

   interface
      logical function i ( a, b )
         import base
         class(base(4)), intent(in) :: a, b
      end function
   end interface

   contains

   logical function equal (a, b)
      class(child(4)), intent(in) :: a
      class(base(4)), intent(in) :: b

      equal = .true.

      select type ( b )
         class is (child(4))
            if ( size(a%x) .ne. size(b%x) ) then
               equal=.false.
            else
               j = 1
               do while  ( j .le. size(a%x) )
                  equal = ( equal .and. ( a%x(j) .eq. b%x(j) ) )
                  j = j+1
               end do
            end if
         class default
            error stop 1_4
      end select

   end function

   logical function equalint (a, b)
      class(child(4)), intent(in) :: a
      integer, intent(in) :: b(:)

      equalint = .true.

      if ( size(a%x) .ne. size(b) ) then
         equalint=.false.
      else
         j = 1
      	 do while  ( j .le. size(a%x) )
            equalint = ( equalint .and. ( a%x(j) .eq. b(j) ) )
            j = j+1
         end do
      end if

   end function

end module

program genericOperatorDeferred003
   use m

   class(base(4)), allocatable :: b1
   class(child(4)), pointer    :: c1

   dimension :: k(4)

   allocate ( b1, source = child(4) ( (/1,2,3,4,5/) ) )
   allocate ( c1, source = child(4) ( (/1,2,3,4,6/) ) )

   if ( b1 == c1 ) error stop 2_4
   if ( .not. (  b1 .eq. b1 ) ) error stop 3_4

   allocate ( c1, source = child(4) ( (/ 1,2,3,4 /) ) )
   if ( b1 == c1 ) error stop 4_4

   if ( .not. ( c1 .eq. (/1,2,3,4/) ) ) error stop 5_4

   k = (/ 1,2,3,4 /)
   if ( .not. ( c1 .eq. k ) ) error stop 6_4

end program
