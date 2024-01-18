!*  ===================================================================
!*
!*  DATE                       : 06/07/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Procedure Pointer with BIND(C) feature
!*                                        Non-bind(c) derived-type contains a nopass bind(c) procedure pointer component
!*                                        subroutine procedure containing C_INT dummy arguments, and pass by reference
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

   use ISO_C_BINDING

   interface
      subroutine printandreset (i,j,k) bind(c, name='print3int')
         import C_INT
         integer(C_INT), intent(inout) :: i,j,k
      end subroutine
   end interface

   interface
      subroutine inf (i,j,k) bind(c)
         import C_INT
         integer(C_INT), intent(inout) :: i,j,k
      end subroutine
   end interface

   type base
      integer(C_INT) :: i,j,k
      procedure(inf), pointer, nopass :: pp
   end type

end module

   use m

   type(base) :: b1
   type(base), pointer :: b2
   type(base), allocatable :: b3

   b1 = base(10_C_INT, 100_C_INT, 1000_C_INT, printandreset)
   allocate ( b2, b3 )

   b2 = base(20, 200, 2000, b1%pp )
   b3 = base(30, 300, 3000, b2%pp )

   call b1%pp( b1%i, b1%j, b1%k )
   call b2%pp( b2%i, b2%j, b2%k )
   call b3%pp( b3%i, b3%j, b3%k )

   if ( ( b1%i /= -999 ) .or. ( b1%j /= -999 ) .or. ( b1%k /= -999 ) .or. ( .not. associated(b1%pp) ) ) error stop 1_4
   if ( ( b2%i /= -999 ) .or. ( b2%j /= -999 ) .or. ( b2%k /= -999 ) .or. ( .not. associated(b2%pp) ) ) error stop 2_4
   if ( ( b3%i /= -999 ) .or. ( b3%j /= -999 ) .or. ( b3%k /= -999 ) .or. ( .not. associated(b3%pp) ) ) error stop 3_4

   nullify ( b1%pp )

   if ( associated ( b1%pp ) .or. ( .not. associated ( b2%pp ) ) .or. ( .not. associated ( b3%pp ) ) )  error stop 4_4

end
