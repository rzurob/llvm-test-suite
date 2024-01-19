! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM/TO are of type integer*8
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
      type  :: base
          integer*8, public, allocatable :: i1 (:)
      end type

      type(base), pointer  :: b
      type(base) d

      contains
         type(base) function func()
            pointer func

            allocate(func, source=base( int( (/1,2,3,4/), 8) ) )
         end function

end module

      use m
      integer(8) i

       allocate(b, source = func())

       call move_alloc(b%i1, d%i1)

       if ( allocated(b%i1)) error stop 21
       if ( .not. allocated(d%i1) ) error stop 23

       do i = 1, 4
          if ( d%i1(i) /= i ) call zzrc(i_4)
       end do

       end
