! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM/TO are of type integer
!*                               FROM is component inheritted from parent
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
      type  :: base
          integer , allocatable :: i1 (:)
      end type

      type, extends(base) :: child
          type(base) :: b
      end type

      type(child), allocatable :: c

      contains

          subroutine sub(arg)
              ! dummy arg is not allocatable/pointer, type compatible
              ! with actual arg

              type(child) arg

              call move_alloc(arg%i1, arg%b%i1)

          end subroutine

end module

use m
          integer i

          allocate(c, source = child( (/ (i, i=1,9,2) /) &
                       , base( (/ (i, i=-9,-1,2) /) )  ))
          call sub(c)

          if ( allocated( c%i1) ) stop 21
          if ( .not. allocated(c%b%i1) ) stop 23

          print *, c%b%i1
          end
