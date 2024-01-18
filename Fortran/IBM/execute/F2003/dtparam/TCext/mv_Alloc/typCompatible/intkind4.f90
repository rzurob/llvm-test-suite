! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/F2003/mv_Alloc/typCompatible/intkind4.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=none

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
      type  :: base(n1,k1)    ! (20,4)
          integer, kind             :: k1
          integer, len              :: n1
          integer(k1) , allocatable :: i1 (:)
      end type

      type, extends(base) :: child    ! (20,4)
          type(base(n1,k1)) :: b
      end type

      type(child(:,4)), allocatable :: c

      contains

          subroutine sub(arg)
              ! dummy arg is not allocatable/pointer, type compatible
              ! with actual arg

              type(child(*,4)) arg

              call move_alloc(arg%i1, arg%b%i1)

          end subroutine

end module

use m
          integer i

          allocate(c, source = child(20,4)( (/ (i, i=1,9,2) /) &
                       , base(20,4)( (/ (i, i=-9,-1,2) /) )  ))
          call sub(c)

          if ( allocated( c%i1) ) stop 21
          if ( .not. allocated(c%b%i1) ) stop 23

          print *, c%b%i1
          end
