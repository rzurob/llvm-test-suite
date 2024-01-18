! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/mv_Alloc/typCompatible/dblcmplx1.f
! opt variations: -qnok -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is of type double complex
!*                               TO is of unlimited poly
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
      type  :: base(k1)    ! (4)
          integer, kind :: k1
          class(*), allocatable :: d1
      end type

      type, extends(base) :: child    ! (4)
          double complex, allocatable :: d2
      end type

      contains
          subroutine sub( arg )
             class(base(4)), allocatable :: arg

             if ( allocated(arg) ) then

                 select type (arg)

                     type is (child(4))

                         allocate(arg%d2, source = cmplx(2,4,8))

                         call move_alloc ( arg%d2, arg%d1 )

                         if ( .not. allocated(arg%d1 ) ) stop 21

                         if ( allocated(arg%d2) ) stop 23

                 end select

            end if

          end subroutine

end module

use m

    class(base(4)), allocatable :: b
    logical precision_R8

    allocate(child(4) :: b)

    call sub(b)

    select type ( x => b%d1)
          type is (double complex)
              if ( .not. precision_R8(x, cmplx(2,4,8)) ) stop 33
          class default
              stop 41
    end select

end
