! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/F2003/mv_Alloc/others/pure3.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/06/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM and TO are unlimit polymorphic,
!*                               dummy args
!*                               move_alloc is called inside pure subroutine
!*
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

    type shape(k1)    ! (4)
        integer, kind :: k1
    end type

    type, extends(shape) :: point    ! (4)
        integer(k1) x
        integer(k1) y
    end type

    class(*), allocatable :: z1(:,:)

end module

program main
use m

    class(*), allocatable :: x1(:,:), y1(:,:)

    integer i


    allocate(x1(2,2), source = reshape((/ (point(4)(4-i,i+4),i=-2,4,2) /), (/2,2/)))

    allocate(y1(1,2), source = reshape((/(shape(4)(), i=1,2)/),(/1,2/)))

    call move_alloc (x1, y1)

    call move_alloc (y1, z1)

    if ( allocated(x1) ) error stop 21
    if ( allocated(y1) ) error stop 23
    if ( .not. allocated(z1) ) error stop 25

    select type(z1)
        type is (point(4))
            print *, z1%x
            print *, z1%y
        class default
            stop 33
    end select

end