!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fmisc010a1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 277245; problem #2:
!                               array constructor with implied-do using
!                               structure constructors, followed by a use of the
!                               same structure that requires initialization)
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
    type base
        real(4), allocatable :: data
    end type

    type base1
        class(*), pointer :: data(:) => null()
        class (*), allocatable :: data2(:)
    end type
end module

program fmisc010a1
use m
    type (base) :: b1(10)
    type (base), allocatable :: b2(:)

    type (base1) :: bb1(10)
    type (base1), pointer :: bb2(:)

    logical precision_r4

    b1 = (/(base(i), i= 1, 10)/)

    allocate (b2(10))


    !! verify b1 and b2
    do i = 1, 10
        if (allocated (b2(i)%data)) error stop 1_4

        if (.not. precision_r4(b1(i)%data, i*1.0)) error stop 2_4
    end do


    bb1 = (/(base1(data2 = null()), j=1,10)/)

    allocate (bb2(2:11))


    !! verify bb1 and bb2
    do i = 1, 10
        if (associated(bb1(i)%data) .or. associated(bb2(i+1)%data)) error stop 3_4

        if (allocated(bb1(i)%data2) .or. allocated(bb2(i+1)%data2)) error stop 4_4
    end do
end
