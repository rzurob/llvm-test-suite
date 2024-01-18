! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc005a11.f
!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc005a11.f
! %VERIFY: falloc005a11.out:falloc005a11.vf
! %STDIN:
! %STDOUT: falloc005a11.out
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 07/14/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (function reference as the source-expr
!                               in ALLOCATE; use rank-one arrays)
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
    type base(k1)    ! (4)
        integer, kind        :: k1
        integer(k1), pointer :: data(:) => null()

        contains

        procedure, nopass :: copy => copyBaseArray
        procedure :: print => printBase
    end type

    contains

    type (base(4)) function copyBaseArray (b)
        type (base(4)), intent(in) :: b(:)

        dimension :: copyBaseArray (size(b))

        do i = 1, size(b)
            if (associated(b(i)%data))   &
                allocate(copyBaseArray(i)%data &
                    (lbound(b(i)%data,1):ubound(b(i)%data,1)), &
                    source=b(i)%data)
        end do
    end function

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        if (associated (b%data)) then
            print *, 'bounds:', lbound(b%data,1), ubound(b%data,1)
            print *, 'data:', b%data
        else
            print *, 'not associated'
        end if

        print *, ''
    end subroutine
end module

program falloc005a11
use m
    type (base(4)), pointer :: b1(:)

    type (base(4)) :: b2(6)

    integer(4), target :: i1(0:2), i2 (100:103), i3(-100:-99), i4(0:0), i5(3)

    i1 = (/0,1,2/)
    i2 = (/100,101,102,103/)
    i3 = (/-100,-99/)
    i4 = (/0/)
    i5 = (/1,2,3/)

    b2(1)%data => i1
    b2(2)%data => i2
    b2(3)%data => i3
    b2(4)%data => i4
    b2(5)%data => i5

    allocate (b1(size(b2)), source=b2%copy(b2))

    if (size (b1) /= 6) error stop 1_4

    do i = 1, 6
        call b1(i)%print
    end do


    do i = 1, 5
        deallocate (b1(i)%data)
    end do

    deallocate (b1)
end
