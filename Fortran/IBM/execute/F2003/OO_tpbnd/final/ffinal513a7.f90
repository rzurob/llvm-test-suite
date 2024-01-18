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
! %GROUP: ffinal513a7.f
! %VERIFY: ffinal513a7.out:ffinal513a7.vf
! %STDIN:
! %STDOUT: ffinal513a7.out
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
!*  DATE                       : 06/24/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : final sub (autodeallocation of allocatable
!                               dummy-arg with INTENT(OUT))
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
        integer*4 :: id = -1

        contains

        final :: finalizeBase, finalizeBaseRank1
        procedure :: isDefault => isBaseDefault
    end type

    type, extends(base) :: child
        character*20 :: name = 'no-name'

        contains

        final :: finalizeChild, finalizeChildRank1
        procedure :: isDefault => isChildDefault
    end type

    contains

    logical function isBaseDefault (b)
        class (base), intent (in) :: b

        isBaseDefault = (b%id == -1)
    end function

    logical function isChildDefault (b)
        class (child), intent (in) :: b

        isChildDefault = (b%base%isDefault() .and. (b%name == 'no-name'))
    end function

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent(in) :: b(:)

        print *, 'finalizeBaseRank1'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child), intent(in) :: c(:)

        print *, 'finalizeChildRank1'
    end subroutine

    integer*4 function abc (b)
        class (base), allocatable, intent(out) :: b

        if (allocated (b)) error stop 10_4

        abc = 1
    end function

    integer*4 function cba (b)
        class (base), allocatable, intent(out) :: b(:)

        if (allocated (b)) error stop 11_4

        cba = 2
    end function
end module

program ffinal513a7
use m
    class (base), allocatable :: b1, b2(:)

    allocate (b1, source=child())

    allocate (child :: b2(3))

    if (.not. b1%isDefault()) error stop 1_4

    if (size (b2) /= 3) error stop 2_4

    do i = 1, 3
        if (.not. b2(i)%isDefault())  error stop 3_4
    end do

    i1 =  abc (b1)

    i2 =  cba (b2)

    if ((i1 /= 1) .or. (i2 /= 2)) error stop 4_4

    if (allocated (b1) .or. allocated (b2)) error stop 5_4
end
