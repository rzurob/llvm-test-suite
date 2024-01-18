!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg601a1.f
! %VERIFY: fArg601a1.out:fArg601a1.vf
! %STDIN:
! %STDOUT: fArg601a1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (unlimited poly
!                               explicit-shape array in sequence association)
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
        integer(4) :: id
    end type

    type, extends(base) :: child
        character (15) :: name
    end type

    contains

    function iType (x)
        class (*), intent(in) :: x (2)

        select type (x)
            type is (base)
                print *, x
                iType = 1
            type is (child)
                print *, x
                iType = 2
            type is (integer(4))
                print *, x
                iType = 3
            class default
                print *, 'other type'
                iType = -1
        end select
    end function
end module

program fArg601a1
use m
    class (*), pointer :: x1(:)
    integer retVal

    allocate (x1(3), source=(/1_4, 2_4, 3_4/))

    retVal = iType (x1)

    if (retVal /= 3) error stop 1_4

    retVal = iType (x1(::2))

    if (retVal /= 3) error stop 2_4

    deallocate (x1)

    allocate (x1(0:3), source=(/(base(j), j=0,3)/))

    retVal = iType (x1((/3,2,0/)))

    if (retVal /= 1) error stop 3_4

    deallocate (x1)

    allocate (x1(0:2), source=(/child(0, 'xlftest 00'), child(1, 'xlftest 01'), &
                        child (2, 'xlftest 02')/))

    retVal = iType (x1(::2))

    if (retVal /= 2) error stop 4_4

    retVal = iType (x1((/2,2/)))

    if (retVal /= 2) error stop 5_4

    deallocate (x1)
end
