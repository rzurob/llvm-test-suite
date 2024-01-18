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
! %GROUP: falloc006.f
! %VERIFY: falloc006.out:falloc006.vf
! %STDIN:
! %STDOUT: falloc006.out
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
!*  DATE                       : 12/15/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : allocate (unlimited poly allocatable scalars in
!                               allocate stmt with type-spec)
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
        integer(8) id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(15), pointer :: name => null()

        contains

        procedure :: print => printChild
        final :: finalizeChild, finalizeChildRank1
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        if (associated (b%name)) then
            print *, b%id, b%name
        else
            print *, b%id, 'null'
        end if
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(inout) :: c
        integer err

        print *, 'finalizeChild'

        if (associated (c%name)) then
            print *, 'deallocating name'
            deallocate (c%name, stat=err)

            if (err /= 0) print *, 'deallocating data failed'
        end if
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child), intent(inout) :: c(:)

        print *, 'finalizeChildRank1'

        do i = 1, size(c)
            call finalizeChild (c(i))
        end do
    end subroutine
end module

program falloc006
use m
    class (*), pointer :: x0, x1(:)

    allocate (child:: x0)

    select type(x0)
        type is (base)
            x0%id = 120

        type is (child)
            x0%id = 220
            allocate (x0%name, source='xlftest abc')
    end select

    select type (x0)
        class is (base)
            call x0%print
    end select

    deallocate (x0)
end
