!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc006a2.f
! %VERIFY: falloc006a2.out:falloc006a2.vf
! %STDIN:
! %STDOUT: falloc006a2.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : allocate (unlimited poly rank-one pointer array
!                               as the allocate-objects with intrinsic types as
!                               type-spec)
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

program falloc006a2
    class (*), pointer :: x1(:)

    !! integer type
    allocate (integer(2) :: x1(0:2))

    select type (x1)
        type is (integer(2))
            x1 = (/(i, i=1, size(x1))/)
        class default
            error stop 1_4
    end select

    call printX (x1)

    !! real type
    deallocate (x1)

    allocate (real(8)::x1(1:1))

    select type (x1)
        type is (real(8))
            x1 = (/(i*1.1d0, i=1, size(x1))/)
        class default
            error stop 2_4
    end select

    call printX (x1)

    contains

    subroutine printX (x)
        class (*), pointer, intent(in) :: x(:)

        if (.not. associated (x)) error stop 9_4

        print *, 'bounds', lbound(x), ubound(x)

        select type (x)
            type is (integer(2))
                print *, x
            type is (real(8))
                print '(f10.2)', x
            class default
                error stop 10_4
        end select
    end subroutine
end
