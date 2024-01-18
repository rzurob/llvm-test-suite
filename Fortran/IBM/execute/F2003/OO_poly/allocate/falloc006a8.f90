!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc006a8.f
! %VERIFY: falloc006a8.out:falloc006a8.vf
! %STDIN:
! %STDOUT: falloc006a8.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : allocate (intrinsic types used in source-expr
!                               for allocating unlimited poly allocatable array;
!                               use the logical types)
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

program falloc006a8
    class (*), allocatable :: x1(:,:)

    call createX (x1, reshape ((/.true._4, .true._4, .false._4, .false._4/), &
                        (/2,2/)))

    call printX (x1)


    call createX (x1, reshape ((/.true._2, .true._2, .false._2, .false._2/), &
                        (/2,2/)))

    call printX (x1)

    contains

    subroutine createX (x, src)
        class (*), allocatable, intent(out) :: x(:,:)
        class (*), intent(in) :: src(:,:)

        integer isize(2)

        isize = shape (src)

        select type (src)
            type is (logical(2))
                allocate(x(isize(1), isize(2)), source=src)
            type is (logical (4))
                allocate (x(isize(1), isize(2)), source=.not. src)
            class default
                error stop 1_4
        end select
    end subroutine

    subroutine printX (x)
        class (*), allocatable, intent(in) :: x(:,:)

        if (.not. allocated (x)) error stop 3_4

        print *, 'lbounds:', lbound(x), 'ubounds:', ubound(x)

        select type (x)
            type is (logical(2))
                print *, x
            type is (logical(4))
                print *, x
        end select
    end subroutine
end
