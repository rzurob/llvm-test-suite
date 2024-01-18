!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc006a3.f
! %VERIFY: falloc006a3.out:falloc006a3.vf
! %STDIN:
! %STDOUT: falloc006a3.out
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
!*  DESCRIPTION                : allocate (intrinsic type-specs for unlimited
!                               poly allocatable arrays; use select type to
!                               verify the results)
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

program falloc006a3
    class (*), allocatable :: x1(:)

    class (*), pointer :: x2(:)

    !! test character type
    call createAlloc (x1, (/'xlftest abc', 'xlftest xyz'/))

    call testType (x1)

    !! test complex data type

    allocate (x2(2), source=(/(-1.0, 2.1), (2.1, .4)/))

    call createAlloc (x1, x2)

    call testType(x1)

    contains

    subroutine createAlloc (x, src)
        class (*), allocatable, intent(out) :: x(:)
        class (*), intent(in) :: src(:)

        select type (src)
            type is (complex)
                allocate (complex:: x(size(src)))
            type is (character(*))
                allocate (character(len(src)) :: x(size(src)))
        end select
    end subroutine

    subroutine testType (x1)
        class (*), intent(in), allocatable :: x1(:)

        if (.not. allocated (x1)) error stop 1_4

        print *, 'bounds: ', lbound(x1), ubound(x1)
        select type (x1)
            type is (character(*))
                print *, 'character type with lenght parameter of', len(x1)
            type is (complex)
                print *, 'default complex type'
            class default
                error stop 2_4
        end select
    end subroutine
end
