!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/14/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 321506)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    class(*), pointer :: x
    character(:), allocatable :: str

    allocate (character*20 :: x)
    allocate (character*(20) :: str)

    str = 'xlftest 101'

    select type (x)
        type is (character*(*))
            x = str

        class default
            stop 10
    end select

    call verify (x, str)

    contains

    subroutine verify (x1, s)
        class(*), intent(in) :: x1
        character*(*), intent(in) :: s

        select type (x1)
            type is (character*(*))
                if (x1 /= s) error stop 1_4

            class default
                stop 20
        end select
    end subroutine
    end
