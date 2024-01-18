!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/01/2005
!*
!*  DESCRIPTION                : argument association (sequence association with
!                               unlimited poly dummy-arg array; use character
!                               type as the actual-arg)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains

    subroutine acceptAnything (x)
        class (*), intent(in) :: x(3)

        select type (x)
            type is (character(*))
                print *, len (x)

                call printChar (x)
        end select
    end subroutine

    subroutine printChar (c)
        character(*), intent(in) :: c(:)

        do i = 1, size(c)
            write (*, '(i5,2a)', advance='no') i, ', ', c(i)
        end do

        write (*,*) '!done'
    end subroutine
end module

program fArg518
use m
    character(*), parameter :: DAYS (7) = (/'SUN', 'MON', 'TUE', 'WED', 'THU', &
                'FRI', 'SAT'/)


    call acceptAnything ((/"xlf test", "test xlf", 'do test ', 'test do '/))

    call acceptAnything (DAYS(2))

    call acceptAnything (DAYS(2::2))
end

