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
! %GROUP: falloc006a17.f
! %VERIFY: falloc006a17.out:falloc006a17.vf
! %STDIN:
! %STDOUT: falloc006a17.out
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
!*  DATE                       : 12/20/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (type-spec in allocate statement)
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

program falloc006a17
    class (*), allocatable :: x0

    allocate (integer(8) :: x0)

    call assignVal (x0)

    call printVal (x0)

    deallocate (x0)

    allocate (real(16) :: x0)

    call assignVal (x0)

    call printVal (x0)

    contains

    subroutine assignVal (x)
        class (*), allocatable, intent(inout) :: x

        if (.not. allocated (x)) error stop 1_4

        select type (x)
            type is (integer(8))
                x = 10_8

            type is (real(16))
                x = 1.3
        end select
    end subroutine


    subroutine printVal (x)
        class (*), allocatable, intent(in) :: x

        if (.not. allocated (x)) error stop 2_4

        select type (x)
            type is (integer(8))
                print *, x

            type is (real(16))
                print '(f10.2)', x

        end select
    end subroutine
end

