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
! %GROUP: falloc018.f
! %VERIFY: falloc018.out:falloc018.vf
! %STDIN:
! %STDOUT: falloc018.out
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
!*  DATE                       : 09/02/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (allocatable variables that are
!                               associated with each other)
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

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(15) :: name

        contains

        procedure :: print => printChild
    end type

    class (base), allocatable :: b1_m, b2_m(:)

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

module m1
    use m, b1 => b1_m, b2 => b2_m
end module

program falloc018
use m1
    if (allocated (b1) .or. allocated(b2)) error stop 1_4

    allocate (b1, source = child (1, 'b1_m'))

    allocate (b2(-1:1), source = (/child(-1,'b1_m_1'), child(0,'b1_m_2'), &
                                   child(1,'b1_m_3')/))

    !! allocating b1 and b2 applies to b1_m, b2_m
    call printM

    !! deallocate b1_m and b2_m through module m
    call deallocateM

    !! now b1 and b2 are unallocated
    if (allocated (b1) .or. allocated (b2)) error stop 2_4

    !! allocate b1, b2 through module m1 will impact on b1_m, b2_m
    call allocateM1

    call printM
end


subroutine printM
use m
    if (allocated(b1_m)) then
        print *, 'b1_m allocated'
        call b1_m%print
    else
        print *, 'b1_m not allocated'
    end if

    if (allocated (b2_m)) then
        print *, 'b2_m allocated with bounds:', lbound(b2_m,1), ubound(b2_m,1)

        do i = lbound(b2_m,1), ubound(b2_m,1)
            call b2_m(i)%print
        end do
    else
        print *, 'b2_m not allocated'
    end if
end subroutine


subroutine deallocateM
use m
    if (allocated (b1_m)) deallocate (b1_m)

    if (allocated (b2_m)) deallocate (b2_m)
end subroutine


subroutine allocateM1
use m1
    if (.not. allocated (b1)) allocate (b1, source = child(1, 'b1'))

    if (.not. allocated (b2)) allocate (b2(0:1), source=(/child(0, 'b2_1'), &
                                        child(1, 'b2_2')/))
end subroutine
