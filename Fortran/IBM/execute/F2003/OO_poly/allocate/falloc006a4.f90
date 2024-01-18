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
! %GROUP: falloc006a4.f
! %VERIFY: falloc006a4.out:falloc006a4.vf
! %STDIN:
! %STDOUT: falloc006a4.out
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
!*  DESCRIPTION                : allocate (use intrinsic types in the
!                               type-specs in allocate statement; use rank-one
!                               arrays; invlove select type construct for type
!                               conversion)
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

program falloc006a4
    class (*), pointer :: x1(:)

    class (*), allocatable :: x2(:)

    !! test logical(2)
    call createPtr (x1, (/.true._2, .true._2/))

    call testType (x1)

    !! test logical(4)
    allocate (x2(1), source=.true._4)

    call createPtr (x1, x2)

    call testType (x1)

    contains

    subroutine createPtr (x, src)
        class (*), pointer, intent(out) :: x(:)
        class (*), intent(in) :: src(:)

        select type (src)
            type is (logical(4))
                allocate (logical(4):: x(size(src)))
            type is (logical(2))
                allocate (logical(2) :: x(size(src)))
            class default
                error stop 10_4
        end select
    end subroutine

    subroutine testType (x)
        class (*), pointer, intent(in) :: x(:)

        if (.not. associated (x)) error stop 1_4

        print *, 'bounds:', lbound(x), ubound(x)

        select type (x)
            type is (logical(2))
                print *, 'logical type with kind =', kind(x)
            type is (logical(4))
                print *, 'logical type with kind =', kind(x)
            class default
                error stop 2_4
        end select
    end subroutine
end
