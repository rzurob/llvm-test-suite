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
! %GROUP: fpAssgn003a4.f
! %VERIFY: fpAssgn003a4.out:fpAssgn003a4.vf
! %STDIN:
! %STDOUT: fpAssgn003a4.out
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
!*  DATE                       : 04/26/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : data pointer assignment (non-poly pointer array
!*                               assigned to poly-pointer array)
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
        integer*4 :: id

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character(20) :: name

        contains

        procedure :: print => printChild
    end type

    class (base), pointer :: b1_m(:)
    class (child), allocatable, target :: c1_m(:)

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

program fpAssgn003a4
use m
    type (base), pointer :: b_ptr(:)

    type (child), target :: c1(10:30)

    class (child), pointer :: c_ptr(:)
    type (child), pointer :: c_ptr2(:)

    allocate (c_ptr(10))

    c_ptr%id = (/(i, i=1,10)/)
    c_ptr%name = 'c_ptr'

    b1_m => c_ptr
    b_ptr => b1_m      ! assign to compatible type

    if (.not. associated (b_ptr, c_ptr%base)) error stop 1_4

    do i = 1, 10
        call b_ptr(i)%print
    end do


    print *, 'test c_ptr2'

    c_ptr2 => c_ptr

    if (.not. associated (b_ptr, c_ptr2%base)) error stop 2_4

    do i = 1, 10
        call c_ptr2(i)%print
    end do

    deallocate(c_ptr)


    print *, 'test c1'

    c1 = (/(child (i, 'c1'), i=10,30)/)

    c_ptr => c1
    b_ptr => c_ptr%base


    if ((size (b_ptr) /= 21) .or. (lbound(b_ptr,1) /= 1) .or. &
        (ubound(b_ptr,1) /= 21))    error stop 3_4

    do i = 1, 20, 2
        call b_ptr(i)%print
        call c_ptr(10+i)%print
    end do

    call b_ptr(21)%print

    if (.not. associated (b_ptr, c1%base)) error stop 4_4

    !! use module variables
    print *, 'test b1_m, c1_m'

    b1_m => c1
    c1 = (/(child(10*i, 'b1_m'), i=10,30)/)

    b_ptr => b1_m

    if ((lbound(b_ptr,1)/= 10) .or. (ubound(b_ptr, 1) /= 30)) error stop 5_4

    do i = 10, 29, 2
        call b1_m(i)%print
        call b_ptr(i+1)%print
    end do

    allocate (c1_m(5))

    c1_m%id = (/1,2,3,4,5/)
    c1_m%name = 'c1_m'

    b1_m => c1_m

    b_ptr => b1_m(2::2)

    do i = 1, 5
        call b1_m(i)%print
    end do

    call b_ptr(1)%print

    call b_ptr(2)%print
end
