!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn001a6.f
! %VERIFY: fpAssgn001a6.out:fpAssgn001a6.vf
! %STDIN:
! %STDOUT: fpAssgn001a6.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data point assignment (poly pointer array
!*                               assigned to allocatables and array sections;
!*                               resolve the type-bound)
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

        procedure, nopass :: typeID => baseID
        procedure :: print => basePrint
    end type

    type, extends(base) :: child
        logical*2 :: flag

        contains

        procedure, nopass :: typeID => childID
        procedure :: print => childPrint
    end type

    type(child), allocatable, target :: c1_m(:)

    class (base), allocatable, target :: b1_m(:)
    class (child), allocatable, target :: c2_m(:)

    contains

    integer*2 function baseID ()
        baseID = 1
    end function

    integer*2 function childID ()
        childID = 2
    end function

    subroutine basePrint (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine childPrint (b)
        class (child), intent(in) :: b

        print *, b%id, b%flag
    end subroutine
end module

program fpAssgn001a6
use m

    !! user defined assignments
    interface assignment(=)
        subroutine copyChild (c, c1)
        use m
            class (child), intent(out) :: c(:)
            type (child), intent(in) :: c1(:)
        end subroutine

        subroutine copyBase (b, b1)
        use m
            class (base), intent(out) :: b(:)
            type (base), intent(in) :: b1(:)
        end subroutine
    end interface

    !! print routine
    interface
        subroutine printData (b)
        use m
            class (base), intent(in) :: b(:)
        end subroutine
    end interface

    class (base), pointer :: b_ptr(:)
    class (child), allocatable, target :: c1(:)

    type (base), allocatable, target :: b1(:)
    type (child), allocatable, target :: c2(:)

    !! test b1 and c2 first

    allocate (b1(100), c2(20))

    b1 = (/(base (i), i=1,100)/)
    c2 = (/(child (i, mod(i,2)==0), i=1,20)/)

    b_ptr => b1(::10)

    if ((size(b_ptr) /= 10) .or. (b_ptr%typeID() /=1)) error stop 1_4

    call printData (b_ptr)

    call printData (b_ptr(::2))

    print *, 'test c2'

    b_ptr => c2(2::2)

    if ((size(b_ptr) /= 10) .or. (b_ptr%typeID() /= 2)) error stop 2_4


    call printData (b_ptr)

    !! test c1 now
    print *, 'test c1'

    allocate (c1(10))

    c1 = c2(::2)

    b_ptr => c1

    if ((size(b_ptr) /= 10) .or. (b_ptr%typeID() /= 2)) error stop 3_4


    call printData (b_ptr)

    call printData (b_ptr(2::3))
end

subroutine copyChild (c, c1)
use m
    class (child), intent(out) :: c(:)
    type (child), intent(in) :: c1(:)

    if (size(c) /= size(c1)) then
        print *, 'size of the two arrays are NOT the same'
        stop 1
    end if

    c%id = c1%id
    c%flag = c1%flag
end subroutine

subroutine copyBase (b, b1)
use m
    class (base), intent(out) :: b(:)
    type (base), intent(in) :: b1(:)

    if (size(b) /= size(b1)) then
        print *, 'size of the two arrays are NOT the same'
        stop 1
    end if

    b%id = b1%id
end subroutine

subroutine printData (b)
use m
    class (base), intent(in) :: b(:)

    do i = 1, size(b)
        call b(i)%print
    end do

end subroutine

