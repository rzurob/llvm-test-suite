! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp /tstdev/OO_poly/point_assgn/fpAssgn001a6.f
! opt variations: -qnol -qdeferredlp

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
! %GROUP: fpAssgn001a6.f
! %VERIFY: fpAssgn001a6.out:fpAssgn001a6.vf
! %STDIN:
! %STDOUT: fpAssgn001a6.out
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
!*  DATE                       : 03/10/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id

        contains

        procedure, nopass :: typeID => baseID
        procedure :: print => basePrint
    end type

    type, extends(base) :: child(k2)    ! (20,4,2)
        integer, kind :: k2
        logical(k2)   :: flag

        contains

        procedure, nopass :: typeID => childID
        procedure :: print => childPrint
    end type

    type(child(20,4,2)), allocatable, target :: c1_m(:)

    class (base(20,4)), allocatable, target :: b1_m(:)
    class (child(20,4,2)), allocatable, target :: c2_m(:)

    contains

    integer*2 function baseID ()
        baseID = 1
    end function

    integer*2 function childID ()
        childID = 2
    end function

    subroutine basePrint (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine childPrint (b)
        class (child(*,4,2)), intent(in) :: b

        print *, b%id, b%flag
    end subroutine
end module

program fpAssgn001a6
use m

    !! user defined assignments
    interface assignment(=)
        subroutine copyChild (c, c1)
        use m
            class (child(*,4,2)), intent(out) :: c(:)
            type (child(*,4,2)), intent(in) :: c1(:)
        end subroutine

        subroutine copyBase (b, b1)
        use m
            class (base(*,4)), intent(out) :: b(:)
            type (base(*,4)), intent(in) :: b1(:)
        end subroutine
    end interface

    !! print routine
    interface
        subroutine printData (b)
        use m
            class (base(*,4)), intent(in) :: b(:)
        end subroutine
    end interface

    class (base(20,4)), pointer :: b_ptr(:)
    class (child(20,4,2)), allocatable, target :: c1(:)

    type (base(20,4)), allocatable, target :: b1(:)
    type (child(20,4,2)), allocatable, target :: c2(:)

    !! test b1 and c2 first

    allocate (b1(100), c2(20))

    b1 = (/(base(20,4) (i), i=1,100)/)
    c2 = (/(child(20,4,2) (i, mod(i,2)==0), i=1,20)/)

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
    class (child(*,4,2)), intent(out) :: c(:)
    type (child(*,4,2)), intent(in) :: c1(:)

    if (size(c) /= size(c1)) then
        print *, 'size of the two arrays are NOT the same'
        stop 1
    end if

    c%id = c1%id
    c%flag = c1%flag
end subroutine

subroutine copyBase (b, b1)
use m
    class (base(*,4)), intent(out) :: b(:)
    type (base(*,4)), intent(in) :: b1(:)

    if (size(b) /= size(b1)) then
        print *, 'size of the two arrays are NOT the same'
        stop 1
    end if

    b%id = b1%id
end subroutine

subroutine printData (b)
use m
    class (base(*,4)), intent(in) :: b(:)

    do i = 1, size(b)
        call b(i)%print
    end do

end subroutine

