! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet009a3.f
!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 05/10/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : poly-function return (defined operator, defined
!                               assignment)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (1,30)
        integer, kind                          :: k1
        integer, len                           :: n1
        character(kind=k1,len=n1), allocatable :: str
    end type

    interface operator(//)
        module procedure concatenate
        module procedure concatenate2
    end interface

    interface assignment(=)
        module procedure assgnB1B2
    end interface

    private concatenate

    contains

    class (base(1,:)) function concatenate (b, c)
        class (base(1,*)), intent(in) :: b
        character(*), intent(in) :: c
        allocatable concatenate

        allocate (base(1,30) :: concatenate)

        if (.not. allocated(b%str)) then
            allocate (concatenate%str, source = c)
        else
            allocate (concatenate%str, source = trim(b%str)//' '//c)
        end if
    end function

    class (base(1,:)) function concatenate2 (b, c)
        class(base(1,*)), intent(in) :: b, c
        allocatable concatenate2

        allocate (base(1,30) :: concatenate2)

        concatenate2 =  b // c%str
    end function

    subroutine assgnB1B2 (b1, b2)
        class(base(1,*)), intent(in) :: b2
        class(base(1,*)), intent(out) :: b1

        if (.not. same_type_as(b1, b2)) error stop 10_4

        select type (b1)
            type is (base(1,*))
                allocate (b1%str, source=b2%str)
            class default
                error stop 11_4
        end select
    end subroutine
end module

program ffuncRet009a3
use m
    class (base(1,:)), pointer :: b1, b2(:)

    allocate(base(1,30) :: b1, b2(0:2))

    b1 = b1 // 'xlftest'

    b1 = b1 // 'team'

    if (b1%str /= 'xlftest team') error stop 1_4

    b2(0) = b1

    b2(1) = b2(0) // b1

    if (b2(1)%str /= 'xlftest team xlftest team') error stop 2_4
end
