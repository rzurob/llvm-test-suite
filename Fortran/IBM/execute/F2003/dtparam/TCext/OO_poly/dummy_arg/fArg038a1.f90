! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg038a1.f
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
!*  DATE                       : 05/19/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : argument association (VALUE attribute and
!                               TARGET used together)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind     :: k1
        real(k1), pointer :: data(:) => null()
    end type

    type, extends(base) :: child(k2,n1)    ! (8,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    interface
        subroutine printBase (b)
        import base
            class(base(8)), intent(in), pointer :: b
        end subroutine
    end interface

    contains

    subroutine testBaseValue (b, ptr)
        type(base(8)), target, value :: b
        class(base(8)), pointer :: ptr

        ptr => b

        call printBase (ptr)

        if (associated (ptr%data)) ptr%data = ptr%data + 1.0_8

        call printBase (ptr)
    end subroutine

    subroutine testChildValue (b, ptr)
        type(child(8,1,20)), target, value :: b
        class(base(8)), pointer :: ptr

        ptr => b

        call printBase (ptr)

        if (associated (ptr%data)) ptr%data = ptr%data + 1.0_8

        select type (ptr)
            type is (child(8,1,*))
                ptr%name = 'reNamed'
        end select

        call printBase (ptr)
    end subroutine

    subroutine testValue (b)
        class(base(8)), target :: b

        class(base(8)), pointer :: bLocal

        select type (b)
            type is (base(8))
                call testBaseValue (b, bLocal)

            type is (child(8,1,*))
                call testChildValue (b, bLocal)
        end select
    end subroutine
end module

subroutine printBase (b)
use m, only: base, child
    class(base(8)), intent(in), pointer :: b

    if (.not. associated(b)) return

    select type (b)
        type is (base(8))
            if (associated(b%data)) write (*, '(1x, 5f10.2)') b%data
        type is (child(8,1,*))
            if (associated(b%data)) then
                write (*, '(1x, 5f10.2)', advance='no') b%data
                write (*, '(1x,a)') b%name
            else
                print *, b%name
            end if
        class default

    end select
end subroutine

program fArg038a1
use m
    class (base(8)), allocatable, target :: b1(:)

    allocate (b1(2), source=(/child(8,1,20)(name='test 1'), child(8,1,20)(name='test 2')/))

    allocate (b1(1)%data(3), source=(/1.5_8, 2.5_8, 3.5_8/))

    call testValue (b1(1))
    call testValue (b1(1))

    write (*, '(1x, 5f10.2)') b1(1)%data

    call testValue (b1(2))
end
