! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg520a.f
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
!*  DATE                       : 01/31/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : argument association (unlimited poly dummy-arg
!                               associated with actual-arg of array constructor
!                               with poly-entities)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind            :: k1
        integer(k1), allocatable :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        if (allocated (b%id)) then
            print *, b%id
        else
            print *, 'id not allocated'
        end if
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        if (allocated (b%id)) then
            print *, b%id, b%name
        else
            print *, 'id not allocated;', b%name
        end if
    end subroutine

    subroutine printX (x)
        class (*), intent(in) :: x(:)

        select type (x)
            class is (base(4))
                do i = 1, size(x)
                    call x(i)%print
                end do
            class default
                print *, 'other type'
        end select
    end subroutine
end module


program fArg520a
use m
    class (base(4)), allocatable :: b1(:,:)

    class (*), allocatable :: x1(:,:)

    allocate (b1(2,2), source=reshape ((/child(4,1,20) (1, 'b1 1'), child(4,1,20)(2, 'b1 2'), &
                child(4,1,20) (3, 'b1 3'), child(4,1,20)(4, 'b1 4')/), (/2,2/)))

    call printX ((/b1(1,:), b1(2,:)/))


    allocate (x1(2,2), source=reshape ((/child(4,1,20) (1, 'x1 1'), child(4,1,20)(2, 'x1 2'), &
                child(4,1,20) (3, 'x1 3'), child(4,1,20)(4, 'x1 4')/), (/2,2/)))

    call printX ((/x1(2:1:-1,2), x1(2:1:-1,1)/))
end
