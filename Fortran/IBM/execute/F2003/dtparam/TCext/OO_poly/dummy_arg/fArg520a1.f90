! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg520a1.f
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
!*  DESCRIPTION                : argument association (poly-function return
!                               results to be associated with unlimited poly
!                               dummy argument array)
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
    type base(k1)    ! (8)
        integer, kind :: k1
        integer(k1)      id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (8,1,18)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(8)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(8,1,*)), intent(in) ::b

        print *, b%id, b%name
    end subroutine


    subroutine printX (x)
        class (*), intent(in) :: x(:)

        select type (x)
            class is (base(8))
                do i = 1, size(x)
                    call x(i)%print
                end do
            class default
                print *, 'other type'
        end select
    end subroutine
end module


program fArg520a1
use m
    class (base(8)), allocatable :: b1(:,:)

    allocate (b1(2,2), source=reshape ((/child(8,1,18)(1, 'b1 1'), child(8,1,18)(2, 'b1 2'), &
                child(8,1,18)(3, 'b1 3'), child(8,1,18)(4, 'b1 4')/), (/2,2/)))

    call printX (reshape ((/b1(2:1:-1,2), b1(2:1:-1,1)/), (/3/)))

    call printX (reshape (b1, (/3/)))
end
