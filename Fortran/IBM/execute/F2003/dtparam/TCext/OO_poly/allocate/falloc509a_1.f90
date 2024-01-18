! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc509a_1.f
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
!*  DATE                       : 01/28/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocate (allocate statement for unlimited
!                               poly-entities; rank one array and use select
!                               type to verify)
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
        integer, kind :: k1
        integer(k1)      id
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,16)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    contains

    subroutine printX (x)
        class (*), allocatable, intent(in) :: x(:)

        if (allocated (x)) then
            print *, 'bounds: ', lbound(x), ubound(x)
            select type (x)
                type is (base(4))
                    print *, x
                type is (child(4,1,*))
                    print *, x
                type is (integer)
                    print *, x
                class default
                    print *, 'other data type'
            end select
        end if
    end subroutine
end module

program falloc509a_1
use m
    class (*), allocatable :: x1(:)

    integer i1(10)
    complex, pointer :: cx (:)

    class (base(4)), allocatable :: b1(:)

    !! test real data type
    allocate (x1(2), source=int((/1.0, 3.0/)))

    call printX (x1)

    !! test integer type
    i1 = (/(i, i=1,10)/)

    deallocate(x1)
    
    allocate (x1(2), source=i1(8::2))

    call printX (x1)

    !! test the complex type
    allocate (cx(0:1), source=(/(-1.0, 2.0), (3.1, -1.5)/))

    deallocate(x1)
    
    allocate (x1(2), source=int(cx))

    call printX (x1)

    !! test derived types
    allocate (b1(3), source=(/child(4,1,16)(1, 'xlftest1'), child(4,1,16)(2, 'xlftest2'), &
                    child(4,1,16)(3, 'xlftest3')/))


    deallocate(x1)
    
    allocate(x1(3), source=b1)

    call printX (x1)
end
