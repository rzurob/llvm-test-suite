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
!*  DATE                       : 06/06/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (A test case on using the
!                               external procedure with explicit interface as
!                               the type bound.)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k,n)
        integer, kind :: k
        integer, len :: n

        complex(k) data(n)

        character(:), allocatable :: str(:)

        contains

        procedure :: format => formatCmplx8
    end type

    abstract interface
        character(:) function formatBase8 (b1, fmt)
        import
            character(*), intent(in) :: fmt
            class(base(8,*)), intent(inout) :: b1

            allocatable formatBase8(:)
        end function
    end interface

    procedure (formatBase8) formatCmplx8
end module


program dtpPass009
use m
    type(base(8,:)), allocatable :: b1
    
    character(:), allocatable :: str(:)

    complex(8), allocatable :: cx(:)

    cx = [(cmplx(i, i+1,8), i=1, 20)]

    allocate (b1, source=base(8,size(cx))(cx, null()))

    str = b1%format ('10F15.10')

    if (.not. allocated(b1%str)) error stop 1_4

    if ((size(b1%str) /= 4) .or. (b1%str%len /= 150)) error stop 2_4

    if (any (b1%str /= str)) error stop 3_4

    print *, (str(i), new_line('a'), i=1,4)

    str = b1%format ('16g13.6')

    if ((size(b1%str) /= 3) .or. (b1%str%len /= 16*13)) error stop 4_4

    if (any (b1%str /= str)) error stop 5_4

    print *, (str(i), new_line('a'), i=1,3)

end

!! this function does the following: create buffer for b1%str based on format
!input from user and then return this value
!! assume the format, fmt, is in a form like '10f15.3'
character(:) function formatCmplx8 (b1, fmt)
use m, only: base
    character(*), intent(in) :: fmt
    class(base(8,*)), intent(inout) :: b1

    allocatable formatCmplx8(:)

    integer index, repeatFactor, iwidth, i
    real width

    index = scan (fmt, 'dDeEfFgG')

    if (index == 0) stop 100

    read(fmt(:index-1), *) repeatFactor
    read(fmt(index+1:), *) width

    iwidth = width

    !! now allocate strings based on the information
    if (allocated(b1%str)) deallocate (b1%str)

    if (repeatFactor * (2*b1%n/repeatFactor) == 2*b1%n) then
        allocate (character(repeatFactor*iwidth) :: b1%str(2*b1%n/repeatFactor))
    else
        allocate(character(repeatFactor*iwidth) :: b1%str(2*b1%n/repeatFactor+1))
    end if

    b1%str(:) = ''

    do i = 1, size(b1%str)-1
        write (b1%str(i), '('//fmt//')') b1%data((i-1)*repeatFactor/2+1: &
                i*repeatFactor/2)
    end do

    write (b1%str(i), '('//fmt//')') b1%data((i-1)*repeatFactor/2+1:)

    formatCmplx8 = b1%str
end function
