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
!*  DATE                       : 02/03/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : pointer assignment (bind(c) type pointer
!                               assigned to unlimited poly target in select type
!                               construct)
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
use iso_c_binding
    type, bind(c) :: bType
        integer(c_size_t) :: length
        real (c_float) :: r
        character(kind=c_char) :: name(20) = ' '
    end type

    interface
        integer(c_int) function cFunc1 (b1) bind(c, name='cfunc1')
        use iso_c_binding
        import bType
            type (bType) :: b1
        end function

        type (bType) function cFunc2 (b) bind(c, name='cfunc2')
        use iso_c_binding
        import bType
            type (bType) :: b(2)
        end function
    end interface
end module

program fpAssgn503a
use m
    class (*), allocatable, target :: x1(:)

    type (bType), pointer :: b1, b2(:)

    allocate (bType :: x1(3))

    select type (y1 => x1(2))
        class default
            b1 => y1

            if (cFunc1 (b1) /= 0) error stop 1_4

            write (*, '(i5, f10.2, 1x, 20a)') b1
    end select


    select type (y2 => x1(1::2))
        class default
            b2 => y2

            b2%length = (/20, 100/)
            b2%r = (/1.2, 5.5/)
            b2(1)%name(1:3) = (/'x', 'y','z'/)
            b2(2)%name(1:4) = (/'6', '5','4', '3'/)


            write (*, '(i5, f10.2, 1x, 20a)') cFunc2(b2)
    end select

end
