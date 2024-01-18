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
!*  DATE                       : 07/18/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 323009)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        class(*), allocatable :: data

        contains

        procedure :: readBaseFmtd
        generic :: read(formatted) => readBaseFmtd
    end type

    character(:), allocatable :: string

    contains

    subroutine resetString
        if (allocated (string)) deallocate (string)

        allocate(character(3000) :: string)

        string(:) =''
    end subroutine


    integer function stringIndex()
        stringIndex = len (trim(string)) + 1
    end function

    subroutine readBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        write (string(stringIndex():), '(e12.4)', &
            iostat=iostat, iomsg=iomsg) 1.2
    end subroutine
end module

use m
    class(base), allocatable :: b1(:)

    character(:), allocatable :: c

    call resetString

    c = ' '

    allocate(b1(100))

    read(c, *, decimal='comma') b1

    deallocate (c)

    allocate (character(1300) :: c)

    write (c, '(100e12.4)') (1.2, i=1,100)

    if (c /= string) error stop 1_4
end
