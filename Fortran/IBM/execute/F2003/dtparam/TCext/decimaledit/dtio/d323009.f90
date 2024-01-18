! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/decimaledit/dtio/d323009.f
! opt variations: -qnok -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/18/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 323009)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
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
        class(base(4,*)), intent(inout) :: dtv
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
    class(base(4,:)), allocatable :: b1(:)

    character(:), allocatable :: c

    call resetString

    c = ' '

    allocate(base(4,20) :: b1(100))

    read(c, *, decimal='comma') b1

    deallocate (c)

    allocate (character(1300) :: c)

    write (c, '(100e12.4)') (1.2, i=1,100)

    if (c /= string) error stop 1_4
end
