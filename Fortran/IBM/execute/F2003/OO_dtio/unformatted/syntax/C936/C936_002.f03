! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/04/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: CLASS(derived-type-spec) in DTIO subroutine
!*                                        shall be invoked for extensible type
!*                                           - try DTIO with polymorphic abstract type
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
    type, abstract :: base
        character(3) :: i
    end type

    type, extends(base) :: child
    end type

end module

program C936_002
use m

    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        use m
            class(base), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        use m
            class(base), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    integer :: stat
    character(20) :: errmsg = ""

    class(base), allocatable :: b1
    class(base), pointer     :: b2

    open(1, file='C936_002.data', access='sequential', form='unformatted')

    allocate (b1 , source= child('IBM'))
    allocate (b2 , source= child('FUN'))

    write(1, iostat=stat, iomsg=errmsg ) b1
    if (stat /= 0)                 error stop 1_4
    if (errmsg /= "" )             error stop 2_4

    rewind 1

    read (1, iostat=stat) b2
    if (stat /= 0)                 error stop 3_4

    if ( b2%i /= 'IBM' )           error stop 4_4

    ! close the file appropriately

    close ( 1, status ='delete' )

end program


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m
    class(base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(3) :: temp

    read (unit, iostat=iostat, iomsg=iomsg ) temp
    dtv%i = temp

end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%i

end subroutine
