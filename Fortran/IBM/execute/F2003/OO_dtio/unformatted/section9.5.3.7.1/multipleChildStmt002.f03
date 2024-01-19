! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/04/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: An unformatted child data transfer statement
!*                                        does not position the file after the data
!*                                        transfer is completed.  Multiple child transfer
!*                                        statement in WRITE DTIO procedure.
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
    type :: base
        character(1) :: i(3)
    end type

end module

program multipleChildStmt002
use m

    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        use m
            class(base), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    integer :: stat1, stat2

    class(base), allocatable :: b1
    class(base), pointer     :: b2
    character(3) :: char1, char2

    open(1, file='multipleChildStmt002.data', access='sequential', form='unformatted')

    allocate (b1 , source= base((/'U','V','W'/)))
    allocate (b2 , source= base((/'X','Y','Z'/)))

    write(1) b1                    ! child I/O will not position file to next record, so it shall write "UVW" to 1 record
    write(1) b2                    ! child I/O will not position file to next record, so it shall write "XYZ" to 1 record

    rewind 1

    read (1, iostat = stat1) char1                 ! this should be "UVW"
    read (1, iostat = stat2) char2                 ! this should be "XYZ"

    if (stat1 /= 0 )      error stop 1_4
    if (stat2 /= 0 )      error stop 2_4

    if ( char1 /= "UVW" ) error stop 3_4
    if ( char2 /= "XYZ" ) error stop 4_4

    ! close the file appropriately

    close ( 1, status ='delete' )

end program



subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%i(1)
    if (iostat /= 0) error stop 5_4
    write (unit, iostat=iostat, iomsg=iomsg ) dtv%i(2)
    if (iostat /= 0) error stop 6_4
    write (unit, iostat=iostat, iomsg=iomsg ) dtv%i(3)
    if (iostat /= 0) error stop 7_4

end subroutine
