! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : multipleChildStmt002l
!*
!*  DATE                       : 2007-09-18 (original: 11/04/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
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
    type :: base (lbase_1,lbase_2) ! lbase_1,lbase_2=1,3
       integer, len :: lbase_1,lbase_2
        character(lbase_1) :: i(lbase_2)
    end type

end module

program multipleChildStmt002l
use m

    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        use m
            class(base(*,*)), intent(in) :: dtv ! tcx: (*,*)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    integer :: stat1, stat2

    class(base(:,:)), allocatable :: b1 ! tcx: (:,:)
    class(base(:,:)), pointer     :: b2 ! tcx: (:,:)
    character(3) :: char1, char2

    open(1, file='multipleChildStmt002l.data', access='sequential', form='unformatted')

    allocate (b1 , source= base(1,3)((/'U','V','W'/)))   ! tcx: (1,3)
    allocate (b2 , source= base(1,3)((/'X','Y','Z'/))) ! tcx: (1,3)

    write(1) b1                    ! child I/O will not position file to next record, so it shall write "UVW" to 1 record
    write(1) b2                    ! child I/O will not position file to next record, so it shall write "XYZ" to 1 record

    rewind 1

    read (1, iostat = stat1) char1                 ! this should be "UVW"
    read (1, iostat = stat2) char2                 ! this should be "XYZ"

    if (stat1 /= 0 )      error stop 101_4
    if (stat2 /= 0 )      error stop 2_4

    if ( char1 /= "UVW" ) error stop 3_4
    if ( char2 /= "XYZ" ) error stop 4_4

    ! close the file appropriately

    close ( 1, status ='delete' )

end program



subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class(base(*,*)), intent(in) :: dtv ! tcx: (*,*)
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


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1,lbase_2) to invoke with (1,3) / declare with (*,*) - 6 changes
