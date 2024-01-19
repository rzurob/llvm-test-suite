! *********************************************************************
!*  ===================================================================
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
!*                                        statement in READ DTIO procedure.
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

program multipleChildStmt001l
use m

    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        use m
            class(base(*,*)), intent(inout) :: dtv ! tcx: (*,*)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    integer :: stat1, stat2

    class(base(:,:)), allocatable :: b1 ! tcx: (:,:)
    class(base(:,:)), pointer     :: b2 ! tcx: (:,:)

    open(1, file='multipleChildStmt001l.data', access='sequential', form='unformatted')

    allocate (b1 , source= base(1,3)((/'U','V','W'/)))   ! tcx: (1,3)
    allocate (b2 , source= base(1,3)((/'X','Y','Z'/))) ! tcx: (1,3)

    write(1) "ABC"
    write(1) "DEF"

    rewind 1

    read (1, iostat=stat1) b1       ! all child I/O will not position the file to next record, therefore it will read "ABC"
    read (1, iostat=stat2) b2       ! similarly, it will read "DEF"

    if (stat1 /= 0 ) error stop 101_4
    if (stat2 /= 0 ) error stop 2_4

    if ( ( b1%i(1) /= "A" ) .or. ( b1%i(2) /= "B" ) .or. ( b1%i(3) /= "C" ) )    error stop 3_4
    if ( ( b2%i(1) /= "D" ) .or. ( b2%i(2) /= "E" ) .or. ( b2%i(3) /= "F" ) )    error stop 4_4

    ! close the file appropriately

    close ( 1, status ='delete' )

end program


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m
    class(base(*,*)), intent(inout) :: dtv ! tcx: (*,*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(1), dimension(3) :: temp

    read (unit, iostat=iostat, iomsg=iomsg ) temp(1)
    if (iostat /= 0) error stop 5_4
    read (unit, iostat=iostat, iomsg=iomsg ) temp(2)
    if (iostat /= 0) error stop 6_4
    read (unit, iostat=iostat, iomsg=iomsg ) temp(3)
    if (iostat /= 0) error stop 7_4

    dtv%i(1) = temp(1)
    dtv%i(2) = temp(2)
    dtv%i(3) = temp(3)

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1,lbase_2) to invoke with (3,1) / declare with (*,*) - 6 changes
! type: base - added parameters (lbase_1,lbase_2) to invoke with (1,3) / declare with (*,*) - 6 changes
