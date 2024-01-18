!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: nextrec001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Secition 9.9 INQUIRE Statement
!*                               - NEXTREC= specifier: Try using INQUIRE stmt with NEXTREC= specifiers in procedures
!*                                                     on unformatted and Standard I/O units
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   type base
      character(3) :: c = ''
      contains
         procedure, pass :: getC
         procedure, pass :: setC
   end type

   procedure(integer(4))  :: getNextrec
   procedure(integer(4))  :: getNextrecByFile

contains
   function getC (a)
      class(base), intent(in) :: a
      character(3) :: getC
      getC = a%c
   end function

   subroutine setC (a, char)
      class(base), intent(inout) :: a
      character(3), intent(in) :: char
      a%c = char
   end subroutine

end module


program nextrec001
   use m1
   use ISO_FORTRAN_ENV

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(base), allocatable :: b1, b2, b3, b4
   integer :: stat1
   character(200) :: msg1

   integer, allocatable :: myUnit1, myUnit2, myUnit3

   ! allocation of variables

   allocate (b1, b2, b3, b4)
   allocate (myunit1, source=1)
   allocate (myunit2, source=2)
   allocate (myunit3, source=3)

   b1%c = 'ibm'

   ! units and files are not connected, therefore, processor cannot determine whether it'll be a direct access connection
   if ( getNextrec(myunit1) /= -999999 )        error stop 1_4
   if ( getNextrec(myunit2) /= -999999 )        error stop 2_4
   if ( getNextrec(myunit3) /= -999999 )        error stop 3_4

   if ( getNextrecByFile('DNE1')    /= -999999 )   error stop 4_4
   if ( getNextrecByFile('abc.f')   /= -999999 )   error stop 5_4
   if ( getNextrecByFile('abc.def') /= -999999 )   error stop 6_4

   open (myunit1, file='nextrec001.1', form='unformatted')
   open (myunit2, access='direct', recl=3)                     !<- by default, this will open file named fort.1
   open (myunit3, file='nextrec001.3', form='unformatted', access='stream' )

   if ( getNextrec(myunit1) /= -999999 )       error stop 7_4  !<- not connected for direct access
   if ( getNextrec(myunit2) /= 1       )       error stop 8_4  !<- initially file should have pos 1
   if ( getNextrec(myunit3) /= -999999 )       error stop 9_4  !<- not connected for direct access

   if ( getNextrecByFile('nextrec001.1') /= -999999 )   error stop 10_4  !<- not connected for direct access
   if ( getNextrecByFile('fort.2') /= 1 )               error stop 11_4  !<- initially file should have pos 1
   if ( getNextrecByFile('nextrec001.3') /= -999999 )   error stop 12_4  !<- not connected for direct access

   ! I/O operations

   write (myunit2, iostat=stat1, iomsg=msg1, rec=1 ) b1
   if (( stat1 /= 2 ) .or. ( msg1 /= 'dtio write' ) )   error stop 13_4
   if ( getNextrecByFile('fort.2') /= 2 )               error stop 14_4

   write (myunit2, iostat=stat1, iomsg=msg1, rec=5 ) b1
   if (( stat1 /= 6 ) .or. ( msg1 /= 'dtio write' ) )   error stop 15_4
   if ( getNextrec(2) /= 6 )                            error stop 16_4

   write (myunit2, iostat=stat1, iomsg=msg1, rec=2 ) b1
   if (( stat1 /= 3 ) .or. ( msg1 /= 'dtio write' ) )   error stop 17_4
   if ( getNextrecByFile('fort.2') /= 3 )               error stop 18_4

   read  (myunit2, iostat=stat1, iomsg=msg1, rec=1 ) b2
   if (( stat1 /= 2 ) .or. ( msg1 /= 'dtio read' ) )    error stop 19_4
   if ( getNextrecByFile('fort.2') /= 2 )               error stop 20_4

   read  (myunit2, iostat=stat1, iomsg=msg1, rec=5 ) b3
   if (( stat1 /= 6 ) .or. ( msg1 /= 'dtio read' ) )    error stop 21_4
   if ( getNextrec(2) /= 6 )                            error stop 22_4

   read  (myunit2, iostat=stat1, iomsg=msg1, rec=2 ) b4
   if (( stat1 /= 3 ) .or. ( msg1 /= 'dtio read' ) )    error stop 23_4
   if ( getNextrecByFile('fort.2') /= 3 )               error stop 24_4

   if ( b2%c /= 'ibm' )    error stop 25_4
   if ( b3%c /= 'ibm' )    error stop 26_4
   if ( b4%c /= 'ibm' )    error stop 27_4

   ! close the file appropriately

   close ( myunit1, status ='delete' )
   close ( myunit2, status ='delete' )
   close ( myunit3, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   integer :: nextrec1, nextrec2

   nextrec1 = getNextrec(unit)

   read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

   if ( iostat /= 0 ) error stop 28_4

   nextrec2 = getNextrec(unit)

   if ( nextrec1 /= nextrec2 ) error stop 29_4

   iostat = nextrec2
   iomsg = 'dtio read'

end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   integer :: nextrec1, nextrec2
   character(1) :: direct1

   inquire ( unit, nextrec=nextrec1, direct=direct1 )

   write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()

   if ( iostat /= 0 ) error stop 30_4

   FLUSH (unit, iostat=iostat, iomsg=iomsg)

   inquire ( unit, nextrec=nextrec2, direct=direct1 )

   if ( nextrec1 /= nextrec2 ) error stop 31_4

   if ( direct1 == 'Y' ) then
      iostat = nextrec2
   else
      iostat = -999999
   end if

   iomsg = 'dtio write'

end subroutine

integer(4) function getNextrec(unit)
   integer, intent(in) :: unit
   integer :: stat
   character :: direct1

   inquire ( unit, nextrec=getNextrec, direct=direct1, iostat=stat )

   if ( stat /= 0 ) error stop 32_4

   if (direct1 /= 'Y' ) then
      getNextrec=-999999
   end if

end function

integer(4) function getNextrecByFile(file)
   character(*), intent(in) :: file
   integer :: stat
   character :: direct1

   inquire ( file=file, nextrec=getNextrecByFile, direct=direct1, iostat=stat )

   if ( stat /= 0 ) error stop 33_4

   if (direct1 /= 'Y') then
      getNextrecByFile=-999999
   end if

end function