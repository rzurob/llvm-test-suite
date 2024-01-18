!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: name001.f
! %VERIFY:
! %STDIN: name001.stdin
! %STDOUT: name001.stdout
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
!*                               - NAME= and NAMED= specifier: Try using INQUIRE stmt with NAME= and NAMED= specifiers in procedures
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

   interface
      character(100) function getNameByUnit(unit)
         integer, intent(in) :: unit
      end function
   end interface

   interface
      character(100) function getNameByFile(file)
         character(*), intent(in) :: file
      end function
   end interface

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


program name001
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
   b2%c = 'ftn'

   ! units are not connected to any files

   if ( getNameByUnit(myunit1) /= 'DNE' )     error stop 1_4
   if ( getNameByUnit(myunit2) /= 'DNE' )     error stop 2_4
   if ( getNameByUnit(myunit3) /= 'DNE' )     error stop 3_4

   ! Files do not exist

   if ( getNameByFile('DNE1')    /= 'DNE' )   error stop 4_4
   if ( getNameByFile('abc.f')   /= 'DNE' )   error stop 5_4
   if ( getNameByFile('abx.def') /= 'DNE' )   error stop 6_4

   open (myunit1, file='name001.1', form='unformatted')
   open (myunit2, access='direct', recl=3)
   open (myunit3, file='name001.3', form='unformatted', access='stream' )

   if ( getNameByUnit(myunit1) /= 'name001.1' )       error stop 7_4
   if ( getNameByUnit(myunit2) /= 'fort.2' )          error stop 8_4   !<- unit 2 connected to file fort.2 by default
   if ( getNameByUnit(myunit3) /= 'name001.3' )       error stop 9_4

   if ( getNameByFile('name001.1') /= 'name001.1' )   error stop 10_4
   if ( getNameByFile('fort.2') /= 'fort.2' )         error stop 11_4
   if ( getNameByFile('name001.3') /= 'name001.3' )   error stop 12_4

   ! I/O operations

   write (myunit1, iostat=stat1, iomsg=msg1 ) b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'name001.1' ) ) error stop 13_4

   write (myunit2, iostat=stat1, iomsg=msg1, rec=1 ) b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'fort.2' ) ) error stop 14_4

   write (myunit3, iostat=stat1, iomsg=msg1 ) b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'name001.3' ) ) error stop 15_4

   rewind myunit1

   read  (myunit1, iostat=stat1, iomsg=msg1 ) b2
   if (( stat1 /= 0 ) .or. ( msg1 /= 'name001.1' ) ) error stop 16_4
   read  (myunit2, iostat=stat1, iomsg=msg1, rec=1 ) b3
   if (( stat1 /= 0 ) .or. ( msg1 /= 'fort.2' ) ) error stop 17_4
   read  (myunit3, iostat=stat1, iomsg=msg1, pos=1 ) b4
   if (( stat1 /= 0 ) .or. ( msg1 /= 'name001.3' ) ) error stop 18_4

   if ( b2%c /= 'ibm' )    error stop 19_4
   if ( b3%c /= 'ibm' )    error stop 20_4
   if ( b4%c /= 'ibm' )    error stop 21_4


   if ( getNameByFile('name001.disconnected') /= 'name001.disconnected' ) error stop 22_4  !<- inquire by file with a file that is not connected
   !  these two lines are commented out due to the resolution of defect 297250   if ( getNameByUnit(INPUT_UNIT)  /= 'name001.stdin' )                   error stop 23_4  !<- inquire by file with a file that is of standard in

   !if ( getNameByUnit(OUTPUT_UNIT) /= 'name001.stdout' )                  error stop 24_4  !<- inquire by file with a file that is of standard out

   ! close the file appropriately

   close ( myunit1, status ='delete' )
   close ( myunit2, status ='delete' )
   close ( myunit3, status ='delete' )

   if ( getNameByUnit(myunit1) /= 'DNE' )     error stop 25_4
   if ( getNameByUnit(myunit2) /= 'DNE' )     error stop 26_4
   if ( getNameByUnit(myunit3) /= 'DNE' )     error stop 27_4

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(100) :: name1, name2

   read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

   if ( iostat /= 0 ) error stop 28_4

   name1 = getNameByUnit(unit)
   name2 = getNameByFile(name1)

   iomsg = name2

end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(100) :: name1, name2

   write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()

   if ( iostat /= 0 ) error stop 29_4

   FLUSH (unit, iostat=iostat, iomsg=iomsg)

   inquire (unit, name = name1 )

   inquire (file=name1, name = name2 )

   iomsg = name2

end subroutine

character(100) function getNameByUnit(unit)
   integer, intent(in) :: unit
   integer :: stat
   logical :: named1

   inquire ( unit, name=getNameByUnit, named=named1, iostat=stat )

   if ( stat /= 0 ) error stop 30_4

   if ( .not. named1 ) then
      getNameByUnit = 'DNE'
   end if

end function

character(100) function getNameByFile(file)
   character(*), intent(in) :: file
   integer :: stat
   logical :: named1

   inquire ( file=file, name=getNameByFile, named=named1, iostat=stat )

   if ( stat /= 0 ) error stop 31_4

   if ( .not. named1 ) then
      getNameByFile = 'DNE'
   end if
end function
