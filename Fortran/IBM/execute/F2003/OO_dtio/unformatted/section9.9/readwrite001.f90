!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: readwrite001.f
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
!*                               - READ= WRITE= and READWRITE= specifier: Try using INQUIRE stmt with READ= WRITE= and READWRITE= specifier in procedures
!*                                                     on unformatted I/O units
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
      logical function isRead(fileorunit)
         class(*), intent(in) :: fileorunit
      end function
   end interface

   interface
      logical function isWrite(fileorunit)
         class(*), intent(in) :: fileorunit
      end function
   end interface

   interface
      logical function isReadWrite(fileorunit)
         class(*), intent(in) :: fileorunit
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


program readwrite001
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
   character(10)  :: readwrite1

   integer, allocatable :: myunit1
   integer, allocatable :: myunit2
   integer, allocatable :: myunit3

   ! allocation of variables

   allocate (b1,b2,b3,b4)
   allocate (myunit1, source=1)
   allocate (myunit2, source=2)
   allocate (myunit3, source=3)

   b1%c = 'ibm'

   inquire ( myunit1, readwrite = readwrite1 )             !<- disconnected unit
   if ( readwrite1 /= 'UNKNOWN' )                            error stop 1_4

   inquire ( file='readwrite001.f', read = readwrite1 )    !<- file not connected to any units
   if ( readwrite1 /= 'UNKNOWN' )                            error stop 2_4

   inquire ( myunit2, write = readwrite1 )                 !<- file not connected to any units
   if ( readwrite1 /= 'UNKNOWN' )                            error stop 3_4

   open (myunit1, file='readwrite001.1', form='unformatted', access='sequential'      , action='read', status='old')
   open (myunit2, file='readwrite001.2', form='unformatted', access='direct', recl=3  , action='write' )
   open (myunit3, file='readwrite001.3', form='unformatted', access='stream'          , action='readwrite' )

   if ( ( .not. isRead(myunit1) ) .or. ( isWrite(myunit1) ) .or. ( isReadWrite(myunit1) ) )                                          error stop 4_4
   if ( ( isRead('readwrite001.2') ) .or. ( .not. isWrite('readwrite001.2') ) .or. ( isReadWrite('readwrite001.2') ) )               error stop 5_4
   if ( ( .not. isRead('readwrite001.3') ) .or. ( .not. isWrite('readwrite001.3') ) .or. ( .not. isReadWrite('readwrite001.3') ) )   error stop 6_4

   ! I/O operations

   write (myunit1, iostat=stat1, iomsg=msg1 )          b1        !<- shall not call DTIO
   if ( stat1 == 0 )    error stop 7_4
   msg1=''

   write (myunit2, iostat=stat1, iomsg=msg1, rec=1 )   b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'nonreadable' ) )           error stop 8_4

   write (myunit3, iostat=stat1, iomsg=msg1, pos=4 )   b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'readable' ) )              error stop 9_4

   rewind 1

   read  (myunit1, iostat=stat1, iomsg=msg1 )          b2
   if (( stat1 /= 0 ) .or. ( msg1 /= 'nonwritable' ) )           error stop 10_4

   read  (myunit2, iostat=stat1, iomsg=msg1,rec=1 )    b3
   if ( stat1 == 0  )                                            error stop 11_4

   read  (myunit3, iostat=stat1, iomsg=msg1, pos=4 )   b4
   if (( stat1 /= 0 ) .or. ( msg1 /= 'writable' ) )              error stop 12_4


   if ( b2%c /= 'ibm' ) error stop 13_4
   if ( b4%c /= 'ibm' ) error stop 14_4

   ! inquire on standard I/O ( all standard I/O has action of 'readwrite' )

   if ( ( .not. isRead(ERROR_UNIT) ) .or. ( .not. isWrite(ERROR_UNIT) ) .or. ( .not. isReadWrite(ERROR_UNIT) ) )        error stop 15_4
   if ( ( .not. isRead(INPUT_UNIT) ) .or. ( .not. isWrite(INPUT_UNIT) ) .or. ( .not. isReadWrite(INPUT_UNIT) ) )        error stop 16_4
   if ( ( .not. isRead(OUTPUT_UNIT) ) .or. ( .not. isWrite(OUTPUT_UNIT) ) .or. ( .not. isReadWrite(OUTPUT_UNIT) ) )     error stop 17_4


   ! close the file appropriately

   close ( myunit2, status ='delete' )
   close ( myunit3, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(20) :: name1
   inquire (unit, name=name1)

   if ( isRead(name1) ) then
      read (unit, iostat=iostat, iomsg=iomsg ) dtv%c
   end if

   if ( isWrite(unit) ) then
      iomsg = 'writable'
   else
      iomsg = 'nonwritable'
   endif

end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( isWrite(unit) ) then
      write (unit, iostat=iostat, iomsg=iomsg ) dtv%c
   end if

   if ( isRead(unit) ) then
      iomsg = 'readable'
   else
      iomsg = 'nonreadable'
   endif


end subroutine

logical function isRead(fileorunit)
   class(*), intent(in) :: fileorunit
   integer :: stat
   character(9) :: read1

   select type ( fileorunit )
      type is (character(*))
         inquire ( file=fileorunit, read=read1, iostat=stat )
      type is (integer)
         inquire ( fileorunit, read=read1, iostat=stat )
      class default
         error stop 18_4
   end select

   if ( stat /= 0 ) error stop 19_4

   if ( read1 == 'YES' ) then
      isRead = .true.
   else
      isRead = .false.
   end if

end function

logical function isWrite(fileorunit)
   class(*), intent(in) :: fileorunit
   integer :: stat
   character(9) :: write1

   select type ( fileorunit )
      type is (character(*))
         inquire ( file=fileorunit, write=write1, iostat=stat )
      type is (integer)
         inquire ( fileorunit, write=write1, iostat=stat )
      class default
         error stop 20_4
   end select

   if ( stat /= 0 ) error stop 21_4

   if ( write1 == 'YES' ) then
      isWrite = .true.
   else
      isWrite = .false.
   end if

end function

logical function isReadWrite(fileorunit)
   class(*), intent(in) :: fileorunit
   integer :: stat
   character(9) :: readwrite1

   select type ( fileorunit )
      type is (character(*))
         inquire ( file=fileorunit, readwrite=readwrite1, iostat=stat )
      type is (integer)
         inquire ( fileorunit, readwrite=readwrite1, iostat=stat )
      class default
         error stop 22_4
   end select

   if ( stat /= 0 ) error stop 23_4

   if ( readwrite1 == 'YES' ) then
      isReadWrite = .true.
   else
      isReadWrite = .false.
   end if

end function