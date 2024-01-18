!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: unformatted001.f
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
!*                               - UNFORMATTED= specifier: Try using INQUIRE stmt with UNFORMATTED= specifier in procedures
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
      character(10) function isUnformatted(fileorunit)
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


program unformatted001
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

   if ( isUnformatted(myunit1) /= 'UNKNOWN' ) error stop 1_4
   if ( isUnformatted(myunit2) /= 'UNKNOWN' ) error stop 2_4
   if ( isUnformatted(myunit3) /= 'UNKNOWN' ) error stop 3_4

   open (myunit1, file='unformatted001.1', form='unformatted')
   open (myunit2, file='unformatted001.2', access='direct', recl=3)
   open (myunit3, file='unformatted001.3', form='unformatted', access='stream' )

   if ( isUnformatted(myunit1) /= 'YES' ) error stop 4_4
   if ( isUnformatted(myunit2) /= 'YES' ) error stop 5_4
   if ( isUnformatted(myunit3) /= 'YES' ) error stop 6_4

   ! I/O operations

   write (myunit1, iostat=stat1, iomsg=msg1 ) b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio write' ) ) error stop 7_4

   write (myunit2, iostat=stat1, iomsg=msg1, rec=3 ) b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio write' ) ) error stop 8_4

   write (myunit3, iostat=stat1, iomsg=msg1 ) b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio write' ) ) error stop 9_4

   rewind 1

   read  (myunit1, iostat=stat1, iomsg=msg1 ) b2
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio read' ) ) error stop 10_4

   read  (myunit2, iostat=stat1, iomsg=msg1, rec=3 ) b3
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio read' ) ) error stop 11_4

   read  (myunit3, iostat=stat1, iomsg=msg1, pos=1 ) b4
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio read' ) ) error stop 12_4

   if ( b2%c /= 'ibm' )    error stop 13_4
   if ( b3%c /= 'ibm' )    error stop 14_4
   if ( b4%c /= 'ibm' )    error stop 15_4


   if ( isUnformatted(ERROR_UNIT)  /= 'NO' ) error stop 16_4
   if ( isUnformatted(INPUT_UNIT)  /= 'NO' ) error stop 17_4
   if ( isUnformatted(OUTPUT_UNIT) /= 'NO' ) error stop 18_4

   ! close the file appropriately

   close ( myunit1, status ='delete' )
   close ( myunit2, status ='delete' )
   close ( myunit3, status ='delete' )

   !  ERROR_UNIT cannot be closed

   close ( INPUT_UNIT, status ='delete' )
   close ( OUTPUT_UNIT, status ='delete' )

   if ( isUnformatted(myunit1) /= 'UNKNOWN' ) error stop 19_4
   if ( isUnformatted(myunit2) /= 'UNKNOWN' ) error stop 20_4
   if ( isUnformatted(myunit3) /= 'UNKNOWN' ) error stop 21_4
   if ( isUnformatted(ERROR_UNIT)  /= 'NO' )  error stop 22_4
   if ( isUnformatted(INPUT_UNIT)  /= 'UNKNOWN' )  error stop 23_4
   if ( isUnformatted(OUTPUT_UNIT) /= 'UNKNOWN' )  error stop 24_4

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

   if ( iostat /= 0 ) error stop 25_4

   if ( isUnformatted(unit) /= 'YES' ) error stop 26_4

   iomsg = 'dtio read'

end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()

   if ( iostat /= 0 ) error stop 27_4

   FLUSH (unit, iostat=iostat, iomsg=iomsg)

   if ( isUnformatted(unit) /= 'YES' ) error stop 28_4

   iomsg = 'dtio write'

end subroutine

character(10) function isUnformatted(fileorunit)
   class(*), intent(in) :: fileorunit
   integer :: stat

   select type ( fileorunit )
      type is (character(*))
         inquire ( file=fileorunit, unformatted=isUnformatted, iostat=stat )
      type is (integer)
         inquire ( fileorunit, unformatted=isUnformatted, iostat=stat )
      class default
         error stop 21_4
   end select

   if ( stat /= 0 ) error stop 22_4

end function
