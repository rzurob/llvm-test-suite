!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: number001.f
! %VERIFY:
! %STDIN: number001.stdin
! %STDOUT: number001.stdout
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
!*                               - NUMBER= specifier: Try using INQUIRE stmt with NUMBER= specifiers in procedures
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

   procedure(integer(4))  :: getNumberByFile

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


program number001
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

   integer :: number1
   integer, allocatable :: myUnit1, myUnit2, myUnit3

   ! allocation of variables

   allocate (b1, b2, b3, b4)
   allocate (myunit1, source=1)
   allocate (myunit2, source=2)
   allocate (myunit3, source=3)

   b1%c = 'ibm'

   ! units and files are not connected

   inquire ( unit = 1 , number = number1 )

   if ( getNumberByFile('fort.1')  /= -1 )   error stop 1_4
   if ( getNumberByFile('fort.2')  /= -1 )   error stop 2_4
   if ( getNumberByFile('fort.3')  /= -1 )   error stop 3_4

   if ( number1 /= 1 )                       error stop 4_4

   open (myunit1, file='number001.1', form='unformatted')
   open (myunit2, access='direct', recl=3)                     !<- by default, this will open file named fort.1
   open (myunit3, file='number001.3', form='unformatted', access='stream' )

   inquire ( unit = 3 , number = number1 )

   if ( number1 /= 3 )                          error stop 5_4

   if ( getNumberByFile('number001.1') /= 1 )   error stop 6_4
   if ( getNumberByFile('fort.2') /= 2 )        error stop 7_4
   if ( getNumberByFile('number001.3') /= 3 )   error stop 8_4

   ! I/O operations

   write (myunit1, iostat=stat1, iomsg=msg1 )        b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio write' ) )  error stop 9_4

   write (myunit2, iostat=stat1, iomsg=msg1, rec=5 ) b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio write' ) )  error stop 10_4

   write (myunit3, iostat=stat1, iomsg=msg1, pos=2 ) b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio write' ) )  error stop 11_4

   rewind 1

   read  (myunit1, iostat=stat1, iomsg=msg1 )        b2
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio read' ) )   error stop 12_4

   read  (myunit2, iostat=stat1, iomsg=msg1, rec=5 ) b3
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio read' ) )   error stop 13_4

   read  (myunit3, iostat=stat1, iomsg=msg1, pos=2 ) b4
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio read' ) )   error stop 14_4

   if ( b2%c /= 'ibm' )    error stop 15_4
   if ( b3%c /= 'ibm' )    error stop 16_4
   if ( b4%c /= 'ibm' )    error stop 17_4

   if ( getNumberByFile('number001.disconnected') /= -1 )      error stop 18_4  !<- inquire by file with a file that is not connected
   ! these two lines are commented out from the resolution of 297250 if ( getNumberByFile('number001.stdin')  /= INPUT_UNIT  )   error stop 19_4  !<- inquire by file with a file that is of standard in
   !if ( getNumberByFile('number001.stdout') /= OUTPUT_UNIT )   error stop 20_4  !<- inquire by file with a file that is of standard out

   ! close the file appropriately

   close ( myunit1, status ='delete' )
   close ( myunit2, status ='delete' )
   close ( myunit3, status ='delete' )
   close ( INPUT_UNIT  )
   close ( OUTPUT_UNIT )

   inquire ( file = 'number001.1', number = number1 )
   if ( number1 /= -1 ) error stop 21_4
   inquire ( myunit2 , number = number1 )
   if ( number1 /= 2  ) error stop 22_4

   inquire ( file = 'number001.stdin', number = number1 )
   if ( number1 /= -1 ) error stop 23_4
   inquire ( OUTPUT_UNIT , number = number1 )
   if ( number1 /= OUTPUT_UNIT  ) error stop 24_4

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   integer :: number1

   read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

   inquire ( unit=unit , number = number1 )

   if ( unit /= number1 ) error stop 25_4

   iomsg = 'dtio read'

end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   integer :: number1
   character(1) :: direct1
   character(100) :: file1

   inquire ( unit=unit , name = file1 )
   inquire ( file=file1, number=number1, iostat=iostat )

   if ( unit /= number1 ) error stop 26_4

   if ( iostat /= 0 ) error stop 23_4

   write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()

   iomsg = 'dtio write'

end subroutine


integer(4) function getNumberByFile(file)
   character(*), intent(in) :: file
   integer :: stat
   character :: direct1

   inquire ( file=file, number=getNumberByFile, iostat=stat )

   if ( stat /= 0 ) error stop 24_4

end function
