! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Secition 9.9 INQUIRE Statement
!*                               - SEQUENTIAL= specifier: Try using INQUIRE stmt with SEQUENTIAL= specifier in procedures
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
      character(10) function isSequential(fileorunit)
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


program sequential001
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

   integer, allocatable :: myunit1
   integer, allocatable :: myunit2
   integer, allocatable :: myunit3

   ! allocation of variables

   allocate (b1,b2,b3,b4)
   allocate (myunit1, source=1)
   allocate (myunit2, source=2)
   allocate (myunit3, source=3)

   b1%c = 'ibm'

   if ( isSequential(myunit1)  /= 'UNKNOWN' )                  error stop 1_4
   if ( isSequential('fort.2') /= 'UNKNOWN' )                  error stop 2_4
   if ( isSequential('sequential001.f') /= 'UNKNOWN' )         error stop 3_4

   open (myunit1, file='sequential001.1', form='unformatted', access='sequential'     )
   open (myunit2, file='sequential001.2', form='unformatted', access='direct', recl=3 )
   open (myunit3, file='sequential001.3', form='unformatted', access='stream' )

   if ( isSequential(myunit1)           /= 'YES' )             error stop 4_4
   if ( isSequential('sequential001.2') /= 'NO' )              error stop 5_4
   if ( isSequential(myunit3)           /= 'NO' )              error stop 6_4

   ! I/O operations

   write (myunit1, iostat=stat1, iomsg=msg1 )          b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'YES' ) )         error stop 7_4
   msg1=''
   write (myunit2, iostat=stat1, iomsg=msg1, rec=100 ) b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'NO' ) )          error stop 8_4
   msg1=''
   write (myunit3, iostat=stat1, iomsg=msg1, pos=1 )   b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'NO' ) )          error stop 9_4
   msg1=''

   rewind myunit1

   read  (myunit1, iostat=stat1, iomsg=msg1 )          b2
   if (( stat1 /= 0 ) .or. ( msg1 /= 'YES' ) )         error stop 10_4
   msg1=''
   read  (myunit2, iostat=stat1, iomsg=msg1,rec=100 )  b3
   if (( stat1 /= 0 ) .or. ( msg1 /= 'NO' ) )          error stop 11_4
   msg1=''
   read  (myunit3, iostat=stat1, iomsg=msg1,pos=1 )    b4
   if (( stat1 /= 0 ) .or. ( msg1 /= 'NO' ) )          error stop 12_4
   msg1=''

   if ( b2%c /= 'ibm' ) error stop 13_4
   if ( b3%c /= 'ibm' ) error stop 14_4
   if ( b4%c /= 'ibm' ) error stop 15_4

   ! check on the standard I/O (they should all be sequential access

   if ( isSequential(INPUT_UNIT)  /= 'YES' ) error stop 16_4
   if ( isSequential(ERROR_UNIT)  /= 'YES' ) error stop 17_4
   if ( isSequential(OUTPUT_UNIT) /= 'YES' ) error stop 18_4

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

   character(20) :: name1

   read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

   if ( iostat /= 0 ) error stop 19_4

   inquire ( unit, name=name1)
   iomsg = isSequential(name1)

end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()

   if ( iostat /= 0 ) error stop 20_4

   FLUSH (unit, iostat=iostat, iomsg=iomsg)

   iomsg = isSequential(unit)

end subroutine

character(10) function isSequential(fileorunit)
   class(*), intent(in) :: fileorunit
   integer :: stat

   select type ( fileorunit )
      type is (character(*))
         inquire ( file=fileorunit, sequential=isSequential, iostat=stat )
      type is (integer)
         inquire ( fileorunit, sequential=isSequential, iostat=stat )
      class default
         error stop 21_4
   end select

   if ( stat /= 0 ) error stop 22_4

end function