! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Secition 9.9 INQUIRE Statement
!*                               - RECL= specifier: Try using INQUIRE stmt with RECL= specifier in procedures
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
      integer(4) function getrecl(fileorunit)
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


program recl001
   use m1

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
   class(base), allocatable :: b1, b2, b3
   integer :: stat1
   character(200) :: msg1

   integer, allocatable :: myunit1
   integer, allocatable :: myunit2
   integer, allocatable :: myunit3

   ! set runtime env variable for both 32-bit and 64-bit (required in 64-bit)

   call setrteopts("default_recl=32")

   ! allocation of variables

   allocate (b1,b2,b3)
   allocate (myunit1, source=1)
   allocate (myunit2, source=2)
   allocate (myunit3, source=3)

   b1%c = 'ibm'

   open (myunit1, file='recl001.1', form='unformatted', access='sequential'     )
   open (myunit2, file='recl001.2', form='unformatted', access='direct', recl=3 )

   if ( getRecl(myunit1) /= 2147483639 )          error stop 1_4
   if ( getRecl('recl001.2') /= 3 )               error stop 2_4

   ! I/O operations

   write (myunit1, iostat=stat1, iomsg=msg1 ) b1
   if (( stat1 /= 2147483639 ) .or. ( msg1 /= 'dtio write' ) ) error stop 3_4

   write (myunit2, iostat=stat1, iomsg=msg1, rec=100 ) b1
   if (( stat1 /= 3 ) .or. ( msg1 /= 'dtio write' ) ) error stop 4_4

   rewind 1

   read  (myunit1, iostat=stat1, iomsg=msg1 ) b2
   if (( stat1 /= 2147483639 ) .or. ( msg1 /= 'dtio read' ) ) error stop 5_4

   read  (myunit2, iostat=stat1, iomsg=msg1,rec=100 ) b3
   if (( stat1 /= 3 ) .or. ( msg1 /= 'dtio read' ) ) error stop 5_4

   if ( b2%c /= 'ibm' ) error stop 6_4

   ! close the file appropriately

   close ( myunit1, status ='delete' )
   close ( myunit2, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(20) :: name1

   read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

   if ( iostat /= 0 ) error stop 6_4

   inquire ( unit, name=name1)
   iostat = getRecl(name1)
   iomsg = 'dtio read'

end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()

   if ( iostat /= 0 ) error stop 8_4

   FLUSH (unit, iostat=iostat, iomsg=iomsg)

   iostat = getRecl(unit)

   iomsg = 'dtio write'

end subroutine

integer(4) function getRecl(fileorunit)
   class(*), intent(in) :: fileorunit
   integer :: stat

   select type ( fileorunit )
      type is (character(*))
         inquire ( file=fileorunit, recl=getRecl, iostat=stat )
      type is (integer)
         inquire ( fileorunit, recl=getRecl, iostat=stat )
      class default
         error stop 22_4
   end select

   if ( stat /= 0 ) error stop 23_4
end function