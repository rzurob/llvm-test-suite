!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: pad001.f
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
!*                               - PAD= specifier: Try using INQUIRE stmt with PAD= specifier in procedures
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

   procedure(character(10)) :: isPad

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


program pad001
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
   class(base), allocatable :: b1, b2
   integer :: stat1
   character(200) :: msg1

   integer, allocatable :: myUnit

   ! allocation of variables

   allocate (b1,b2)
   allocate (myunit, source=2)

   b1%c = 'ibm'
   b2%c = 'ftn'

   if ( isPad(myunit) /= 'aUNDEFINED' ) error stop 1_4

   open (myunit, file='pad001.data', form='unformatted', access='sequential' )

   if ( isPad(myunit) /= 'aUNDEFINED' ) error stop 2_4

   ! I/O operations

   write (myunit, iostat=stat1, iomsg=msg1 ) b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio write' ) ) error stop 3_4

   rewind(myunit)

   read  (myunit, iostat=stat1, iomsg=msg1 ) b2
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio read' ) ) error stop 4_4

   if ( b2%c /= 'ibm' ) error stop 5_4

   ! close the file appropriately

   close ( myunit, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

   if ( iostat /= 0 ) error stop 6_4

   if ( isPad(unit) /= 'aUNDEFINED' ) error stop 7_4

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

   if ( isPad(unit) /= 'aUNDEFINED' ) error stop 9_4

   iomsg = 'dtio write'

end subroutine

character(10) function isPad(unit)
   integer, intent(in) :: unit
   integer :: stat
   character(9) :: pad1
   inquire ( unit, pad=pad1, iostat=stat )

   if ( stat /= 0 ) error stop 10_4

   if ( pad1 .eq. 'YES' ) then
      isPad = 'aYES'
   else if ( pad1 .eq. 'NO' ) then
      isPad = 'aNO'
   else if ( pad1 .eq. 'UNDEFINED' ) then
      isPad = 'aUNDEFINED'
   else
      isPad = 'error'
   end if
end function
