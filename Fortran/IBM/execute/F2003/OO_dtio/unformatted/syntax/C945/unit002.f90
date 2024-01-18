!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp unit002.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.8: FLUSH statement
!*                               - Try FLUSH statement on internal file inside dtio
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


program unit002
   use m1

   interface read(formatted)
      subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface write(formatted)
      subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(base), allocatable :: b1, b2
   integer :: stat1, stat2
   character(200) :: msg1, msg2

   character(4) :: internalFile(10)
   ! allocation of variables

   allocate (b1,b2)

   b1%c = 'ibm'
   b2%c = ''

   ! I/O operations

   write (internalFile(1),*, iostat=stat1, iomsg = msg1)    b1            !<- inside DTIO, it will flush internalfile

   print *, stat1, msg1

   rewind 1

   read (internalFile(1), *, iostat=stat1, iomsg = msg1)    b2

   print *, stat1, msg1

   if ( b2%c /= 'ibm' ) error stop 3_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1
    class(base), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)

    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    read (unit, *, iostat=iostat, iomsg=iomsg ) dtv%c

    if ( iostat /= 0 ) error stop 5_4

    FLUSH (unit, iostat=iostat, iomsg = iomsg)

end subroutine


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, *, iostat=iostat, iomsg=iomsg ) dtv%getC()

    if ( iostat /= 0 ) error stop 5_4

    FLUSH (unit, iostat=iostat, iomsg=iomsg)

end subroutine
