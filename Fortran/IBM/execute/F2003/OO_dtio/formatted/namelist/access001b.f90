!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: access001b.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with private polymorphic component (output)
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

   type data
      integer(4), private :: i
   contains
      procedure, pass :: get => getdata
   end type

   type base
      class(data), allocatable, private :: d
   contains
      procedure, pass :: set => setbase
      procedure, pass :: get => getbase
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base), allocatable :: b1
   class(base), pointer     :: b2
   type(base)               :: b3
   integer :: stat
   character(150) :: msg
   namelist /n123/ b1, b2, b3

   contains

   subroutine setbase(dtv, i)
      class(base), intent(inout) :: dtv
      dtv%d%i = i
   end subroutine

   function getbase(dtv)
      class(base), intent(in) :: dtv
      class(data), allocatable :: getbase
      allocate (getbase, source = dtv%d)
   end function

   integer function getdata(dtv)
      class(data), intent(in) :: dtv
      getdata = dtv%i
   end function
   
   subroutine start()
      allocate ( b1, source = base( d=data(101)))
      allocate ( b2, source = base( d=data(102)))
      allocate ( b3%d, source = data(103) )
   end subroutine
   
   subroutine write(unit)
      integer, intent(in) :: unit
      write (unit, n123, iostat = stat, iomsg = msg )
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   end subroutine

end module

program access001b
use m

   open (1, file = 'access001b.1', form='formatted', access='sequential' )
   call start()
   
   call write (1)
   
end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, data

   interface write(formatted)
      subroutine writeformatteddata(dtv, unit, iotype, v_list, iostat, iomsg )
         import data
         class(data), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   write (unit, *, iostat=iostat, iomsg = iomsg )   dtv%get()
   
   if ( ( iostat /= 0 ) .or. ( iomsg /= 'datawrite' ) ) error stop 4_4

   iomsg = 'dtiowrite'

end subroutine

subroutine writeformatteddata (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: data

   class(data), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "LISTDIRECTED" ) error stop 5_4
   if ( size(v_list, 1) /= 0 )     error stop 6_4

   write (unit, *, iostat=iostat )   dtv%get()

   iomsg = 'datawrite'

end subroutine
