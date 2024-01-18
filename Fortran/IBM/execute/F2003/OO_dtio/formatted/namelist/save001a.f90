!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: save001a.f
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
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with save attribute
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

   type :: data
      integer(4) :: i
   end type

   type :: base
      class(data), allocatable :: d
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

contains

   subroutine writeDummy(unit)
      integer, intent(in) :: unit

      class(base), pointer, save :: dummy
      namelist /nml/ dummy
      integer :: stat
      character(200) :: msg

      if ( .not. associated(dummy) ) then
      	 allocate(dummy, source = base(data(10)) )
      else
      	 dummy%d%i = dummy%d%i + 1
      end if

      write ( unit, nml, iostat=stat, iomsg = msg)
      if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   end subroutine

end module

program save001a
   use m

   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'save001a.1', form='formatted', access='stream' )

   call writeDummy(1)
   call writeDummy(1)
   call writeDummy(1)
   call writeDummy(1)
   call writeDummy(1)

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

   write (unit,*, iostat=iostat, iomsg=iomsg )              dtv%d

   if ( ( iostat /= 0 ) .or. ( iomsg /= 'datawrite' ) ) error stop 1_4

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

   write (unit, "(1X,I4)", iostat=iostat )              dtv%i

   iomsg = 'datawrite'

end subroutine
