! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with save attribute (output)
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

   type :: base
      integer(4) :: i
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

      class(base), allocatable, save :: dummy
      namelist /nml/ dummy
      integer :: stat
      character(200) :: msg

      if ( .not. allocated(dummy) ) then
      	 allocate(dummy, source = base(7) )
      else
      	 dummy%i = dummy%i + 1
      end if

      write ( unit, nml, iostat=stat, iomsg = msg)
      if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   end subroutine

end module

program save001
   use m

   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'save001.1', form='formatted', access='stream' )

   call writeDummy(1)
   call writeDummy(1)
   call writeDummy(1)
   call writeDummy(1)
   call writeDummy(1)

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   write (unit, "('i=',1X,I4)", iostat=iostat )              dtv%i

   iomsg = 'dtiowrite'

end subroutine

