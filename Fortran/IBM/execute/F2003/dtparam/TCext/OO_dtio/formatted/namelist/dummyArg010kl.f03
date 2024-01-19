! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-05 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with dummy argument with VALUE attr(Output)
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
   type base (kb)
      integer, kind :: kb
      integer(kb) :: i
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(in) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface
contains
   subroutine valueWrite ( unit, b1, b2, b3 )
      integer, intent(in) :: unit
      type(base(4)), intent(in), value :: b1 ! tcx: (4)
      type(base(4)), intent(in), value :: b2 ! tcx: (4)
      type(base(4)), intent(in), value :: b3 ! tcx: (4)

      integer :: stat
      character(150) :: msg

      namelist /polymorphic/ b1, b2, b3

      write (unit, polymorphic, iostat = stat, iomsg = msg)

      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   end subroutine

   integer function valueWrite1 ( unit, b1, b2, b3 )
      integer, intent(in) :: unit
      type(base(4)), intent(in), value   :: b1 ! tcx: (4)
      type(base(4)), intent(in), value   :: b2 ! tcx: (4)
      type(base(4)), intent(in), value   :: b3 ! tcx: (4)

      character(150) :: msg

      namelist /nonpolymorphic/ b1, b2, b3

      write (unit, nonpolymorphic, iostat = valueWrite1, iomsg = msg)
      if ( ( valueWrite1 /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   end function

end module

program dummyArg010kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(4)), allocatable :: b1 ! tcx: (4)
   class(base(4)), pointer     :: b2 ! tcx: (4)
   type(base(4))               :: b3 ! tcx: (4)
   type(base(4)), allocatable  :: b4 ! tcx: (4)
   type(base(4)), pointer      :: b5 ! tcx: (4)

   open (1, file = 'dummyArg010kl.1', form='formatted', access='sequential' )
   allocate(b1, b2, b4, b5)

   b1%i = 2
   b2%i = 4
   b3%i = 6
   b4%i = 8
   b5%i = 10

   call valueWrite(1, b1, b2, b1)   !<- this writes 2,4,2 to file
   call valueWrite(1, b2, b5, b4)   !<- this writes 4,10,8 to file
   call valueWrite(1, b3, b1, b2)   !<- this writes 6,2,4  to file

   if ( valueWrite1 ( 1, b3, b5, b4 ) /= 0 ) error stop 3_4   !<- writes 6, 10, 8
   if ( valueWrite1 ( 1, b4, b5, b4 ) /= 0 ) error stop 4_4   !<- writes 8, 10, 8
   if ( valueWrite1 ( 1, b5, b5, b4 ) /= 0 ) error stop 5_4   !<- writes 10, 10, 8

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base(4)), intent(in) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 6_4
   if ( size(v_list, 1) /= 0 ) error stop 7_4

   write (unit, "('i=',I4,1X)", iostat=iostat )      dtv%i

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 13 changes
