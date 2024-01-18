! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-06 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2
!*                                        Try linked list data structure with recursive DTIO with namelist formatting and namelist formatting inside DTIO
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

   type :: base (lb)
      integer, len :: lb
      class(base(:)), pointer :: next => null() ! tcx: (:)
      character(lb) :: c = 'xxx'
   end type

   interface write(formatted)
      recursive subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface
end module

program recursive001akl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(:)), pointer :: head ! tcx: (:)
   class(base(:)), allocatable, target :: b1, b2, b3, b4, b5, b6 ! tcx: (:)
   namelist /linkedlist/ head

   open (1, file = 'recursive001akl.1', form='formatted', access='stream' )

   allocate(b1, source = base(3)(c='abc')) ! tcx: (3)
   allocate(b2, source = base(3)(c='def')) ! tcx: (3)
   allocate(b3, source = base(3)(c='ghi')) ! tcx: (3)
   allocate(b4, source = base(3)(c='jkl')) ! tcx: (3)
   allocate(b5, source = base(3)(c='mno')) ! tcx: (3)
   allocate(b6, source = base(3)(c='pqr')) ! tcx: (3)

   ! first linked list
   b1%next => b2
   b2%next => b3

   ! second linked list
   b4%next => b5
   b5%next => b6

   head => b1

   write (1, linkedlist, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   head => b4

   write (1, linkedlist, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   head => b5

   write (1, linkedlist, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

end program


recursive subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, write(formatted)

   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer,  intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer,  intent(out) :: iostat
   character(*),  intent(inout) :: iomsg

   character(3) :: c
   type(base(3)) :: dummy ! tcx: (3)
   namelist /dtio1/ c
   namelist /dtio2/ dummy

   c = dtv%c

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   write (unit, dtio1, iostat=iostat )
   if ( iostat /= 0  ) error stop 6_4

   if ( associated(dtv%next) ) then
      dummy = dtv%next
      write(unit, dtio2, iostat= iostat, iomsg = iomsg )
      if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowrite' ) ) error stop 7_4
   end if

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 12 changes
