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
!*                                        Try linked list data structure with recursive DTIO with namelist formatting
!*                                        and namelist formatting inside DTIO with class hierarchy
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

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc) :: i
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

program recursive002kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(:)), pointer :: head ! tcx: (:)
   class(base(:)), allocatable, target :: b1, b2, b3, b4, b5, b6 ! tcx: (:)
   namelist /linkedlist/ head

   open (1, file = 'recursive002kl.1', form='formatted', access='stream' )

   allocate(b1, source = base(3) (c='abc')) ! tcx: (3)
   allocate(b2, source = child(3,4)(c='def',i=1001)) ! tcx: (3,4)
   allocate(b3, source = base(3) (c='ghi')) ! tcx: (3)
   allocate(b4, source = child(3,4)(c='jkl',i=1002)) ! tcx: (3,4)
   allocate(b5, source = base(3) (c='mno')) ! tcx: (3)
   allocate(b6, source = child(3,4)(c='pqr',i=1003)) ! tcx: (3,4)

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
   use m, only: base, child, write(formatted)

   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer,  intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer,  intent(out) :: iostat
   character(*),  intent(inout) :: iomsg

   character(3) :: c
   integer(4)   :: i
   class(base(:)), allocatable :: dummy ! tcx: (:)
   namelist /dtiobase/  c
   namelist /dtiochild/ c, i
   namelist /dtionext/ dummy

   if ( iotype /= "NAMELIST" ) error stop 4_4
   if ( size(v_list, 1) /= 0 ) error stop 5_4

   select type(dtv)
      type is (base(*)) ! tcx: (*)
         c=dtv%c
         write (unit, dtiobase, iostat=iostat )
      type is (child(*,4)) ! tcx: (*,4)
         c=dtv%c
         i=dtv%i
         write (unit, dtiochild, iostat=iostat )
   end select

   if ( iostat /= 0  ) error stop 6_4

   if ( associated(dtv%next) ) then
      allocate(dummy, source = dtv%next)
      write(unit, dtionext, iostat= iostat, iomsg = iomsg )
      if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowrite' ) ) error stop 7_4
   end if

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 10 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 4 changes
