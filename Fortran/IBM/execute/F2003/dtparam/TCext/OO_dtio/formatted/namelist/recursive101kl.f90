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
      integer, len :: lb ! lb=3
      class(base(:)), pointer :: next => null() ! tcx: (:)
      character(lb) :: c = 'xxx'
   end type

   interface read(formatted)
      recursive subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program recursive101kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(:)), pointer :: head, dummy ! tcx: (:)
   class(base(:)), allocatable, target :: b1, b2 ! tcx: (:)
   namelist /linkedlist/ head

   open (1, file = 'recursive101kl.1', form='formatted', access='stream' )

   allocate(base(3):: b1,b1%next,b1%next%next) ! tcx: base(3)
   allocate(base(3):: b2,b2%next,b2%next%next,b2%next%next%next) ! tcx: base(3)

   head => b1

   read (1, linkedlist, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   dummy => head
   do while (associated(dummy))
      print *, dummy%c
      dummy => dummy%next
   end do

   head => b2

   read (1, linkedlist, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   dummy => head
   do while (associated(dummy))
      print *, dummy%c
      dummy => dummy%next
   end do

   ! combine two linked list together
   b1%next%next%next => b2

   head => b1

   read (1, linkedlist, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   dummy => head
   do while (associated(dummy))
      print *, dummy%c
      dummy => dummy%next
   end do

end program


recursive subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, read(formatted)

   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer,  intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer,  intent(out) :: iostat
   character(*),  intent(inout) :: iomsg

   read (unit, *, iostat=iostat )   dtv%c
   if ( iostat /= 0  ) error stop 4_4

   if ( associated(dtv%next) ) then
      read(unit, *, iostat= iostat, iomsg = iomsg ) dtv%next
      if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioread' ) ) error stop 5_4
   end if

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 5 changes
