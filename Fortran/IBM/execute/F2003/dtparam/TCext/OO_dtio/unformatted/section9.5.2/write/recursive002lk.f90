! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : recursive002lk
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2
!*                                        Try linked list data structure with class hierarchy and container with recursive DTIO
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

   type :: base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      class(base(:)), pointer :: next => null() ! tcx: (:)
      character(lbase_1) :: c
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i
   end type

   type :: linkedlist
      class(base(:)), pointer :: head ! tcx: (:)
   end type

   interface write(unformatted)
      subroutine writeunformattedll(dtv, unit, iostat, iomsg )
         import linkedlist
         class(linkedlist), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
      recursive subroutine writeunformatted(dtv, unit, iostat, iomsg )
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program recursive002lk
   use m

   integer :: stat

   integer(4) :: i2, i4, i5, i7
   character(3) :: c1, c2, c3, c4, c5, c6, c7

   character(200) :: msg = ''
   class(linkedlist), pointer :: ll

   allocate ( ll )
   allocate ( ll%head, source = base(3) (c='abc')) ! tcx: (3)
   allocate ( ll%head%next, source = child(3,4)(c='def',i=1001) ) ! tcx: (3,4)
   allocate ( ll%head%next%next, source = base(3) (c='ghi') ) ! tcx: (3)
   allocate ( ll%head%next%next%next, source = child(3,4) (c='jkl', i=1002) ) ! tcx: (3,4)
   allocate ( ll%head%next%next%next%next, source = child(3,4) (c='mno', i=1003) ) ! tcx: (3,4)

   open (1, file = 'recursive002lk.1', form='unformatted', access='stream' )

   write (1, iostat=stat, iomsg=msg)       ll
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 101_4

   deallocate ( ll%head%next%next%next%next )
   deallocate ( ll%head%next%next%next )
   deallocate ( ll%head%next%next )

   ll%head%next%next => null()

   write (1, iostat=stat, iomsg=msg)       ll
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   rewind 1

   read (1, iostat=stat, iomsg = msg)      c1, c2, i2, c3, c4, i4, c5, i5

   if ( ( c1 /= 'abc' ) .or. ( c2 /= 'def' ) .or. ( i2 /= 1001 ) .or. ( c3 /= 'ghi' ) &
   .or. ( c4 /= 'jkl' ) .or. ( i4 /= 1002 ) .or. ( c5 /= 'mno' ) .or. ( i5 /= 1003 )  )        error stop 3_4

   read (1, iostat=stat, iomsg = msg)      c6, c7, i7

   if ( ( c6 /= 'abc' ) .or. ( c7 /= 'def' ) .or. ( i7 /= 1001 ) )                             error stop 4_4

   close (1, status = 'delete' )

end program

subroutine writeunformattedll ( dtv, unit, iostat, iomsg )
   use m, only: linkedlist, writeunformatted, write(unformatted)

   class(linkedlist), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write(unit, iostat= iostat, iomsg = iomsg ) dtv%head
   if ( ( iostat /= 0 ) .or. ( iomsg /= 'itemwrite' ) ) error stop 5_4

   iomsg = 'dtiowrite'

end subroutine

recursive subroutine writeunformatted ( dtv, unit, iostat, iomsg )
   use m, only: base, child, write(unformatted)

   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is (base(*)) ! tcx: (*)
         write (unit, iostat=iostat )   dtv%c
      type is (child(*,4)) ! tcx: (*,4)
         write (unit, iostat=iostat )   dtv%c, dtv%i
   end select

   if ( iostat /= 0  ) error stop 6_4

   if ( associated(dtv%next) ) then
      write(unit, iostat= iostat, iomsg = iomsg ) dtv%next
      if ( ( iostat /= 0 ) .or. ( iomsg /= 'itemwrite' ) ) error stop 7_4
   end if

   iomsg = 'itemwrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 7 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 4 changes
