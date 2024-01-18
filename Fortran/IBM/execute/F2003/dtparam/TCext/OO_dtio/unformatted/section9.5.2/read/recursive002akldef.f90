! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : recursive002akldef
!*
!*  DATE                       : 2007-09-17 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2
!*                                        Try linked list data structure with sequence type and container with recursive DTIO
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
      sequence
      type(base(:)), pointer :: next => null() ! tcx: (:)
      character(lbase_1) :: c = 'xxx'
   end type

   type :: linkedlist
      type(base(:)), pointer :: head ! tcx: (:)
   end type

   interface read(unformatted)
      subroutine readunformattedll(dtv, unit, iostat, iomsg )
         import linkedlist
         class(linkedlist), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
      recursive subroutine readunformatted(dtv, unit, iostat, iomsg )
         import base
         type(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program recursive002akldef
   use m

   integer :: stat

   character(200) :: msg = ''
   class(linkedlist), pointer :: ll
   type(base(:)), pointer :: dummy ! tcx: (:)

   allocate ( ll )
   allocate ( ll%head, source = base(3)()) ! tcx: (3)
   allocate ( ll%head%next, source = base(3)() ) ! tcx: (3)
   allocate ( ll%head%next%next, source = base(3)() ) ! tcx: (3)
   allocate ( ll%head%next%next%next, source = base(3)() ) ! tcx: (3)
   allocate ( ll%head%next%next%next%next, source = base(3) () ) ! tcx: (3)

   open (1, file = 'recursive002akldef.1', form='unformatted', access='sequential' )

   write (1, iostat=stat, iomsg = msg)      'abc', 'def', 'ghi', 'jkl', 'mno'
   write (1, iostat=stat, iomsg = msg)      'pqr', 'stu'

   rewind 1

   read (1, iostat=stat, iomsg=msg)       ll
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 101_4

   dummy => ll%head
   do while (associated(dummy))
      print *, "BASE:  ", dummy%c
      dummy => dummy%next
   end do

   deallocate ( ll%head%next%next%next%next )
   deallocate ( ll%head%next%next%next )
   deallocate ( ll%head%next%next )

   ll%head%next%next => null()

   read (1, iostat=stat, iomsg=msg)       ll
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   dummy => ll%head
   do while (associated(dummy))
      print *, "BASE:  ", dummy%c
      dummy => dummy%next
   end do

  close (1, status = 'delete' )

end program

subroutine readunformattedll ( dtv, unit, iostat, iomsg )
   use m, only: linkedlist, readunformatted, read(unformatted)

   class(linkedlist), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read(unit, iostat= iostat, iomsg = iomsg ) dtv%head
   if ( ( iostat /= 0 ) .or. ( iomsg /= 'itemread' ) ) error stop 5_4

   iomsg = 'dtioread'

end subroutine

recursive subroutine readunformatted ( dtv, unit, iostat, iomsg )
   use m, only: base, read(unformatted)

   type(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iostat=iostat )   dtv%c

   if ( iostat /= 0  ) error stop 6_4

   if ( associated(dtv%next) ) then
      read(unit, iostat= iostat, iomsg = iomsg ) dtv%next
      if ( ( iostat /= 0 ) .or. ( iomsg /= 'itemread' ) ) error stop 7_4
   end if

   iomsg = 'itemread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 10 changes
