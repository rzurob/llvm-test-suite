!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: recursive002.f
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

   type :: base
      class(base), pointer :: next => null()
      character(3) :: c
   end type

   type, extends(base) :: child
      integer(4) :: i
   end type

   type :: linkedlist
      class(base), pointer :: head
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
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program recursive002
   use m

   integer :: stat

   integer(4) :: i2, i4, i5, i7
   character(3) :: c1, c2, c3, c4, c5, c6, c7

   character(200) :: msg = ''
   class(linkedlist), pointer :: ll

   allocate ( ll )
   allocate ( ll%head, source = base (c='abc'))
   allocate ( ll%head%next, source = child(c='def',i=1001) )
   allocate ( ll%head%next%next, source = base (c='ghi') )
   allocate ( ll%head%next%next%next, source = child (c='jkl', i=1002) )
   allocate ( ll%head%next%next%next%next, source = child (c='mno', i=1003) )

   open (1, file = 'recursive002.1', form='unformatted', access='stream' )

   write (1, iostat=stat, iomsg=msg)       ll
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

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

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is (base)
         write (unit, iostat=iostat )   dtv%c
      type is (child)
         write (unit, iostat=iostat )   dtv%c, dtv%i
   end select

   if ( iostat /= 0  ) error stop 6_4

   if ( associated(dtv%next) ) then
      write(unit, iostat= iostat, iomsg = iomsg ) dtv%next
      if ( ( iostat /= 0 ) .or. ( iomsg /= 'itemwrite' ) ) error stop 7_4
   end if

   iomsg = 'itemwrite'

end subroutine
