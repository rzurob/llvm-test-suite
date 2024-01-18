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
! %GROUP: recursive002a.f
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

   type :: base
      sequence
      type(base), pointer :: next => null()
      character(3) :: c
   end type

   type :: linkedlist
      type(base), pointer :: head
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
         type(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program recursive002a
   use m

   integer :: stat

   character(3) :: c1, c2, c3, c4, c5, c6, c7

   character(200) :: msg = ''
   class(linkedlist), pointer :: ll

   allocate ( ll )
   allocate ( ll%head, source = base (c='abc'))
   allocate ( ll%head%next, source = base(c='def') )
   allocate ( ll%head%next%next, source = base (c='ghi') )
   allocate ( ll%head%next%next%next, source = base (c='jkl') )
   allocate ( ll%head%next%next%next%next, source = base (c='mno') )

   open (1, file = 'recursive002a.1', form='unformatted', access='stream' )

   write (1, iostat=stat, iomsg=msg)       ll
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) )    error stop 1_4

   deallocate ( ll%head%next%next%next%next )
   deallocate ( ll%head%next%next%next )
   deallocate ( ll%head%next%next )

   ll%head%next%next => null()

   write (1, iostat=stat, iomsg=msg)       ll
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) )    error stop 2_4

   rewind 1

   read (1, iostat=stat, iomsg = msg)      c1, c2, c3, c4, c5

   if ( ( c1 /= 'abc' ) .or. ( c2 /= 'def' ) .or. ( c3 /= 'ghi' ) &
   .or. ( c4 /= 'jkl' ) .or. ( c5 /= 'mno' )  )        error stop 3_4

   read (1, iostat=stat, iomsg = msg)      c6, c7

   if ( ( c6 /= 'abc' ) .or. ( c7 /= 'def' )  )        error stop 4_4

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
   use m, only: base, write(unformatted)

   type(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iostat=iostat )   dtv%c

   if ( iostat /= 0  ) error stop 6_4

   if ( associated(dtv%next) ) then
      write(unit, iostat= iostat, iomsg = iomsg ) dtv%next
      if ( ( iostat /= 0 ) .or. ( iomsg /= 'itemwrite' ) ) error stop 7_4
   end if

   iomsg = 'itemwrite'

end subroutine
