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
! %GROUP: recursive003.f
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
!*                                        Try linked list data structure with recursive DTIO with namelist formatting
!*                                        and namelist formatting inside DTIO with class hierarchy with unlimited polymorphic
!*                                        pointer
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
      class(*), pointer :: next => null()
      character(3) :: c = 'xxx'
   end type

   type, extends(base) :: child
      integer(4) :: i
   end type

   interface write(formatted)
      recursive subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program recursive003
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), pointer :: head
   class(base), allocatable, target :: b1, b2
   type(base), target :: b3
   type(child), target :: b4
   class(child), pointer :: b5, b6
   
   character(3), target :: ll1end = 'end'
   integer, target :: ll2end = 99999
   
   namelist /linkedlist/ head

   open (1, file = 'recursive003.1', form='formatted', access='sequential' )

   allocate(b1, source = base (c='abc'))
   allocate(b2, source = child(c='def',i=1001))
   b3 = base (c='ghi')
   b4 = child(c='jkl',i=1002)
   allocate(b5, source = child (c='mno', i=1003))
   allocate(b6, source = child(c='pqr',i=1004))

   ! first linked list
   b1%next => b2
   b2%next => b3
   b3%next => ll1end

   ! second linked list
   b4%next => b5
   b5%next => b6
   b6%next => ll2end

   head => b1

   write (1, linkedlist, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   head => b4

   write (1, linkedlist, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

end program


recursive subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child, write(formatted)

   class(base), intent(in) :: dtv
   integer,  intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer,  intent(out) :: iostat
   character(*),  intent(inout) :: iomsg

   character(3) :: c
   integer(4)   :: i
   class(base), allocatable :: dummy
   namelist /dtiobase/  c
   namelist /dtiochild/ c, i
   namelist /dtionext/ dummy

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   select type(dtv)
      type is (base)
         c=dtv%c
         write (unit, dtiobase, iostat=iostat )
      type is (child)
         c=dtv%c
         i=dtv%i
         write (unit, dtiochild, iostat=iostat )
   end select

   if ( iostat /= 0  ) error stop 5_4

   if ( associated(dtv%next) ) then
      select type ( g => dtv%next )
         class is (base)
            allocate(dummy, source = g)
            write(unit, dtionext, iostat= iostat, iomsg = iomsg )
            if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowrite' ) ) error stop 6_4
         type is (character(*))
            write(unit, *, iostat= iostat, iomsg = iomsg ) g
         type is (integer)
            write(unit, *, iostat= iostat, iomsg = iomsg ) g
         end select
   end if

   iomsg = 'dtiowrite'

end subroutine
