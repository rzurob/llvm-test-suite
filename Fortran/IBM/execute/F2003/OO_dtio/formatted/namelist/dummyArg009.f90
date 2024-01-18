! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting for derived type object with type bound procedures
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
      character(3) ::  c
      contains
         procedure, pass :: writeme => writebase
   end type

   type, extends(base) :: child
      integer(4)   ::  i
      contains
         procedure, pass :: writeme => writechild
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

   subroutine writebase(b, unit)
      class(base), intent(in) :: b
      integer, intent(in) :: unit
      namelist /basenml/ b
      integer :: stat
      character(200) :: msg

      write (unit, basenml, iostat=stat, iomsg = msg)
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   end subroutine

   subroutine writechild(b, unit)
      class(child), intent(in) :: b
      integer, intent(in) :: unit
      namelist /childnml/ b
      integer :: stat
      character(200) :: msg

      write (unit, childnml, iostat=stat, iomsg = msg)
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   end subroutine
end module

program dummyArg009
   use m
   class(base), pointer       :: b1
   class(base), allocatable   :: b2
   type(base)                 :: b3
   class(child), allocatable  :: b4
   class(child), pointer      :: b5

   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'dummyArg009.1', form='formatted', access='stream' )

   allocate(b1, source = base(c='abc')      )
   allocate(b2, source = child(c='def',i=2) )
   b3 = base('ghi')
   allocate(b4, source = child('jkl', 4) )
   allocate(b5, source = child('mno', 5) )

   call b1%writeme(1)
   call b2%writeme(1)
   call b3%writeme(1)
   call b4%writeme(1)
   call b5%writeme(1)

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   select type (dtv)
      type is (base)
         write (unit, "('c= ',A3,1X)", iostat=iostat )        dtv%c
      type is (child)
         write (unit, "('i= ',I4, 1X,'c= ',A3,1X)", iostat=iostat )        dtv%i, dtv%c
   end select

   iomsg = 'dtiowrite'

end subroutine
