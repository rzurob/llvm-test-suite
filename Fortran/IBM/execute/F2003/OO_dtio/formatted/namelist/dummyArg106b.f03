! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting for derived type object with module subroutine (Use and Host Association)
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
      character(3) ::  c = 'xxx'
   end type

   type, extends(base) :: child
      integer(4)   ::  i = -999
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base), pointer :: b1

   namelist /nmlb1b2/ b1

contains

   subroutine readB1B2(unit, b2)
      integer, intent(in) :: unit
      class(base), intent(inout) :: b2

      namelist /nmlb1b2/ b1, b2     !<- only b1, b2 will be written to file

      integer :: stat
      character(200) :: msg
      read ( unit, nmlb1b2, iostat=stat, iomsg = msg)
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   end subroutine

end module

program dummyArg106b
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b2
   type(base) :: b3   = base('IBM')
   open (1, file = 'dummyArg106b.1', form='formatted', access='sequential' )

   allocate(b1, source = base () )
   allocate(b2, source = child() )

   call readB1B2(1, b2)
   select type(b2)
      type is (child)
         print *, b1%c, b2%c, b2%i
         if ( (b1%c /='abc') .or. (b2%c /= 'def') .or. (b2%i /= 1234) ) error stop 2_4
      class default
         error stop 3_4
   end select

   allocate(b1, source = child() )
   call readB1B2(1, b3)

   select type(b1)
      type is (child)
         if ( (b1%c /='ABC') .or. (b1%i /= 2345) .or. (b3%c /= 'DEF') ) error stop 4_4
      class default
         error stop 5_4
   end select

   read ( 1, nmlb1b2, iostat=stat, iomsg = msg)  !<- only reads b1
   select type(b1)
      type is (child)
         if ( (b1%c /='xyz') .or. (b1%i /= 7777) ) error stop 6_4
      class default
         error stop 7_4
   end select

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 8_4
   if ( size(v_list, 1) /= 0 ) error stop 9_4

   select type (dtv)
      type is (base)
         read (unit, *, iostat=iostat )                     dtv%c
      type is (child)
         read (unit, *, iostat=iostat )        dtv%i, dtv%c
   end select

   iomsg = 'dtioread'

end subroutine
