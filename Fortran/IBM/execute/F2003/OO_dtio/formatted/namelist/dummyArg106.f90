! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting for derived type object with module subroutine with input statement(Host Association)
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
   class(base), allocatable :: b2

   namelist /nmlb1b2/ b1, b2

contains

   subroutine readB1B2(unit)
      integer, intent(in) :: unit

      integer :: stat
      character(200) :: msg

      read ( unit, nmlb1b2, iostat=stat, iomsg = msg)

   end subroutine

end module

program dummyArg106
   use m

   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'dummyArg106.1', form='formatted', access='stream' )

   allocate(b1, source = base () )
   allocate(b2, source = child() )

   call readB1B2(1)

   if ( b1%c /= 'abc' )                            error stop 1_4
   select type (b2)
      type is (child)
         if ( ( b2%c /= 'def' ) .or. ( b2%i /= 1234 ) )  error stop 2_4
      class default
          error stop 3_4
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

   if ( iotype /= "NAMELIST" ) error stop 4_4
   if ( size(v_list, 1) /= 0 ) error stop 5_4

   select type (dtv)
      type is (base)
         read (unit, "(A3)", iostat=iostat )        dtv%c
      type is (child)
         read (unit, "(I4,1X,A3)", iostat=iostat )        dtv%i, dtv%c
   end select

   iomsg = 'dtioread'

end subroutine

