! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting for derived type object with module subroutine
!*                                        with non-poly pointer/allocatable dummy arguments (input)
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

contains

   subroutine readB1B2(unit, b1, b2, b3, b4)
      integer, intent(in) :: unit
      type(base),  intent(inout) :: b1
      type(child), intent(inout) :: b2
      type(base), pointer, intent(inout) :: b3
      type(child), allocatable, intent(inout) :: b4
      namelist /nml/ b1, b2, b3, b4

      integer :: stat
      character(200) :: msg

      read ( unit, nml, iostat=stat, iomsg = msg)
      if ( ( stat /= 0 )  .or. ( msg /= 'dtioread' )  ) error stop 1_4

   end subroutine

end module

program dummyArg108a
   use m
   type(base)  :: b1
   type(child) :: b2
   type(base), pointer      :: b3
   type(child), allocatable :: b4

   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'dummyArg108a.1', form='formatted', access='stream' )

   allocate(b3, source = base()  )
   allocate(b4, source = child() )

   call readB1B2( 1, b1, b2, b3, b4 )

   if ( ( b1%c /= 'abc' ) .or. ( b2%c /= 'def' ) .or. ( b2%i /= 1234 ))   error stop 1_4
   if ( ( b3%c /= 'ghi' ) .or. ( b4%c /= 'jkl' ) .or. ( b4%i /= 2345 ))   error stop 2_4

   call readB1B2(1, b3, b4, b3, b4 )

   if ( ( b3%c /= 'mno' ) .or. ( b4%i /= 3456 ).or. ( b4%c /= 'pqr' ) )   error stop 3_4

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
         read (unit, "(A3)", iostat=iostat )         dtv%c
      type is (child)
         read (unit, "(A3,1X,I4)", iostat=iostat )   dtv%c, dtv%i
   end select

   iomsg = 'dtioread'

end subroutine
