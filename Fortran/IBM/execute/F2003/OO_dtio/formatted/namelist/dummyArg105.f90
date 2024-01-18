! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting for derived type object with internal subroutine (Host Association)
!*                                        on input statement
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

   class(base), pointer :: b2

contains

   subroutine readBase(dtv, unit)
      class(base), intent(inout) :: dtv
      integer, intent(in) :: unit

      integer :: stat
      character(200) :: msg

      namelist /nml/ dtv

      call internalreadBase(unit, dtv)

      contains

      subroutine internalreadBase(unit, dtv)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         read ( unit, nml, iostat=stat, iomsg = msg)
         if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
      end subroutine

   end subroutine

end module

program dummyArg105
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1
   type(base)               :: b3
   type(base), pointer      :: b4

   open (1, file = 'dummyArg105.1', form='formatted', access='stream' )

   allocate(child:: b1,b2 )
   b3 = base()
   allocate(b4)

   call readBase(b1,1)
   call readBase(b2,1)
   call readBase(b3,1)
   call readBase(b4,1)

   select type (b1)
      type is (child)
         if ( ( b1%i /= 123 ) .or. ( b1%c /= 'abc' ) ) error stop 2_4
      class default
         error stop 3_4
   end select
   select type (b2)
      type is (child)
         if ( ( b2%i /= 234 ) .or. ( b2%c /= 'def' ) ) error stop 4_4
      class default
         error stop 5_4
   end select
   if ( b3%c /= 'ghi' ) error stop 6_4
   if ( b4%c /= 'jkl' ) error stop 7_4

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
         read (unit, "(A3)", iostat=iostat )        dtv%c
      type is (child)
         read (unit, "(I3,1X,A3)", iostat=iostat )  dtv%i, dtv%c
   end select

   iomsg = 'dtioread'

end subroutine

