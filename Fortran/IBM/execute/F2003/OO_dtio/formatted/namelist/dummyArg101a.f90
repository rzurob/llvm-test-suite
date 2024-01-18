! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting for derived type object which is a explicit array dummy argument
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

   integer :: unit = 1

contains

   subroutine readBase(dtv)
      class(base), intent(inout) :: dtv(3)
      integer :: stat
      character(200) :: msg

      namelist /nml/ dtv
      read ( unit, nml, iostat=stat, iomsg = msg)
      if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   end subroutine

end module

program dummyArg101a
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1(:)
   class(base), pointer     :: b2(:)
   type(base)               :: b3(3)
   type(child), pointer     :: b4(:)

   open (unit, file = 'dummyArg101a.1', form='formatted', access='stream' )

   allocate( b1(3) )
   allocate( child :: b2(3) )
   allocate(b4(3) )

   call readBase(b1)
   call readBase(b2)
   call readBase(b3)
   call readBase(b4)

   if ( ( b1(1)%c /= 'abc' ) .or. ( b1(2)%c /= 'def' ) .or. ( b1(3)%c /= 'ghi' ) )    error stop 2_4

   select type ( b2 )
      type is (child)
         if ( ( b2(1)%c /= 'ABC' ) .or. ( b2(2)%c /= 'DEF' ) .or. ( b2(3)%c /= 'GHI' ) .or. &
              ( b2(1)%I /= 2001 ) .or. ( b2(2)%I /= 2002 ) .or. ( b2(3)%I /= 2003 ) ) error stop 3_4
   end select

   if ( ( b3(1)%c /= 'abc' ) .or. ( b3(2)%c /= 'xxx' ) .or. ( b3(3)%c /= 'ghi' ) )    error stop 4_4
   if ( ( b4(1)%c /= 'ABC' ) .or. ( b4(2)%c /= 'xxx' ) .or. ( b4(3)%c /= 'GHI' ) .or. &
        ( b4(1)%I /= 4001 ) .or. ( b4(2)%I /= -999 ) .or. ( b4(3)%I /= 4003 ) )       error stop 5_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   select type ( dtv )
      type is ( base )
         read (unit, "(A3,1X)", iostat=iostat )        dtv%c
      type is ( child )
         read (unit, "(I4,1X,A3,1X)", iostat=iostat )        dtv%i, dtv%c
   end select

   iomsg = 'dtioread'

end subroutine

