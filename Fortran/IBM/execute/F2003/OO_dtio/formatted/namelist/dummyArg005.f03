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
   end type

   type, extends(base) :: child
      integer(4)   ::  i
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

   class(base), pointer :: b2

contains

   subroutine writeBase(dtv, unit)
      class(base), intent(in) :: dtv
      integer, intent(in) :: unit

      integer :: stat
      character(200) :: msg

      call internalWriteBase(unit, dtv)

      contains

      subroutine internalWriteBase(unit, dtv)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         namelist /nml/ dtv
         write ( unit, nml, iostat=stat, iomsg = msg)
         if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
      end subroutine

   end subroutine

end module

program dummyArg005
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1
   type(base)               :: b3
   type(base), pointer      :: b4

   open (1, file = 'dummyArg005.1', form='formatted', access='stream' )

   allocate(b1, source = child(c='abc',i=1) )
   allocate(b2, source = child(c='ghi',i=3) )
   b3 = base(c='mno')
   allocate(b4, source = b3 )

   call writeBase(b1,1)
   call writeBase(b2,1)
   call writeBase(b3,1)
   call writeBase(b4,1)

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   select type (dtv)
      type is (base)
         write (unit, "('c= ',A3,1X)", iostat=iostat )        dtv%c
      type is (child)
         write (unit, "('i= ',I4, 1X,'c= ',A3,1X)", iostat=iostat )        dtv%i, dtv%c
   end select

   iomsg = 'dtiowrite'

end subroutine

