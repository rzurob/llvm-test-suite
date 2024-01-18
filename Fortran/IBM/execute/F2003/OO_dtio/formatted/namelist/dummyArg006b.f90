! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting for derived type object with module subroutine (Host Association)
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

   class(base), pointer :: b1

   namelist /nmlb1b2/ b1

contains

   subroutine writeB1B2(unit, b2)
      integer, intent(in) :: unit
      class(base), intent(in) :: b2

      namelist /nmlb1b2/ b1, b2     !<- only b1, b2 will be written to file

      integer :: stat
      character(200) :: msg
      write ( unit, nmlb1b2, iostat=stat, iomsg = msg)
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   end subroutine

end module

program dummyArg006b
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b2
   type(base) :: b3   = base('IBM')
   open (1, file = 'dummyArg006b.1', form='formatted', access='sequential' )

   allocate(b1, source = base (c='abc')     )
   allocate(b2, source = child(c='ghi',i=3) )

   call writeB1B2(1, b2)
   call writeB1B2(1, b3)

   write ( 1, nmlb1b2, iostat=stat, iomsg = msg)  !<- only writes b1

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
