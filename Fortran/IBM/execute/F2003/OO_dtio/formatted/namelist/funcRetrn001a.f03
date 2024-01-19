! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with polymorphic function return variable
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
   type base
      character(3) :: c
      contains
         procedure, pass :: write
   end type

   type, extends(base) :: child
      integer(4) :: i
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

   integer :: unit = 1
   integer :: stat
   character(200) :: msg

contains

   class(base) function write(dtv)
      class(base), intent(in) :: dtv
      allocatable :: write
      namelist /W/ write

      allocate ( write, source = dtv )
      write (unit, W, iostat = stat, iomsg = msg )

   end function

end module

program funcRetrn001a
   use m

   class(base), allocatable :: b1
   type(base)               :: b2 = base  ('IBM')
   type(child)              :: c1 = child ('FTN',123)
   class(child), pointer    :: c2

   open (1, file = 'funcRetrn001a.1', form='formatted', access='sequential' )

   allocate ( b1, source = b2%write()  ) !<- writes b2

   associate( gg => c1%write() )         !<- writes c1
      select type ( gg )
         class is (child)
            allocate ( c2, source = gg )
      end select
   end associate

   deallocate (b1)
   allocate(b1, source = child('IBM',2005))

   select type ( g => b1 )
      type is (child)
         select type ( gg => g%write() )
            type is (child)
               c1 = gg
         end select
   end select

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base,child

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 1_4
   if ( size(v_list, 1) /= 0 ) error stop 2_4

   select type (dtv)
      type is (base)
         write (unit, "('c=',A3)", iostat=iostat )                 dtv%c
      type is (child)
         write (unit, "('i=',I4,1X,'c=',A3)", iostat=iostat )      dtv%i, dtv%c
   end select

   iomsg = 'dtiowrite'

end subroutine
