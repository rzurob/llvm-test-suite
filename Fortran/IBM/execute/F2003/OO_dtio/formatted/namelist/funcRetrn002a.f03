! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with polymorphic array function return variable
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
      class(base), intent(in) :: dtv(:)
      allocatable :: write(:)
      namelist /W/ write

      allocate ( write(size(dtv,1)), source = dtv )
      write (unit, W, iostat = stat, iomsg = msg )

   end function

end module

program funcRetrn002a
   use m

   class(base), allocatable :: b1(:)
   type(base)               :: b2(3) = (/ base('abc'), base('def'), base('ghi') /)
   type(child)              :: c1(3) = (/ child('ABC',101), child('DEF',102), child('GHI',103) /)
   class(child), pointer    :: c2(:)

   open (1, file = 'funcRetrn002a.1', form='formatted', access='sequential' )

   allocate ( b1(3), source = write(b2) ) !<- writes b2
   associate( gg => write(c1) )           !<- writes c1
      select type ( gg )
         class is (child)
            allocate ( c2(3), source = gg )
      end select
   end associate


   deallocate (b1)
   allocate(b1(3), source = (/ child('JKL',201), child('MNO',202), child('PQR',203) /))

   select type ( g => b1 )
      type is (child)
         select type ( gg => write(g) )
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
