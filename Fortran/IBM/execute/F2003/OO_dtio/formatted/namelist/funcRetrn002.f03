! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with non polymorphic array function return variable
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

end module

program funcRetrn002
   use m

   class(base), allocatable :: b1(:)
   type(base)               :: b2(3) = (/ base('abc'), base('def'), base('ghi') /)
   type(child)              :: c1(3) = (/ child('ABC',101), child('DEF',102), child('GHI',103) /)
   class(child), pointer    :: c2(:)

   open (1, file = 'funcRetrn002.1', form='formatted', access='sequential' )

   allocate ( b1(3), source = writebase(b2)  ) !<- writes b2
   allocate ( c2(3), source = writechild(c1) ) !<- writes c1

   deallocate (b1)
   allocate(b1(3), source = (/ child('JKL',201), child('MNO',202), child('PQR',203) /))

   select type ( b1 )
      type is (child)
         c1 = writechild(b1) !<- writes b1
   end select

contains

   type(base) function writeBase(dtv)
      class(base), intent(in) :: dtv(3)
      dimension :: writeBase(3)
      namelist /WB/ writeBase

      select type ( dtv )
         type is (base)
            writebase = dtv
      end select

      write (unit, WB, iostat = stat, iomsg = msg )

   end function

   function writeChild(dtv)
      class(child), intent(in) :: dtv(3)
      type(child) :: writeChild(3)
      namelist /WC/ writeChild

      select type ( dtv )
         type is (child)
            writechild = dtv
      end select

      write (unit, WC, iostat = stat, iomsg = msg )

   end function

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
