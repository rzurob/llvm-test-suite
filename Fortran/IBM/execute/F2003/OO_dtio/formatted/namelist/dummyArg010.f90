!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dummyArg010.f
! %VERIFY: dummyArg010.1:dummyArg010.vf
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with dummy argument with VALUE attr(Output)
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
      integer :: i
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
contains
   subroutine valueWrite ( unit, b1, b2, b3 )
      integer, intent(in) :: unit
      type(base), intent(in), value :: b1
      type(base), intent(in), value :: b2
      type(base), intent(in), value :: b3

      integer :: stat
      character(150) :: msg

      namelist /polymorphic/ b1, b2, b3

      write (unit, polymorphic, iostat = stat, iomsg = msg)

      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   end subroutine

   integer function valueWrite1 ( unit, b1, b2, b3 )
      integer, intent(in) :: unit
      type(base), intent(in), value   :: b1
      type(base), intent(in), value   :: b2
      type(base), intent(in), value   :: b3

      character(150) :: msg

      namelist /nonpolymorphic/ b1, b2, b3

      write (unit, nonpolymorphic, iostat = valueWrite1, iomsg = msg)
      if ( ( valueWrite1 /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   end function

end module

program dummyArg010
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1
   class(base), pointer     :: b2
   type(base)               :: b3
   type(base), allocatable  :: b4
   type(base), pointer      :: b5

   open (1, file = 'dummyArg010.1', form='formatted', access='sequential' )
   allocate(b1, b2, b4, b5)

   b1%i = 2
   b2%i = 4
   b3%i = 6
   b4%i = 8
   b5%i = 10

   call valueWrite(1, b1, b2, b1)   !<- this writes 2,4,2 to file
   call valueWrite(1, b2, b5, b4)   !<- this writes 4,10,8 to file
   call valueWrite(1, b3, b1, b2)   !<- this writes 6,2,4  to file

   if ( valueWrite1 ( 1, b3, b5, b4 ) /= 0 ) error stop 3_4   !<- writes 6, 10, 8
   if ( valueWrite1 ( 1, b4, b5, b4 ) /= 0 ) error stop 4_4   !<- writes 8, 10, 8
   if ( valueWrite1 ( 1, b5, b5, b4 ) /= 0 ) error stop 5_4   !<- writes 10, 10, 8

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 6_4
   if ( size(v_list, 1) /= 0 ) error stop 7_4

   write (unit, "('i=',I4,1X)", iostat=iostat )      dtv%i

   iomsg = 'dtiowrite'

end subroutine
