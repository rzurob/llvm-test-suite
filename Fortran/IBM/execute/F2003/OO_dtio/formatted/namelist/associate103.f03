! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with associate construct
!*                                        change value with associate-name with unlimited polymorphic
!*                                        component(Output)
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
   type data
      integer :: i = 123
   end type

   type base
      class(*), pointer :: u
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

end module

program associate103
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1
   class(base), pointer     :: b2
   type(base)               :: b3
   type(base), allocatable  :: b4
   type(base), pointer      :: b5

   namelist /nml/ b1, b2, b3
   namelist /nml/ b4, b5

   open (1, file = 'associate103.1', form='formatted', access='sequential' )
   allocate(b1, b2, b4, b5)

   associate( b1 => b2, b2 => b3, b3 => b4, b4 => b5, b5 => b1)
      allocate(b1%u, source = -9_8)
      allocate(b2%u, source = 'xxxxxx')
      allocate(b3%u, source = data(-9) )
      allocate(b4%u, source = -9_4)
      allocate(b5%u, source = -9_2)
      read (1, nml, iostat = stat, iomsg = msg )
   end associate

   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   select type (g => b1%u)
      type is (integer(2))
         print *, g
   end select
   select type (g => b2%u)
      type is (integer(8))
         print *, g
   end select
   select type (g => b3%u)
      type is (character(*))
         print *, g
   end select
   select type (g => b4%u)
      type is (data)
         print *, g
   end select
   select type (g => b5%u)
      type is (integer(4))
         print *, g
   end select

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, data

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   type(data) :: dummy
   namelist /dtio/ dummy

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   select type (g => dtv%u)
      type is (integer(2))
         read ( 1, *, iostat = iostat ) g
      type is (integer(4))
         read ( 1, *, iostat = iostat ) g
      type is (integer(8))
         read ( 1, *, iostat = iostat ) g
      type is (character(*))
         read ( 1, *, iostat = iostat ) g
      type is (data)
         read ( 1, dtio, iostat = iostat )
         g = dummy
      class default
         error stop 4_4
   end select


   iomsg = 'dtioread'

end subroutine
