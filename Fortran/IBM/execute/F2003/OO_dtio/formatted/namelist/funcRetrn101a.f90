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
   end type

   type, extends(base) :: child
      integer(4) :: i
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
   integer :: stat
   character(200) :: msg

contains

   class(base) function read(i)
      namelist /R/ read
      allocatable :: read
      integer, intent(in) :: i

      if ( i == 0 ) then
         allocate ( base :: read )
         read (unit, R, iostat = stat, iomsg = msg )
      else
         allocate ( child :: read )
         read (unit, R, iostat = stat, iomsg = msg )
      end if
   end function

end module

program funcRetrn101a
   use m, newread => read

   class(base), allocatable :: b1
   type(base)               :: b2 = base  ('xxx')
   type(child)              :: c1 = child ('xxx',-999)
   class(child), pointer    :: c2

   open (1, file = 'funcRetrn101a.1', form='formatted', access='sequential' )

   allocate ( b1, source = newread(0) )
   select type ( gg => newread(1) )
      class is (child)
         allocate ( c2, source = gg )
   end select

   if ( b1%c /= 'IBM' )                           error stop 1_4
   if ( ( c2%c /= 'FTN' ) .or. ( c2%i /= 123 ) )  error stop 2_4

   deallocate (b1)
   allocate(b1, source = child('xxx',-999))

   select type ( g => newread(1) )
      type is (child)
         c1 = g
   end select

   if ( ( c1%c /= 'IBM' ) .or. ( c1%i /= 2005 ) )  error stop 3_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base,child

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
         read (unit, "(A3)", iostat=iostat )            dtv%c
      type is (child)
         read (unit, *, iostat=iostat )      dtv%i, dtv%c
   end select

   iomsg = 'dtioread'

end subroutine
