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

end module

program funcRetrn102a
   use m

   class(base), allocatable :: b1(:)
   type(child), allocatable :: c1(:)
   class(child), pointer    :: c2(:)

   open (1, file = 'funcRetrn102a.1', form='formatted', access='sequential' )

   allocate ( b1(3), source = read(0) )
   select type ( g => read(1) )
      class is (child)
         allocate ( c2(3), source = g )
   end select

   if ( ( b1(1)%c /= 'abc' ) .or. ( b1(2)%c /= 'def' ) .or. ( b1(3)%c /= 'ghi' ) )  error stop 1_4

   if ( ( c2(1)%c /= 'ABC' ) .or. ( c2(2)%c /= 'DEF' ) .or. ( c2(3)%c /= 'GHI' ) .or. &
        ( c2(1)%i /= 101 ) .or. ( c2(2)%i /= 102 ) .or. ( c2(3)%i /= 103 ) )        error stop 2_4

   allocate ( c1(3) )

   select type ( gg => read(1) )
      type is ( child )
         c1 = gg
   end select

   if ( ( c1(1)%c /= 'JKL' ) .or. ( c1(2)%c /= 'MNO' ) .or. ( c1(3)%c /= 'PQR' ) .or. &
     ( c1(1)%i /= 201 ) .or. ( c1(2)%i /= 202 ) .or. ( c1(3)%i /= 203 ) )           error stop 3_4

contains

   class(base) function read(i)
      dimension :: read(:)
      allocatable :: read
      integer, intent(in) :: i
      namelist /R/ read

      if ( i == 0 ) then
         allocate ( base :: read(3) )
      else
      	 allocate ( child :: read(3) )
      end if

      read (unit, R, iostat = stat, iomsg = msg )

   end function

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
         read (unit, "(A3,1X)", iostat=iostat )            dtv%c
      type is (child)
         read (unit, "(I3,1X,A3)", iostat=iostat )      dtv%i, dtv%c
   end select

   iomsg = 'dtioread'

end subroutine
