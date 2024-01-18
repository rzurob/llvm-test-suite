!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dummyArg110.f
! %VERIFY: dummyArg110.1:dummyArg110.vf
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

contains

   subroutine valueread ( unit, b1, b2, b3 )
      integer, intent(in) :: unit
      type(base), value :: b1
      type(base), value :: b2
      type(base), value :: b3

      integer :: stat
      character(150) :: msg

      namelist /poly/ b1, b2, b3

      read (unit, poly, iostat = stat, iomsg = msg)

      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
      if ( ( b1%i /= -999 ) .or. ( b2%i /= -998 ) .or. ( b3%i /= -997 ) ) error stop 2_4

   end subroutine

   integer function valueread1 ( unit, b1, b2, b3 )
      integer, intent(in) :: unit
      type(base), value   :: b1
      type(base), value   :: b2
      type(base), value   :: b3

      character(150) :: msg

      namelist /nonpoly/ b1, b2, b3

      read (unit, nonpoly, iostat = valueread1, iomsg = msg)
      if ( ( valueread1 /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
      if ( ( b1%i /= -199 ) .or. ( b2%i /= -198 ) .or. ( b3%i /= -197 ) ) error stop 4_4

   end function

end module

program dummyArg110
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1
   class(base), pointer     :: b2
   type(base)               :: b3
   type(base), allocatable  :: b4
   type(base), pointer      :: b5

   open (1, file = 'dummyArg110.1', form='formatted', access='sequential' )
   allocate(b1, b2, b4, b5)

   b1%i = 2
   b2%i = 4
   b3%i = 6
   b4%i = 8
   b5%i = 10

   call valueread (1, b1, b2, b3)
   if ( ( b1%i /= 2 ) .or. ( b2%i /= 4 ) .or. ( b3%i /= 6 ) )  error stop 5_4

   rewind 1

   call valueread (1, b3, b4, b5)
   if ( ( b3%i /= 6 ) .or. ( b4%i /= 8 ) .or. ( b5%i /= 10 ) ) error stop 6_4

   if ( valueread1(1, b5, b4, b3) /= 0 )                       error stop 7_4
   if ( ( b3%i /= 6 ) .or. ( b4%i /= 8 ) .or. ( b5%i /= 10 ) ) error stop 8_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 9_4
   if ( size(v_list, 1) /= 0 ) error stop 10_4

   read (unit, "(I4)", iostat=iostat )      dtv%i

   iomsg = 'dtioread'

end subroutine
