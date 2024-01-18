!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dummyArg103.f
! %VERIFY:
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
!*                                        Try namelist formatting for derived type object which is a dummy argument
!*                                        Subroutine being type bound procedure (for input)
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
      character(3) ::  c = 'xxx'
   contains
      procedure, pass :: readBase
   end type

   type, extends(base) :: child
      integer(4)   ::  i  = -999
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

   class(base), pointer :: b2

contains

   integer function readBase(dtv, unit)
      class(base), intent(inout) :: dtv
      integer, intent(in) :: unit

      integer :: stat
      character(200) :: msg = ''

      namelist /nml/ dtv
      read ( unit, nml, iostat=readBase, iomsg = msg)
      if (( msg /= 'dtioread' )) error stop 1_4

   end function

end module

program dummyArg103
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1
   type(base)               :: b3
   type(base), pointer      :: b4

   open (1, file = 'dummyArg103.1', form='formatted', access='stream' )

   allocate(child :: b1 )
   allocate(b2)
   b3 =  base(c='ghi')
   allocate(b4, source = base(c='jkl') )

   if ( b1%readBase(1) /= 0 ) error stop 2_4
   if ( b2%readBase(1) /= 0 ) error stop 3_4
   if ( b3%readBase(1) /= 0 ) error stop 4_4
   if ( b4%readBase(1) /= 0 ) error stop 5_4

   select type (b1)
      type is (child)
         if ( ( b1%c /= 'abc' ) .or. (b1%i /= 1234 ) ) error stop 6_4
      class default
         error stop 7_4
   end select

   if ( b2%c /= 'def' ) error stop 8_4
   if ( b3%c /= 'ghi' ) error stop 9_4
   if ( b4%c /= 'jkl' ) error stop 10_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 11_4
   if ( size(v_list, 1) /= 0 ) error stop 12_4

   select type (dtv)
      type is (base)
         read (unit, "(A3)", iostat=iostat )        dtv%c
      type is (child)
         read (unit, "(I4,1X,A3)", iostat=iostat )  dtv%i, dtv%c
   end select

   iomsg = 'dtioread'

end subroutine

