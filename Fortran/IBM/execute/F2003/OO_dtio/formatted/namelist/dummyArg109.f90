!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dummyArg109.f
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
!*                                        Try namelist formatting for derived type object with type bound procedures (input)
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

   type, abstract :: base
      character(3) ::  c = 'xxx'
      contains
         procedure, pass :: readme => readbase
   end type

   type, extends(base) :: child
      integer(4)   ::  i = -999
      contains
         procedure, pass :: readme => readchild
   end type

   type, extends(base) :: badchild
      character(3) :: c1 = 'XXX'
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

   subroutine readbase(b, unit)
      class(base), intent(inout) :: b
      integer, intent(in) :: unit
      namelist /basenml/ b
      integer :: stat
      character(200) :: msg
      read (unit, basenml, iostat=stat, iomsg = msg)
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   end subroutine

   subroutine readchild(b, unit)
      class(child), intent(inout) :: b
      integer, intent(in) :: unit
      namelist /childnml/ b
      integer :: stat
      character(200) :: msg

      read (unit, childnml, iostat=stat, iomsg = msg)
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4
   end subroutine

end module

program dummyArg109
   use m
   class(base), pointer       :: b1
   class(base), allocatable   :: b2
   type(child)                :: b3
   type(badchild)             :: b4
   class(badchild), allocatable  :: b5
   class(child), pointer      :: b6

   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'dummyArg109.1', form='formatted', access='stream' )

   allocate(badchild::b1)
   allocate(child::b2)
   b3 = child()
   b4 = badchild()
   allocate(b5, b6)

   call b1%readme(1)
   call b2%readme(1)
   call b3%readme(1)
   call b4%readme(1)
   call b5%readme(1)
   call b6%readme(1)

   select type (b1)
      type is (badchild)
         if (( b1%c /= 'abc' ) .or. ( b1%c1 /= 'ABC' ) ) error stop 1_4
      class default
         error stop 2_4
   end select

   select type (b2)
      type is (child)
         if (( b2%c /= 'def' ) .or. ( b2%i /= 1234 ) )   error stop 3_4
      class default
         error stop 4_4
   end select

   if (( b3%c /= 'ghi' ) .or. ( b3%i /= 2345 ) )     error stop 5_4
   if (( b4%c /= 'jkl' ) .or. ( b4%c1 /= 'JKL' ) )   error stop 6_4
   if (( b5%c /= 'mno' ) .or. ( b5%c1 /= 'MNO' ) )   error stop 7_4
   if (( b6%c /= 'pqr' ) .or. ( b6%i /= 3456 ) )     error stop 8_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child, badchild

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 9_4
   if ( size(v_list, 1) /= 0 ) error stop 10_4

   select type (dtv)
      type is (child)
         read (unit, "(A3,1X,I4)", iostat=iostat )  dtv%c, dtv%i
      type is (badchild)
         read (unit, "(A3,1X,A3)", iostat=iostat )  dtv%c, dtv%c1
      class default
         error stop 11_4
   end select

   iomsg = 'dtioread'

end subroutine
