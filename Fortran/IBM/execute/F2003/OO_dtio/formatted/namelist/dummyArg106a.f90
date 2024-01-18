!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dummyArg106a.f
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
!*                                        Try namelist formatting for derived type object with module subroutine (Host Association)
!*                                        Try internal file and input statement
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
   end type

   type, extends(base) :: child
      integer(4)   ::  i = -999
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

   class(base), pointer :: b1
   class(base), allocatable :: b2
   character(29) :: internalFile (10)
   namelist /nmlb1b2/ b1, b2

contains

   subroutine readB1B2(unit)
      class(*), intent(in) :: unit(:)

      integer :: stat
      character(200) :: msg
      select type(unit)
         type is (character(*))  !<- internal file
            read ( unit, nmlb1b2, iostat=stat, iomsg = msg)
            if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
      end select

   end subroutine

end module

program dummyArg106a
   use m

   integer :: stat
   character(200) :: msg = ''

   allocate(b1, source = base () )
   allocate(b2, source = child() )

   write ( internalFile(1), * ) " &NMLB1B2"
   write ( internalFile(2), * ) " B1= abc"
   write ( internalFile(3), * ) " B2= 1234 def"
   write ( internalFile(4), * ) " B1= ABC"
   write ( internalFile(5), * ) " B2= 4321 DEF   /"
   write ( internalFile(8), * ) " &NMLB1B2"
   write ( internalFile(9), * ) " B1= 9876 abc"
   write ( internalFile(10), * )" B2= 1234 def    /"

   call readB1B2(internalFile)

   if ( b1%c /= 'ABC' ) error stop 2_4
   select type (b2)
      type is (child)
         if (( b2%c /= 'DEF' ) .or. (b2%i /= 4321)) error stop 3_4
      class default
         error stop 4_4
   end select

   allocate (b1, source=child() )
   call readB1B2(internalFile(8:10))
   select type (b1)
      type is (child)
         if (( b1%c /= 'abc' ) .or. (b1%i /= 9876)) error stop 5_4
      class default
         error stop 6_4
   end select

   select type (b2)
      type is (child)
         if (( b2%c /= 'def' ) .or. (b2%i /= 1234)) error stop 7_4
      class default
         error stop 8_4
   end select

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 9_4
   if ( size(v_list, 1) /= 0 ) error stop 10_4

   select type (dtv)
      type is (base)
         read (unit, "(A3)", iostat=iostat )        dtv%c
      type is (child)
         read (unit, "(I4,1X,A3)", iostat=iostat )  dtv%i, dtv%c
   end select

   iomsg = 'dtioread'

end subroutine
