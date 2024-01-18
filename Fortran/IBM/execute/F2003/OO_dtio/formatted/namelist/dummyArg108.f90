!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dummyArg108.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting for derived type object with module subroutine
!*                                        with pointer/allocatable dummy arguments (input)
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

contains

   subroutine readB1B2(unit, b1, b2)
      integer, intent(in) :: unit
      class(base), pointer, intent(inout) :: b1
      class(base), allocatable, intent(inout) :: b2

      namelist /nmlb1b2/ b1, b2

      integer :: stat
      character(200) :: msg

      read ( unit, nmlb1b2, iostat=stat, iomsg = msg)

   end subroutine

end module

program dummyArg108
   use m
   class(base), pointer :: b1
   class(base), allocatable :: b2

   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'dummyArg108.1', form='formatted', access='stream' )

   allocate(b1, source = base()  )
   allocate(b2, source = child() )

   call readB1B2(1, b1, b2)

   select type (b2)
      type is (child)
         if ( ( b1%c /= 'ibm' ) .or. ( b2%c /= 'IBM' ) .or. ( b2%i /= 1234 ))   error stop 1_4
      class default
         error stop 2_4
   end select

   deallocate(b1, b2)
   allocate(b1, source = child() )
   allocate(b2, source = base()  )

   call readB1B2(1, b1, b2)

   select type (b1)
      type is (child)
         if ( ( b1%c /= 'DEF' ) .or. ( b1%i /= 2005 ).or. ( b2%c /= 'ABC' ) )   error stop 2_4
      class default
         error stop 3_4
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

   !if ( iotype /= "NAMELIST" ) error stop 4_4
   if ( size(v_list, 1) /= 0 ) error stop 5_4

   select type (dtv)
      type is (base)
         read (unit, "(A3)", iostat=iostat )         dtv%c
      type is (child)
         read (unit, "(A3,1X,I4)", iostat=iostat )   dtv%c, dtv%i
   end select

   iomsg = 'dtioread'

end subroutine
