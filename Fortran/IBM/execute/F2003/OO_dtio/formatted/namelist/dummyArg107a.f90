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
! %GROUP: dummyArg107a.f
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
!*                                        Try namelist formatting for derived type object external subroutine with internal sub(Host Association)
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

   class(base), pointer :: b2

end module

program dummyArg107a
   use m

   interface
      subroutine readBase(dtv, unit)
         import base
         class(base), intent(inout) :: dtv
         integer, intent(in)     :: unit
      end subroutine
   end interface

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1
   type(base)               :: b3
   type(base), pointer      :: b4

   open (1, file = 'dummyArg107a.1', form='formatted', access='stream' )

   allocate(b1, source = base() )
   allocate(b2, source = base() )
   b3 = base()
   allocate(b4, source = b3 )

   call readBase(b1,1)
   if ( ( b1%i /= 1234 ) .or. ( b1%c /= 'abc' ) ) error stop 1_4
   call readBase(b2,1)
   if ( ( b2%i /= 2345 ) .or. ( b2%c /= 'def' ) ) error stop 2_4
   call readBase(b3,1)
   if ( ( b3%i /= 3456 ) .or. ( b3%c /= 'ghi' ) ) error stop 3_4
   call readBase(b4,1)
   if ( ( b4%i /= 4567 ) .or. ( b4%c /= 'jkl' ) ) error stop 4_4

end program

subroutine readBase(dtv, unit)
   use m
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer :: stat
   character(200) :: msg

   namelist /nml/ dtv
   call internalreadBase(unit)

contains

   subroutine internalreadBase(unit)
      integer, intent(in) :: unit
      read ( unit, nml, iostat=stat, iomsg = msg)
      if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 5_4
   end subroutine

end subroutine

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 6_4
   if ( size(v_list, 1) /= 0 ) error stop 7_4

   read (unit, "(I4,1X,A3)", iostat=iostat )        dtv%i, dtv%c


   iomsg = 'dtioread'

end subroutine

