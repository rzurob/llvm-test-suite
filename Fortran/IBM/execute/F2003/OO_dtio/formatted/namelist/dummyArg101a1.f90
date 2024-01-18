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
! %GROUP: dummyArg101a1.f
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
!*                                        Try namelist formatting for derived type object
!*                                        which is a explicit array dummy argument with non-initialization
!*                                        expr bounds.
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

   integer :: unit = 1

contains

   subroutine readBase(dtv,lb,ub)
      integer, intent(in) :: lb,ub
      class(base), intent(inout) :: dtv(lb:ub)
      integer :: stat
      character(200) :: msg

      namelist /nml/ dtv
      read ( unit, nml, iostat=stat, iomsg = msg)
      if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   end subroutine

end module

program dummyArg101a1
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable  :: b1(:)
   class(base), pointer      :: b2(:)
   type(child)               :: b3(3)
   type(child), pointer      :: b4(:)

   open (unit, file = 'dummyArg101a1.1', form='formatted', access='stream' )

   allocate( child :: b1(2:4) )
   allocate( child :: b2(3:5) )
   allocate( b4(4:6) )

   call readBase(b1,1,3)
   call readBase(b2,2,4)
   call readBase(b3,3,5)
   call readBase(b4,4,6)

   select type ( b1 )
      type is (child)
         if ( ( b1(2)%c /= 'abc' ) .or. ( b1(3)%c /= 'def' ) .or. ( b1(4)%c /= 'ghi' ) .or. &
              ( b1(2)%I /= 1001  ) .or. ( b1(3)%I /= 1002  ) .or. ( b1(4)%I /= 1003  ) ) error stop 2_4
   end select

   select type ( b2 )
      type is (child)
         if ( ( b2(3)%c /= 'ABC' ) .or. ( b2(4)%c /= 'DEF' ) .or. ( b2(5)%c /= 'GHI' ) .or. &
              ( b2(3)%I /= 2001 ) .or. ( b2(4)%I /= 2002 ) .or. ( b2(5)%I /= 2003 ) )  error stop 3_4
   end select

   if ( ( b3(1)%c /= 'abc' ) .or. ( b3(2)%c /= 'xxx' ) .or. ( b3(3)%c /= 'ghi' )       .or. &
        ( b3(1)%I /= 3001  ) .or. ( b3(2)%I /= -999  ) .or. ( b3(3)%I /= 3003  ) )     error stop 4_4

   if ( ( b4(4)%c /= 'ABC' ) .or. ( b4(5)%c /= 'xxx' ) .or. ( b4(6)%c /= 'GHI' )       .or. &
        ( b4(4)%I /= 4001  ) .or. ( b4(5)%I /= -999  ) .or. ( b4(6)%I /= 4003  ) )     error stop 5_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   select type ( dtv )
      class is ( base )
         error stop 4_4
      type is ( child )
         read (unit, "(I4,1X,A3,1X)", iostat=iostat )        dtv%i, dtv%c
   end select

   iomsg = 'dtioread'

end subroutine

