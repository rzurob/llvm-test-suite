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
! %GROUP: dummyArg101a4.f
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
!*                                        Try namelist formatting for derived type object which is a explicit array dummy argument
!*                                        which contains internal subroutine
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

   integer :: unit = 1

contains

   subroutine readBase(dtv,lb)
      class(base), intent(inout) :: dtv(3)
      integer, intent(in) :: lb

      integer :: stat
      character(200) :: msg

      if (( innerreadBase(dtv,lb) /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

      contains

         integer function innerreadBase(dtv,lb)
            class(base), intent(inout) :: dtv(lb:(lb+2))  !<- contains 3 elements
            integer, intent(in) :: lb

            namelist /nml/ dtv
            read ( unit, nml, iostat=innerreadBase, iomsg = msg)

         end function
   end subroutine

end module

program dummyArg101a4
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable  :: b1(:)
   class(base), pointer      :: b2(:)
   type(child)               :: b3(3)
   type(child), pointer      :: b4(:)

   open (unit, file = 'dummyArg101a4.1', form='formatted', access='sequential' )

   allocate( b1(3) )
   allocate( child :: b2(3) )
   allocate( b4(3) )

   call readBase(b1,1)
   call readBase(b2,2)
   call readBase(b3,3)
   call readBase(b4,4)

   if ( ( b1(1)%c /= 'abc' ) .or. ( b1(2)%c /= 'def' ) .or. ( b1(3)%c /= 'ghi' ) )    error stop 3_4

   select type ( b2 )
      type is ( base )
         error stop 4_4
      type is ( child )
         if ( ( b2(1)%c /= 'ABC' ) .or. ( b2(1)%i /= 3 ) .or. ( b2(2)%c /= 'DEF' ) .or. ( b2(2)%i /= 4 ) .or. &
              ( b2(3)%c /= 'GHI' ) .or. ( b2(3)%i /= 5 ) ) error stop 5_4
   end select

   if ( ( b3(1)%c /= 'mno' ) .or. ( b3(1)%i /= 4 ) .or. ( b3(2)%c /= 'pqr' ) .or. ( b3(2)%i /= 5 ) .or. &
        ( b3(3)%c /= 'xxx' ) .or. ( b3(3)%i /= -999 ) ) error stop 6_4

   if ( ( b4(1)%c /= 'stu' ) .or. ( b4(1)%i /= 6 ) .or. ( b4(2)%c /= 'xxx' ) .or. ( b4(2)%i /= -999 ) .or. &
        ( b4(3)%c /= 'vwx' ) .or. ( b4(3)%i /= 7 ) )    error stop 7_4

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
      type is ( base )
         read (unit, "(A3)", iostat=iostat )        dtv%c
      type is ( child )
         read (unit, "(I1,1X,A3)", iostat=iostat )  dtv%i, dtv%c
   end select

   iomsg = 'dtioread'

end subroutine
