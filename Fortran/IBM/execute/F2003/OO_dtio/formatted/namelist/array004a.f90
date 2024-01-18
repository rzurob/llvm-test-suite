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
! %GROUP: array004a.f
! %VERIFY: array004a.1:array004a.vf
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
!*                                        Try namelist formatting array objects with sequence type contain non-polymorphic component(Output)
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

   type :: data
      sequence
      complex(4)   :: x = (0,0)
   end type

   type :: base
      sequence
      type(data), allocatable :: b(:)
      integer(4)   ::  i = -999
      character(3) ::  c = 'xxx'
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         type(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program array004a
   use m

   type(base), pointer     :: b1(:)
   type(base), allocatable :: b2(:)
   type(base)              :: b3(2,2)

   integer :: stat
   character(200) :: msg = ''

   namelist /nml1/ b1, b2, b3
   
   allocate ( b1(4), source = (/ base(null(),c='abc', i =101), base(null(),c='def', i =102), base(null(),c='ghi',i = 103), base(null(),c='jkl',i = 104) /) )
   allocate ( b2(4), source = (/ base(null(),c='ABC', i =105), base(null(),c='DEF', i =106), base(null(),c='GHI',i = 107), base(null(),c='JKL',i = 108) /) )
   b3 = reshape( source = b2(4:1:-1) , shape = (/2,2/) )     !<- b2 in reverse order

   allocate( b3(1,1)%b(2), source = (/data((1,2)), data((3,4))/) )
   allocate( b3(2,1)%b(2), source = (/data((5,6)), data((7,8))/) )
   allocate( b3(1,2)%b(0) )
   allocate( b3(2,2)%b(2), source = (/data((9,10)), data((11,12))/) )
   allocate( b1(1)%b(1), source = (/data((13,14))/) )
   allocate( b1(2)%b(2), source = (/data((15,16)), data((17,18))/) )
   allocate( b1(3)%b(0) )
   allocate( b1(4)%b(2), source = (/data((19,20)), data((21,22))/) )
   allocate( b2(1)%b(1), source = (/data((23,24))/) )
   allocate( b2(2)%b(2), source = (/data((27,28)), data((29,30))/) )
   allocate( b2(3)%b(0) )
   allocate( b2(4)%b(2), source = (/data((31,32)), data((33,34))/) )

   open (1, file = 'array004a.1', form='formatted', access='sequential' )

   write (1,NML=nml1, iostat=stat, iomsg=msg)

   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, data

   interface write(formatted)
      subroutine writeformatteddata(dtv, unit, iotype, v_list, iostat, iomsg )
         import data
         type(data), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   type(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   type(data), allocatable :: dummy(:)
   namelist /dtio/ dummy

   allocate( dummy(size(dtv%b,1)), source= (/ dtv%b /) )
   
   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   write ( unit, dtio  , iostat = iostat, iomsg = iomsg )
   if ( iostat /= 0 ) error stop 5_4
   write ( unit, "(A3)", iostat = iostat ) dtv%c
   write ( unit, "(I4)", iostat = iostat ) dtv%i
   
   iomsg = 'dtiowrite'

end subroutine

subroutine writeformatteddata (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: data

   type(data), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   write ( unit, *, iostat = iostat ) dtv%x

   iomsg = 'datawrite'

end subroutine
