! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : array004akl
!*
!*  DATE                       : 2007-06-20 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
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

   type :: data (kd)
      integer, kind :: kd
      sequence
      complex(kd)   :: x = (0,0)
   end type

   type :: base (kb,lb)
      integer, kind :: kb
      integer, len :: lb
      sequence
      type(data(kb)), allocatable :: b(:)
      integer(kb)   ::  i = -999
      character(lb) ::  c = 'xxx'
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         type(base(4,*)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program array004akl
   use m

   type(base(4,:)), pointer     :: b1(:)
   type(base(4,:)), allocatable :: b2(:)
   type(base(4,3))              :: b3(2,2)

   integer :: stat
   character(200) :: msg = ''

   namelist /nml1/ b1, b2, b3

   allocate ( b1(4), source = (/ base(4,3)(null(),c='abc', i =101), base(4,3)(null(),c='def', i =102), base(4,3)(null(),c='ghi',i = 103), base(4,3)(null(),c='jkl',i = 104) /) )
   allocate ( b2(4), source = (/ base(4,3)(null(),c='ABC', i =105), base(4,3)(null(),c='DEF', i =106), base(4,3)(null(),c='GHI',i = 107), base(4,3)(null(),c='JKL',i = 108) /) )
   b3 = reshape( source = b2(4:1:-1) , shape = (/2,2/) )     !<- b2 in reverse order

   allocate( b3(1,1)%b(2), source = (/data(4)((1,2)), data(4)((3,4))/) )
   allocate( b3(2,1)%b(2), source = (/data(4)((5,6)), data(4)((7,8))/) )
   allocate( b3(1,2)%b(0) )
   allocate( b3(2,2)%b(2), source = (/data(4)((9,10)), data(4)((11,12))/) )
   allocate( b1(1)%b(1), source = (/data(4)((13,14))/) )
   allocate( b1(2)%b(2), source = (/data(4)((15,16)), data(4)((17,18))/) )
   allocate( b1(3)%b(0) )
   allocate( b1(4)%b(2), source = (/data(4)((19,20)), data(4)((21,22))/) )
   allocate( b2(1)%b(1), source = (/data(4)((23,24))/) )
   allocate( b2(2)%b(2), source = (/data(4)((27,28)), data(4)((29,30))/) )
   allocate( b2(3)%b(0) )
   allocate( b2(4)%b(2), source = (/data(4)((31,32)), data(4)((33,34))/) )

   open (1, file = 'array004akl.1', form='formatted', access='sequential' )

   write (1,NML=nml1, iostat=stat, iomsg=msg)

   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, data

   interface write(formatted)
      subroutine writeformatteddata(dtv, unit, iotype, v_list, iostat, iomsg )
         import data
         type(data(4)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   type(base(4,*)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   type(data(4)), allocatable :: dummy(:)
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

   type(data(4)), intent(in) :: dtv
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
