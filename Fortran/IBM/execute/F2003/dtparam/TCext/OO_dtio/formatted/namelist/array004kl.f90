! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : array004kl
!*
!*  DATE                       : 2007-06-20 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting array objects with sequence type (Output)
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

   type :: base (kb,lb)
      integer, kind :: kb
      integer, len :: lb
      sequence
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

program array004kl
   use m

   type(base(4,:)), pointer     :: b1(:)
   type(base(4,:)), allocatable :: b2(:)
   type(base(4,3))              :: b3(2,2)

   integer :: stat
   character(200) :: msg = ''

   namelist /nml1/ b1, b2, b3

   allocate ( b1(4), source = (/ base(4,3)(c='abc', i =101), base(4,3)(c='def', i =102), base(4,3)(c='ghi',i = 103), base(4,3)(c='jkl',i = 104) /) )
   allocate ( b2(4), source = (/ base(4,3)(c='ABC', i =105), base(4,3)(c='DEF', i =106), base(4,3)(c='GHI',i = 107), base(4,3)(c='JKL',i = 108) /) )
   b3 = reshape( source = b2(4:1:-1) , shape = (/2,2/) )     !<- b2 in reverse order

   open (1, file = 'array004kl.1', form='formatted', access='sequential' )

   write (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   type(base(4,*)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   write ( unit, "(A3)", iostat = iostat ) dtv%c
   write ( unit, "(I4)", iostat = iostat ) dtv%i

   iomsg = 'dtiowrite'

end subroutine
