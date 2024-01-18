! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : array001kl
!*
!*  DATE                       : 2007-06-20 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with polymorphic/nonpoly array (Output)
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
   type base (kb)
      integer, kind :: kb
      integer(kb) :: i
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base(4)), pointer :: b2(:,:)
   namelist /nml1/ b2

end module

program array001kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(4)), allocatable :: b1(:)
   type(base(4))               :: b3(2,2)
   type(base(4)), allocatable  :: b4(:,:)

   namelist /nml2/ b1, b3
   namelist /nml2/ b4

   open (1, file = 'array001kl.1', form='formatted', access='stream' )
   allocate(b1(2), source = (/ base(4)(1), base(4)(2) /))
   b3 = reshape( source = (/base(4)(7), base(4)(8), base(4)(9), base(4)(10)/) , shape =(/2,2/))
   allocate(b4(2,2), source = reshape( source = (/base(4)(3), base(4)(4), base(4)(5), base(4)(6)/) , shape =(/2,2/)))
   allocate(b2(2,2), source = b4 )

   write (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   write (unit, "('i= ',I4,1X)", iostat=iostat )      dtv%i

   iomsg = 'dtiowrite'

end subroutine
