! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar101kl
!*
!*  DATE                       : 2007-07-10 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with polymorphic/nonpoly scalar allocatable (Input)
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
      integer(kb) :: i = 0
   end type

end module

module m1
   use m
   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(inout) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface
end module


program scalar101kl
   use m1

   integer :: stat
   character(200) :: msg = ''
   class(base(4)), allocatable :: b1 ! tcx: (4)
   class(base(4)), pointer     :: b2 ! tcx: (4)
   type(base(4))               :: b3 ! tcx: (4)
   type(base(4)), allocatable  :: b4 ! tcx: (4)
   type(base(4)), pointer      :: b5 ! tcx: (4)

   namelist /nml/ b1, b2, b3, b4, b5
   open (1, file = 'scalar101kl.1', form='formatted', access='sequential', status='old', BLANK='NULL' )

   allocate(b1, b2, b4, b5)

   read  (1,NML=nml, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   if ( b1%i /= 99 ) error stop 2_4
   if ( b2%i /= 30 ) error stop 3_4
   if ( b3%i /= 8 )  error stop 4_4
   if ( b4%i /= 4 )  error stop 5_4
   if ( b5%i /= 2 )  error stop 6_4

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m1, only: base

   class(base(4)), intent(inout) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 7_4
   if ( size(v_list, 1) /= 0 ) error stop 8_4

   read (unit, "(i2)", iostat=iostat )      dtv%i

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 7 changes
