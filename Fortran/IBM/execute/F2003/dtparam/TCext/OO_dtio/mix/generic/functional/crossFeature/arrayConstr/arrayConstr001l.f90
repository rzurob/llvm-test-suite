!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : arrayConstr001l
!*
!*  PROGRAMMER                 : David Forster (derived from arrayConstr001 by Robert Ma)
!*  DATE                       : 2007-07-23 (original: 04/26/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Array Constructor
!*                                    -  Array constructor with formatted i/o
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

   type base (lb) ! lb=3
      integer, len :: lb
      character(lb) :: c = 'xxx'
      contains
         procedure, pass :: write => writeb
         generic :: write(formatted) => write
   end type

   type, extends(base) :: child (kc) ! kc=4
      integer, kind :: kc
      integer(kc) :: i = -999
      contains
         procedure, pass :: write => writec
   end type

   type, extends(child) :: gen3
      character(lb) :: s = 'xxx'
      contains
         procedure, pass :: write => writeg
   end type

   contains

      subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt

         write (fmt, *) "(A", v_list(1), ")"
         write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt

         write (fmt, *) "(A", v_list(1), ",1X, I",v_list(2),")"

         write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine writeg (dtv, unit, iotype, v_list, iostat, iomsg)
         class(gen3(*,4)), intent(in) :: dtv ! tcx: (*,4,*) ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(30) :: fmt

         write (fmt, *) "(A", v_list(1), ",1X, I",v_list(2),",1X, A", v_list(3),")"

         write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%s

         iomsg = 'dtiowriteg'

      end subroutine

end module

program arrayConstr001l
   use m

   integer :: stat
   character(200) :: msg

   open ( 1, file = 'arrayConstr001l.1', form='formatted', access='sequential' )

   write ( 1, "(3(DT(3)))", iostat = stat, iomsg = msg )            (/ base(3)(), base(3)(), base(3)() /) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 101_4
   
   write ( 1, "(3(DT(3,4)))", iostat = stat, iomsg = msg )          (/ ( child(3,4)(),i=1,3 ) /) ! tcx: (3,4)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 2_4
   
   write ( 1, "(3(DT(3,4,3)))", iostat = stat, iomsg = msg )        (/ ( gen3(3,4)(),i=4,6 ) /) ! tcx: (3,4,3) ! tcx: (3,4)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) ) error stop 3_4
   
   write ( 1, "(4(DT(3)))", iostat = stat, iomsg = msg )          (/ base(3)('abc'), base(3)('def'), ( base(3)('ghi'), i=1,2 )/) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 4_4
   
   write ( 1, "(3(DT(3,4)))", iostat = stat, iomsg = msg )        (/ ( child(3,4)('IBM',2005),i=1,3 ) /) ! tcx: (3,4)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 5_4
   
   write ( 1, "(3(DT(4,4,4)))", iostat = stat, iomsg = msg )            (/ ( gen3(3,4)('FTN',2003,'GRT'),i=4,6 ) /) ! tcx: (3,4,3) ! tcx: (3,4)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) ) error stop 6_4

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 7 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 3 changes
! type: gen3 - added parameters () to invoke with (3,4) / declare with (*,4) - 3 changes
