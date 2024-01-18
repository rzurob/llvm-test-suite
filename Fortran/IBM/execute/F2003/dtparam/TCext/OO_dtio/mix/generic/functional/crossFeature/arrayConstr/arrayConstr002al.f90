!*  ===================================================================
!*
!*  TEST CASE NAME             : arrayConstr002al
!*
!*  DATE                       : 2007-08-02 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Array Constructor
!*                                    -  Array constructor with allocatable components and io-implied-do unformatted i/o
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
      character(lb), allocatable :: c
      contains
         procedure, pass :: write => writeb
         generic :: write(unformatted) => write
   end type

   type, extends(base) :: child
      character(lb), allocatable :: i
      contains
         procedure, pass :: write => writec
   end type

   type, extends(child) :: gen3
      character(lb), allocatable :: s
      contains
         procedure, pass :: write => writeg
   end type

   contains

      subroutine writeb (dtv, unit, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine writeg (dtv, unit, iostat, iomsg)
         class(gen3(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%s

         iomsg = 'dtiowriteg'

      end subroutine

end module

program arrayConstr002al
   use m

   integer :: stat
   character(200) :: msg

   character(12)  :: c4
   character(18) :: c5
   character(54) :: c6


   open ( 1, file = 'arrayConstr002al.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg )          (/ base(3)('abc'), base(3)('def'), ( base(3)('ghi'), i=1,2 )/) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 101_4

   write ( 1, iostat = stat, iomsg = msg )          (/ ( child(3)('IBM','ibm'), i=1,3 ) /) ! tcx: (3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 2_4

   write ( 1, iostat = stat, iomsg = msg )          ( (/ ( gen3(3)('FTN','ftn','GRT'),i=4,6 ) /), i=0,1) ! tcx: (3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) ) error stop 3_4

   rewind 1

   read (1, iostat = stat, iomsg = msg )            c4
   read (1, iostat = stat, iomsg = msg )            c5
   read (1, iostat = stat, iomsg = msg )            c6

   if ( ( c4 /= 'abcdefghighi' ) .or. ( c5 /= 'IBMibmIBMibmIBMibm' ) .or. ( c6 /= 'FTNftnGRTFTNftnGRTFTNftnGRTFTNftnGRTFTNftnGRTFTNftnGRT' ) ) error stop 7_4


end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 4 changes
! type: child - added parameters () to invoke with (3) / declare with (*) - 2 changes
! type: gen3 - added parameters () to invoke with (3) / declare with (*) - 2 changes
