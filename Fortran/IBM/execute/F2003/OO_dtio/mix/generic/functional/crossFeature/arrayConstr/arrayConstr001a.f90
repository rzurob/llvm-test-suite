!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Array Constructor
!*                                    -  array constructor with unformatted i/o
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

   type base
      character(3) :: c = 'xxx'
      contains
         procedure, pass :: write => writeb
         generic :: write(unformatted) => write
   end type

   type, extends(base) :: child
      character(3) :: i = 'xxx'
      contains
         procedure, pass :: write => writec
   end type

   type, extends(child) :: gen3
      character(3) :: s = 'xxx'
      contains
         procedure, pass :: write => writeg
   end type

   contains

      subroutine writeb (dtv, unit, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine writeg (dtv, unit, iostat, iomsg)
         class(gen3), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%s

         iomsg = 'dtiowriteg'

      end subroutine

end module

program arrayConstr001a
   use m

   integer :: stat
   character(200) :: msg

   character(9)  :: c1
   character(18) :: c2, c5
   character(27) :: c3, c6
   character(12) :: c4


   open ( 1, file = 'arrayConstr001a.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg )            (/ base(), base(), base() /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4

   write ( 1, iostat = stat, iomsg = msg )          (/ ( child(),i=1,3 ) /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 2_4

   write ( 1, iostat = stat, iomsg = msg )        (/ ( gen3(),i=4,6 ) /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) ) error stop 3_4

   write ( 1, iostat = stat, iomsg = msg )          (/ base('abc'), base('def'), ( base('ghi'), i=1,2 )/)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 4_4

   write ( 1, iostat = stat, iomsg = msg )        (/ ( child('IBM','ibm'), i=1,3 ) /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 5_4

   write ( 1, iostat = stat, iomsg = msg )            (/ ( gen3('FTN','ftn','GRT'),i=4,6 ) /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) ) error stop 6_4

   rewind 1
   read (1, iostat = stat, iomsg = msg )            c1
   read (1, iostat = stat, iomsg = msg )            c2
   read (1, iostat = stat, iomsg = msg )            c3
   read (1, iostat = stat, iomsg = msg )            c4
   read (1, iostat = stat, iomsg = msg )            c5
   read (1, iostat = stat, iomsg = msg )            c6

   if ( ( c1 /= 'xxxxxxxxx' ) .or. ( c2 /= 'xxxxxxxxxxxxxxxxxx' ) .or. ( c3 /= 'xxxxxxxxxxxxxxxxxxxxxxxxxxx' ) .or. &
        ( c4 /= 'abcdefghighi' ) .or. ( c5 /= 'IBMibmIBMibmIBMibm' ) .or. ( c6 /= 'FTNftnGRTFTNftnGRTFTNftnGRT' ) ) error stop 7_4

   close (1, status ='delete' )

end program
