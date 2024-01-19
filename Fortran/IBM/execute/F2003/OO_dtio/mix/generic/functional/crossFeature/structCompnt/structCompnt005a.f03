!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Structure Component
!*                                    -  polymorphic structure array component with unformatted i/o
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
         procedure, pass :: read => readb
         generic :: write(unformatted) => write
         generic :: read(unformatted) => read
   end type

   type, extends(base) :: child
      integer(4) :: i = -999
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   type container
      class(base), allocatable :: b1(:)
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

      subroutine readb (dtv, unit, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtioreadb'

      end subroutine

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine


      subroutine readc (dtv, unit, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtioreadc'

      end subroutine

end module

program structCompnt005a
   use m

   integer :: stat
   character(200) :: msg

   type(container) :: cc1
   class(container), allocatable :: cc2

   open ( 1, file = 'structCompnt005a.1', form='unformatted', access='sequential' )

   cc1 = container( (/ base('abc'), base('def'), base('ghi') /) )
   allocate ( cc2, source = container( (/ base('ABC'), base('DEF'), base('GHI'), base('JKL') /) ) )

   write ( 1, iostat = stat, iomsg = msg )      cc1%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )         error stop 1_4

   write ( 1, iostat = stat, iomsg = msg )      cc2%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )         error stop 2_4

   deallocate ( cc2 )
   cc1 = container( (/ child('jkl', 1001), child('mno', 10002), child('pqr', 100003) /) )
   allocate ( cc2, source = container( (/ child('MNO', 21), child('PQR', 202), child('STU', 2003), child('VWX', 20004) /) ) )

   write ( 1, iostat = stat, iomsg = msg )    cc1%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )                         error stop 3_4

   write ( 1, iostat = stat, iomsg = msg )   cc2%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )                                 error stop 4_4

   rewind 1

   deallocate ( cc2 )
   cc1 = container( (/ ( base('xxx') , i=1,3 )  /) )
   allocate ( cc2, source = container( (/ ( base('xxx') , i=1,4 )  /) ) )

   read ( 1, iostat = stat, iomsg = msg )      cc1%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )          error stop 3_4

   if ( ( cc1%b1(1)%c /= 'abc' ) .or. ( cc1%b1(2)%c /= 'def' ) .or. ( cc1%b1(3)%c /= 'ghi' ) )    error stop 4_4

   read ( 1, iostat = stat, iomsg = msg )     cc2%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )          error stop 5_4

   if ( ( cc2%b1(1)%c /= 'ABC' ) .or. ( cc2%b1(2)%c /= 'DEF' ) .or. ( cc2%b1(3)%c /= 'GHI' ) .or. ( cc2%b1(4)%c /= 'JKL' ) )  error stop 6_4

   deallocate ( cc2 )
   cc1 = container( (/ ( child('xxx',-999) , i=1,3 ) /) )
   allocate ( cc2, source = container( (/ ( child('xxx',-999) , i=1,4 ) /) ) )

   read ( 1, iostat = stat, iomsg = msg )     cc1%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )                         error stop 7_4

   select type ( g => cc1%b1 )
      type is ( child )
        if ( ( g(1)%c /= 'jkl' ) .or. ( g(1)%i /= 1001 ) .or. &
             ( g(2)%c /= 'mno' ) .or. ( g(2)%i /= 10002 ) .or. &
             ( g(3)%c /= 'pqr' ) .or. ( g(3)%i /= 100003 ) )                error stop 8_4
   end select

   read ( 1, iostat = stat, iomsg = msg )    cc2%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )                                  error stop 9_4

   select type ( g => cc2%b1 )
      type is ( child )
        if ( ( g(1)%c /= 'MNO' ) .or. ( g(1)%i /= 21 )  .or. &
             ( g(2)%c /= 'PQR' ) .or. ( g(2)%i /= 202 ) .or. &
             ( g(3)%c /= 'STU' ) .or. ( g(3)%i /= 2003 ) .or. &
             ( g(4)%c /= 'VWX' ) .or. ( g(4)%i /= 20004 ) )                 error stop 10_4
   end select

   close ( 1, status ='delete')

end program
