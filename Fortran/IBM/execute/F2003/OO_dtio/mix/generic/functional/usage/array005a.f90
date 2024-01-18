!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - array (non-) polymorphic derived type variable
!*                                    with unformatted I/O with io-implied-do
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
         generic :: read(unformatted)  => read
   end type

   type, extends(base) :: child
      integer(4) :: i = -999
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
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

program array005
   use m

   class(base), allocatable :: b1(:)
   class(child), pointer     :: c1(:,:)

   integer :: stat
   character(200) :: msg

   allocate ( b1(3), source = (/ base ('abc'), base ('def'), base('ghi') /) )
   allocate ( c1(2,2), source = reshape ( source = (/ child ('abc',101 ), child ('def',102 ), child ('ghi',103 ), child ('jkl',104 ) /) , shape = (/2,2/) ) )

   open ( 1, file = 'array005.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg )                 (b1(i),i = 1,3)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )       error stop 1_4

   write ( 1, iostat = stat, iomsg = msg )         (c1(1,i),c1(2,i),i=1,2)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )       error stop 2_4

   rewind 1

   deallocate ( b1, c1 )
   allocate ( base :: b1(3) )
   allocate ( child :: c1(2,2) )

   read ( 1, iostat = stat, iomsg = msg )                  (b1(j), j = 3,1,-1)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )           error stop 3_4
   if ( ( b1(1)%c /= 'ghi' ) .or. &
        ( b1(2)%c /= 'def' ) .or. &
        ( b1(3)%c /= 'abc' ) )                                error stop 4_4

   read ( 1, iostat = stat, iomsg = msg )          (c1(1,i),c1(2,i),i=1,2)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )           error stop 5_4

   if ( ( c1(1,1)%c /= 'abc' ) .or. ( c1(1,1)%i /= 101 ) .or. &
        ( c1(2,1)%c /= 'def' ) .or. ( c1(2,1)%i /= 102 ) .or. &
        ( c1(1,2)%c /= 'ghi' ) .or. ( c1(1,2)%i /= 103 ) .or. &
        ( c1(2,2)%c /= 'jkl' ) .or. ( c1(2,2)%i /= 104 ))     error stop 6_4

   close ( 1, status ='delete')

end program
