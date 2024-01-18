!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - scalar non-polymorphic derived type entity without DTIO
!*                                    containing components which has DTIO procedure with unformatted I/O
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

   type container
      integer :: i = -999
      type(base) :: b = base()
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

end module

program scalar002a
   use m

   type(container)              :: c1 = container ( 1001, base('abc') )
   type(container), allocatable :: c2

   integer :: stat
   character(200) :: msg = ''

   allocate ( c2, source = container ( 1002, base('def') ) )

   open ( 1, file = 'scalar002a.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg )                 c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )          error stop 2_4

   write ( 1, iostat = stat, iomsg = msg )      c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )          error stop 3_4

   rewind 1

   c1 = container()
   c2 = container()

   read ( 1, iostat = stat, iomsg = msg )                  c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )          error stop 6_4

   read ( 1, iostat = stat, iomsg = msg )       c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )          error stop 7_4

   if ( ( c1%i /= 1002 ) .or. ( c1%b%c /= 'def' ) .or. &
        ( c2%i /= 1001 ) .or. ( c2%b%c /= 'abc' ) )          error stop 8_4

   close ( 1, status ='delete')

end program
