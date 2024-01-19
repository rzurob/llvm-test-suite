!*  ===================================================================
!*
!*  DATE                       : 2007-08-13 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - array derived type parameter
!*                                    with formatted I/O
!*                               adaptation: exposed kind, length
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

   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = 'xxx'
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i = -999
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   contains

      subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3)", iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3)" , iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadb'

      end subroutine

      subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,1X,I4)" , iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtioreadc'

      end subroutine

end module

program array007kl
   use m

   type(base(3)), parameter  :: b1(3)   = (/ base(3)('aaa'), base(3)('bbb'), base(3)('ccc') /) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   type(base(3)), parameter  :: b2(2,2) = reshape ( source= (/ base(3)('AAA'), base(3)('BBB'), base(3)('CCC'), base(3)('DDD') /), shape =(/2,2/) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   type(child(3,4)), parameter :: c1(4)   = (/ child(3,4)('abc',101), child(3,4)('def',102), child(3,4)('ghi',103), child(3,4)('jkl',104) /) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)
   type(child(3,4)), parameter :: c2(2,2) = reshape ( source= (/ child(3,4)('ABC',201), child(3,4)('DEF',202), child(3,4)('GHI',203), child(3,4)('JKL',204) /), shape =(/2,2/) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)

   type(base(3))  :: b11(3), b12(2,2) ! tcx: (3)
   type(child(3,4)) :: c11(4), c12(2,2) ! tcx: (3,4)

   integer :: stat
   character(200) :: msg

   open ( 1, file = 'array007kl.1', form='formatted', access='sequential' )

   write ( 1, "(7DT)", iostat = stat, iomsg = msg )     b1, c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )    error stop 101_4

   write ( 1, *, iostat = stat, iomsg = msg )           c2, b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )    error stop 2_4

   rewind 1

   read ( 1, "(7DT)", iostat = stat, iomsg = msg )            b11, c11
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )           error stop 3_4
   read ( 1, "(8(1x,DT))", iostat = stat, iomsg = msg )       c12, b12
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )           error stop 4_4

   if ( ( b11(1)%c /= 'aaa' ) .or. ( b11(2)%c /= 'bbb' ) .or. ( b11(3)%c /= 'ccc' ) .or. &
        ( b12(1,1)%c /= 'AAA' ) .or. ( b12(2,1)%c /= 'BBB' ) .or. ( b12(1,2)%c /= 'CCC' ) .or. ( b12(2,2)%c /= 'DDD' )  .or. &
        ( c11(1)%c /= 'abc' ) .or. ( c11(2)%c /= 'def' ) .or. ( c11(3)%c /= 'ghi' ) .or. ( c11(4)%c /= 'jkl' ) .or. &
        ( c11(1)%i /= 101 ) .or. ( c11(2)%i /= 102 ) .or. ( c11(3)%i /= 103 ) .or. ( c11(4)%i /= 104 ) .or. &
        ( c12(1,1)%c /= 'ABC' ) .or. ( c12(2,1)%c /= 'DEF' ) .or. ( c12(1,2)%c /= 'GHI' ) .or. ( c12(2,2)%c /= 'JKL' ) .or. &
        ( c12(1,1)%i /= 201 ) .or. ( c12(2,1)%i /= 202 ) .or. ( c12(1,2)%i /= 203 ) .or. ( c12(2,2)%i /= 204 ) &
        ) error stop 5_4

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 12 changes
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 12 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 13 changes
