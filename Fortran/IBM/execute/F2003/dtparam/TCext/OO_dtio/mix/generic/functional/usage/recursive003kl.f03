!*  ===================================================================
!*
!*  DATE                       : 2007-08-13 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - Recursive Formatted I/O with linked-list that contains
!*                                    components which requires DTIO
!*                               adaptation: exposed lengths
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

   type item (litem_1) ! litem_1=3
      integer, len :: litem_1
      character(litem_1) :: c = 'xxx'
      contains
         procedure, pass :: write => writei
         procedure, pass :: read => readi
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      type(item(lbase_1)) :: i! = item(lbase_1)() ! tcx: (lbase_1) ! tcx: (lbase_1)
      type(base(:)), pointer :: next => null() ! tcx: (:)
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   contains

      subroutine writei (dtv, unit, iotype, v_list, iostat, iomsg)
         class(item(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3)", iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtiowritei'

      end subroutine

      subroutine readi (dtv, unit, iotype, v_list, iostat, iomsg)
         class(item(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3)" , iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadi'

      end subroutine

      recursive subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(DT)", iostat=iostat, iomsg=iomsg) dtv%i
         if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowritei' ) )    error stop 101_4

         if ( associated(dtv%next) ) then
            write (unit, "(DT)", iostat=iostat, iomsg=iomsg)    dtv%next
            if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowriteb' ) ) error stop 102_4
         end if

         iomsg = 'dtiowriteb'

      end subroutine

      recursive subroutine readb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(DT)" , iostat=iostat, iomsg=iomsg) dtv%i
         if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioreadi' ) )     error stop 103_4
         if ( associated(dtv%next) ) then
            read (unit, "(DT)", iostat=iostat, iomsg=iomsg)     dtv%next
            if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioreadb' ) )  error stop 104_4
         end if

         iomsg = 'dtioreadb'

      end subroutine

end module

program recursive003kl
   use m

   class(base(:)), allocatable, target :: b1 ! tcx: (:)
   type(base(3)), target               :: b2 ! tcx: (3)

   type(base(:)), pointer :: dummy ! tcx: (:)

   namelist /n1/ b1

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = base(3) ( item(3)('ABC') ) ) ! tcx: (3) ! tcx: (3)
   allocate ( b1%next, source = base(3) ( item(3)('DEF') ) ) ! tcx: (3) ! tcx: (3)
   allocate ( b1%next%next, source = base(3) ( item(3)('GHI') ) ) ! tcx: (3) ! tcx: (3)
   allocate ( b1%next%next%next, source = base(3) ( item(3)('JKL') ) ) ! tcx: (3) ! tcx: (3)
   allocate ( b1%next%next%next%next, source = base(3) ( item(3)('MNO') ) ) ! tcx: (3) ! tcx: (3)
   allocate ( b1%next%next%next%next%next, source = base(3) ( item(3)('PQR') ) ) ! tcx: (3) ! tcx: (3)

   b2 = base(3)(item(3)('abc')) ! tcx: (3) ! tcx: (3)
   b2%next => b1%next

   open ( 1, file='recursive003kl.1', form='formatted', access='sequential' )

   write ( 1, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )   error stop 3_4

   write ( 1, *, iostat = stat, iomsg = msg )          b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )   error stop 4_4

   allocate ( b2%next, source = base(3)(item(3)('def')) ) ! tcx: (3) ! tcx: (3)
   allocate ( b2%next%next, source = base(3)(item(3)('ghi')) ) ! tcx: (3) ! tcx: (3)
   allocate ( b2%next%next%next, source = base(3)(item(3)('jkl')) ) ! tcx: (3) ! tcx: (3)
   allocate ( b2%next%next%next%next, source = base(3)(item(3)('mno')) ) ! tcx: (3) ! tcx: (3)
   allocate ( b2%next%next%next%next%next, source = base(3)(item(3)('pqr')) ) ! tcx: (3) ! tcx: (3)

   write ( 1, "(DT,DT)", iostat = stat, iomsg = msg )  b1, b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )   error stop 5_4

   rewind 1

   dummy => b1
   do while ( associated (dummy) )
      dummy%i%c = 'xxx'
      dummy => dummy%next
   end do

   dummy => b2
   do while ( associated (dummy) )
      dummy%i%c = 'xxx'
      dummy => dummy%next
   end do

   read ( 1, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )    error stop 6_4

   dummy => b1
   do while ( associated (dummy) )
      print *,dummy%i%c
      dummy => dummy%next
   end do

   read ( 1, "(1X,DT)", iostat = stat, iomsg = msg )   b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )    error stop 7_4

   dummy => b1
   do while ( associated (dummy) )
      print *,dummy%i%c
      dummy => dummy%next
   end do

   read ( 1, "(DT,DT)", iostat = stat, iomsg = msg )   b2, b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )    error stop 8_4

   dummy => b1
   do while ( associated (dummy) )
      print *,dummy%i%c
      dummy => dummy%next
   end do

   dummy => b2
   do while ( associated (dummy) )
      print *,dummy%i%c
      dummy => dummy%next
   end do

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: item - added parameters (litem_1) to invoke with (3) / declare with (*) - 16 changes
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 18 changes