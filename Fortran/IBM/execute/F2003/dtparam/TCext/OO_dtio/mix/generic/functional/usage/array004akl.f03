!*  ===================================================================
!*
!*  DATE                       : 2007-08-12 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - array unlimited polymorphic derived type variable
!*                                    with unformatted I/O
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
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i = -999
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
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

      subroutine readb (dtv, unit, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadb'

      end subroutine

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iostat, iomsg)
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtioreadc'

      end subroutine

end module

program array004akl
   use m

   class(*), allocatable :: b1(:)
   class(*), pointer     :: c1(:,:)

   integer :: stat
   character(200) :: msg

   allocate ( b1(3), source = (/ base(3) ('abc'), base(3) ('def'), base(3)('ghi') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( c1(2,2), source = reshape ( source = (/ child(3,4) ('abc',101 ), child(3,4) ('def',102 ), child(3,4) ('ghi',103 ), child(3,4) ('jkl',104 ) /) , shape = (/2,2/) ) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)

   open ( 1, file = 'array004akl.1', form='unformatted', access='sequential' )

   select type ( b1 )
      class is ( base(*) ) ! tcx: (*)
         write ( 1, iostat = stat, iomsg = msg )                 b1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )          error stop 101_4
   end select


   select type ( c1 )
      class is ( base(*) ) ! tcx: (*)
         write ( 1, iostat = stat, iomsg = msg )                 c1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )          error stop 2_4
   end select

   rewind 1

   deallocate ( b1, c1 )
   allocate ( base(3) :: b1(3) ) ! tcx: (3)
   allocate ( child(3,4) :: c1(2,2) ) ! tcx: (3,4)

   select type ( b1 )
      type is ( base(*) ) ! tcx: (*)
         read ( 1, iostat = stat, iomsg = msg )                  b1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )           error stop 3_4
         if ( ( b1(1)%c /= 'abc' ) .or. &
              ( b1(2)%c /= 'def' ) .or. &
              ( b1(3)%c /= 'ghi' ) )                                error stop 4_4
   end select

   select type ( c1 )
      type is ( child(*,4) ) ! tcx: (*,4)
         read ( 1, iostat = stat, iomsg = msg )          c1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )           error stop 5_4
         if ( ( c1(1,1)%c /= 'abc' ) .or. ( c1(1,1)%i /= 101 ) .or. &
              ( c1(2,1)%c /= 'def' ) .or. ( c1(2,1)%i /= 102 ) .or. &
              ( c1(1,2)%c /= 'ghi' ) .or. ( c1(1,2)%i /= 103 ) .or. &
              ( c1(2,2)%c /= 'jkl' ) .or. ( c1(2,2)%i /= 104 ))     error stop 6_4
   end select

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 9 changes
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 9 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 8 changes