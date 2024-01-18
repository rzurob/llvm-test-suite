!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar007akkk
!*
!*  DATE                       : 2007-08-13 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Usage:
!*                                    -  Scalar entity containing sequence scalar/array components with unformatted i/o
!*                               adaptation: exposed kinds
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

   type data (kdata_1) ! kdata_1=8
      integer, kind :: kdata_1
      sequence
      integer(kdata_1) :: i = -999
   end type

   type base (kbase_1) ! kbase_1=8
      integer, kind :: kbase_1
      type(data(kbase_1)) :: s1 = data(kbase_1)() ! tcx: (kbase_1) ! tcx: (kbase_1)
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(unformatted) => write
         generic :: read(unformatted) => read
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=8
      integer, kind :: kchild_1
      type(data(kchild_1)), allocatable :: s2(:) ! tcx: (kchild_1)
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   contains

      subroutine writeb (dtv, unit, iostat, iomsg)
         class(base(8)), intent(in) :: dtv ! tcx: (8)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%s1

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child(8,8)), intent(in) :: dtv ! tcx: (8,8)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%s1, dtv%s2

         iomsg = 'dtiowritec'

      end subroutine

      subroutine readb (dtv, unit, iostat, iomsg)
         class(base(8)), intent(inout) :: dtv ! tcx: (8)
         integer, intent(in) :: unit

         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%s1

         iomsg = 'dtioreadb'

      end subroutine

      subroutine readc (dtv, unit, iostat, iomsg)
         class(child(8,8)), intent(inout) :: dtv ! tcx: (8,8)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%s1, dtv%s2

         iomsg = 'dtioreadc'

      end subroutine

end module

program scalar007akkk
   use m

   integer :: stat
   character(200) :: msg

   class(base(8)), allocatable :: b1 ! tcx: (8)
   class(base(8)), pointer     :: b2 ! tcx: (8)

   open ( 1, file = 'scalar007akkk.1', form='unformatted', access='sequential' )

   allocate ( b1, source = base(8) (data(8)(101)) ) ! tcx: (8) ! tcx: (8)
   allocate ( b2, source = child(8,8)(data(8)(201), (/ data(8)(20001), data(8)(20002), data(8)(20003) /) ) ) ! tcx: (8) ! tcx: (8) ! tcx: (8) ! tcx: (8) ! tcx: (8,8)

   write ( 1, iostat = stat, iomsg = msg )           b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )            error stop 101_4

   write ( 1, iostat = stat, iomsg = msg )         b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )            error stop 2_4

   rewind 1

   deallocate ( b1, b2 )
   allocate ( b1, source = base(8)() ) ! tcx: (8)
   allocate ( b2, source = child(8,8)(data(8)(),(/(data(8)(),i=1,3)/)) ) ! tcx: (8) ! tcx: (8) ! tcx: (8,8)

   read ( 1, iostat = stat, iomsg = msg )           b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )           error stop 3_4

   read ( 1, iostat = stat, iomsg = msg )           b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )           error stop 4_4

   if ( b1%s1%i /= 101 ) error stop 5_4

   select type ( b2 )
      type is ( child(8,8) ) ! tcx: (8,8)
         if ( ( b2%s1%i /= 201 ) .or. ( b2%s2(1)%i /= 20001 ) .or. ( b2%s2(2)%i /= 20002 ) .or. ( b2%s2(3)%i /= 20003 ) ) error stop 6_4
   end select

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: data - added parameters (kdata_1) to invoke with (8) / declare with (8) - 10 changes
! type: base - added parameters (kbase_1) to invoke with (8) / declare with (8) - 6 changes
! type: child - added parameters (kchild_1) to invoke with (8,8) / declare with (8,8) - 5 changes
