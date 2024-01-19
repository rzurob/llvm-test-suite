!*  ===================================================================
!*
!*  DATE                       : 2007-08-13 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - scalar non-polymorphic derived type entity without DTIO
!*                                    containing components which has DTIO procedure with formatted I/O
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

   type container (kcontainer_1,lcontainer_1) ! kcontainer_1,lcontainer_1=4,3
      integer, kind :: kcontainer_1
      integer, len :: lcontainer_1
      integer(kcontainer_1) :: i = -999
      type(base(lcontainer_1)) :: b! = base(lcontainer_1)() ! tcx: (lcontainer_1) ! tcx: (lcontainer_1)
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

end module

program scalar002kl
   use m

   type(container(4,3))              :: c1 = container(4,3) ( 1001, base(3)('abc') ) ! tcx: (3) ! tcx: (4,3) ! tcx: (4,3)
   type(container(4,:)), allocatable :: c2 ! tcx: (4,:)

   integer :: stat
   character(200) :: msg = ''

   allocate ( c2, source = container(4,3) ( 1002, base(3)('def') ) ) ! tcx: (3) ! tcx: (4,3)

   open ( 1, file = 'scalar002kl.1', form='formatted', access='sequential' )

   write ( 1, *, iostat = stat, iomsg = msg )                 c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )          error stop 2_4

   write ( 1, "(I5,1X,DT)", iostat = stat, iomsg = msg )      c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )          error stop 3_4

   rewind 1

   c1 = container(4,3)(b=base(3)()) ! tcx: (4,3)
   c2 = container(4,3)(b=base(3)()) ! tcx: (4,3)

   read ( 1, *, iostat = stat, iomsg = msg )                  c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )          error stop 6_4

   read ( 1, "(I5,1X,DT)", iostat = stat, iomsg = msg )       c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )          error stop 7_4

   if ( ( c1%i /= 1002 ) .or. ( c1%b%c /= 'def' ) .or. &
        ( c2%i /= 1001 ) .or. ( c2%b%c /= 'abc' ) )           error stop 8_4

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 6 changes
! type: container - added parameters (kcontainer_1,lcontainer_1) to invoke with (4,3) / declare with (4,*) - 6 changes
