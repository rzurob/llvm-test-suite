!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar005kl
!*
!*  PROGRAMMER                 : David Forster (derived from scalar005 by Robert Ma)
!*  DATE                       : 2007-08-13 (original: 04/26/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - scalar variables with io-implied-do
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

   type newbase (knewbase_1) ! knewbase_1=4
      integer, kind :: knewbase_1
      real(knewbase_1) :: r =  -999.0
      contains
      procedure, pass :: write => writen
      procedure, pass :: read => readn
      generic :: write(formatted) => write
      generic :: read(formatted)  => read
   end type

   contains

      subroutine writen (dtv, unit, iotype, v_list, iostat, iomsg)
         class(newbase(4)), intent(in) :: dtv ! tcx: (4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(F8.3)", iostat=iostat, iomsg=iomsg) dtv%r

         iomsg = 'dtiowriten'

      end subroutine

      subroutine readn (dtv, unit, iotype, v_list, iostat, iomsg)
         class(newbase(4)), intent(inout) :: dtv ! tcx: (4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(F8.3)" , iostat=iostat, iomsg=iomsg) dtv%r
         iomsg = 'dtioreadn'

      end subroutine

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

program scalar005kl
   use m

   integer :: stat
   character(200) :: msg

   class(base(:)), allocatable :: b1, b2 ! tcx: (:)
   class(newbase(4)), pointer  :: n1, n2 ! tcx: (4)

   logical :: precision_r4

   open ( 1, file = 'scalar005kl.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(3) ('abc') ) ! tcx: (3)
   allocate ( b2, source = child(3,4)('def',101) ) ! tcx: (3,4)
   allocate ( n1, source = newbase(4) (1.0) ) ! tcx: (4)
   allocate ( n2, source = newbase(4) (2.0) ) ! tcx: (4)

   write ( 1, "(DT,2(DT),DT)", iostat = stat, iomsg = msg )   ( b1, n1, i = -5, -4 )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriten' ) )        error stop 101_4

   write ( 1, *, iostat = stat, iomsg = msg )   ( n2, b2, i =100,99,-1 )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )        error stop 2_4

   rewind 1

   deallocate ( b1, b2, n1, n2 )
   allocate (base(3):: b1 ) ! tcx: base(3)
   allocate ( n1, n2 ) ! tcx: newbase(4)
   allocate ( child(3,4) :: b2 ) ! tcx: (3,4)

   read ( 1, "(4(DT))", iostat = stat, iomsg = msg )         ( b1, n1, i = -5, -4 )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadn' ) )        error stop 3_4

   read ( 1, "(4(1X,DT))", iostat = stat, iomsg = msg )                 ( n2, b2, i =100,99,-1 )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )        error stop 4_4

   select type ( b2 )
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b1%c /= 'abc' ) .or. ( b2%c /= 'def' ) .or. ( b2%i /= 101 ) .or. &
              ( .not. precision_r4(n1%r, 1.0) ) .or. ( .not. precision_r4(n2%r, 2.0) ) ) error stop 5_4
   end select

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 4 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 5 changes
! type: newbase - added parameters (knewbase_1) to invoke with (4) / declare with (4) - 5 changes
