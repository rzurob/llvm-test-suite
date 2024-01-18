!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : selectType003kl
!*
!*  PROGRAMMER                 : David Forster (derived from selectType003 by Robert Ma)
!*  DATE                       : 2007-08-07 (original: 04/26/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Select Type Construct
!*                                    -  selector is a unlimited polymorphic scalar/array entity with formatted i/o

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

   type base (lbase1) ! lbase1=3
      integer, len :: lbase1
      character(lbase1) :: c = 'xxx'
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type, extends(base) :: child (kchild1) ! kchild1=4
      integer, kind :: kchild1
      integer(kchild1) :: i = -999
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

         character(20) :: fmt

         write (fmt, *) "(A", v_list(1), ")"
         write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt

         write (fmt, *) "(A", v_list(1), ")"

         read (unit, fmt , iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadb'

      end subroutine

      subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt

         write (fmt, *) "(A", v_list(1), ",1X, I",v_list(2),")"

         write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt
         write (fmt, *) "(A", v_list(1), ",1X, I",v_list(2),")"

         read (unit, fmt , iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtioreadc'

      end subroutine

end module

program selectType003kl
   use m

   integer :: stat
   character(200) :: msg

   class(*), pointer     :: b1
   class(*), allocatable :: b2
   class(*), allocatable :: c1(:)

   allocate ( b1, source = base(3)('abc') ) ! tcx: (3)
   allocate ( b2, source = base(3)('ABC') ) ! tcx: (3)
   allocate ( c1(2), source = (/ child(3,4)('abc',10001 ), child(3,4)('def',100002 ) /) ) ! tcx: (3,4) ! tcx: (3,4)

   open ( 1, file = 'selectType003kl.1', form='formatted', access='stream' )

   select type ( g => b1 )
      class is ( base(*) ) ! tcx: (*)
         write ( 1, "(DT(3))", iostat = stat, iomsg = msg, pos = 1 )    g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4
   end select

   select type ( h => b2 )
      type is ( base(*) ) ! tcx: (*)
         write ( 1, "(DT(3))", iostat = stat, iomsg = msg, pos = 4 )    h
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 2_4
   end select

   select type ( i => c1 )
      class is ( base(*) ) ! tcx: (*)
         write ( 1, "(DT(3,5),DT(3,6))", iostat = stat, iomsg = msg, pos=7 )  i
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 3_4
   end select

   deallocate ( b1, b2, c1 )

   allocate ( base(3) :: b1, b2) ! tcx: (3)
   allocate ( child(3,4) :: c1(2) ) ! tcx: (3,4)

   select type ( g => b1 )
      class is ( base(*) ) ! tcx: (*)
         read ( 1, "(DT(3))", iostat = stat, iomsg = msg, pos = 1 )    g
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 4_4
         if ( g%c /= 'abc' ) error stop 5_4

   end select

   select type ( h => b2 )
      type is ( base(*) ) ! tcx: (*)
         read ( 1, "(DT(3))", iostat = stat, iomsg = msg, pos = 4 )    h
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 6_4
         if ( h%c /= 'ABC' ) error stop 7_4
   end select

   select type ( i => c1 )
      class is ( base(*) ) ! tcx: (*)
         read ( 1, "(DT(3,5),DT(3,6))", iostat = stat, iomsg = msg, pos=7 )  i
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 8_4
   end select

   select type ( j => c1 )
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( j(1)%c /= 'abc' ) .or.  ( j(2)%c /= 'def' ) .or. ( j(1)%i /= 10001 ) .or.  ( j(2)%i /= 100002 ) ) error stop 9_4

   end select

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase1) to invoke with (3) / declare with (*) - 11 changes
! type: child - added parameters (kchild1) to invoke with (3,4) / declare with (*,4) - 6 changes
