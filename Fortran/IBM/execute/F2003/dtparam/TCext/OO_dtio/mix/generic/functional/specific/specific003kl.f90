!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : specific003kl
!*
!*  PROGRAMMER                 : David Forster (derived from specific003 by Robert Ma)
!*  DATE                       : 2007-08-09 (original: 04/26/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic-binding)
!*                                 - Specific Binding
!*                                    - specific binding is a overriding binding
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
         procedure, pass :: write => writebase
         procedure, pass :: read => readbase
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i = -999
      contains
         procedure, pass :: write => writechild
         procedure, pass :: read => readchild
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   contains

      subroutine writebase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         !  no implementation

      end subroutine

      subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         !  no implementation

      end subroutine

      subroutine writechild (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtiowrite'
      end subroutine

      subroutine readchild (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,1X,I4)" , iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtioread'
      end subroutine

end module

program specific003kl
   use m

   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer     :: b2 ! tcx: (:)

   class(child(:,4)), allocatable :: c1 ! tcx: (:,4)
   type(child(3,4))               :: c2 = child(3,4) ('jkl', 1004) ! tcx: (3,4) ! tcx: (3,4)

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = child(3,4) ( 'abc', 1001 ) ) ! tcx: (3,4)
   allocate ( b2, source = child(3,4) ( 'def', 1002 ) ) ! tcx: (3,4)
   allocate ( c1, source = child(3,4) ( 'ghi', 1003 ) ) ! tcx: (3,4)

   open ( 1, file = 'specific003kl.1', form='formatted', access='sequential' )

   select type ( g => b1 )
      class is (child(*,4)) ! tcx: (*,4)
         write ( 1, "(DT)", iostat = stat, iomsg = msg )    g
   end select
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   
   select type ( b2 )
      type is (child(*,4)) ! tcx: (*,4)
         write ( 1, "(DT)", iostat = stat, iomsg = msg )    b2
   end select
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4
   
   write ( 1, "(2(DT,:,/))", iostat = stat, iomsg = msg )    c1, c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

   rewind 1

   select type ( b1 )
      type is ( child(*,4) ) ! tcx: (*,4)
         select type ( b2 )
            class is ( child(*,4) ) ! tcx: (*,4)
               read ( 1, "(DT)", iostat = stat, iomsg = msg )  c2, c1, b2, b1
         end select
   end select
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )      error stop 4_4
   
   select type ( b1 )
      type is (child(*,4)) ! tcx: (*,4)
        if ( ( b1%c /= 'jkl' ) .or. ( b1%i /= 1004 ) )  error stop 5_4
   end select

   select type ( b2 )
      type is (child(*,4)) ! tcx: (*,4)
        if ( ( b2%c /= 'ghi' ) .or. ( b2%i /= 1003 ) )  error stop 6_4
   end select
   if ( ( c1%c /= 'def' ) .or. ( c1%i /= 1002 ) )       error stop 7_4
   if ( ( c2%c /= 'abc' ) .or. ( c2%i /= 1001 ) )       error stop 8_4

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 4 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 14 changes
