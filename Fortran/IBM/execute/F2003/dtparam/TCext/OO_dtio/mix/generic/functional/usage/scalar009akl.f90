!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar009akl
!*
!*  DATE                       : 2007-08-13 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - scalar derived type containing private components
!*                                    with unformatted I/O
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
      character(lbase_1), private :: c = 'xxx'
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read

         procedure, pass :: getc
         procedure, pass :: setc

   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1), private :: i = -999
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc

         procedure, pass :: geti
         procedure, pass :: seti

   end type

   contains

      subroutine writeb (dtv, unit, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%getc()

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readb (dtv, unit, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(3) :: c
         read (unit, iostat=iostat, iomsg=iomsg) c
         call dtv%setc(c)

         iomsg = 'dtioreadb'

      end subroutine

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%getc(), dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iostat, iomsg)
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(3) :: c
         read (unit, iostat=iostat, iomsg=iomsg) c, dtv%i

         call dtv%setc(c)

         iomsg = 'dtioreadc'

      end subroutine

      character(3) function getc(dtv)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         getc = dtv%c
      end function

      subroutine setc(dtv,cc)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         character(3), intent(in) :: cc
         dtv%c = cc
      end subroutine

      integer(4) function geti(dtv)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         geti = dtv%i
      end function

      subroutine seti(dtv,ii)
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer(4), intent(in) :: ii
         dtv%i = ii
      end subroutine

end module

program scalar009akl
   use m

   class(base(:)) , allocatable :: b1 ! tcx: (:)
   class(child(:,4)), pointer     :: c1 ! tcx: (:,4)

   integer :: stat
   character(200) :: msg

   open ( 1, file = 'scalar009akl.1', form='unformatted', access='sequential' )

   allocate (base(3):: b1 ) ! tcx: base(3)
   allocate (child(3,4):: c1 ) ! tcx: child(3,4)

   call b1%setc('abc')
   call c1%setc('def')
   call c1%seti(1001)

   write ( 1, iostat = stat, iomsg = msg ) b1, c1

   deallocate ( b1 )
   allocate ( child(3,4) :: b1 ) ! tcx: (3,4)

   select type ( b1 )
      type is ( child(*,4) ) ! tcx: (*,4)
         call b1%setc('ghi')
         call b1%seti(1002)
   end select

   write ( 1, iostat = stat, iomsg = msg ) b1

   rewind 1

   deallocate ( b1, c1 )

   allocate (base(3):: b1 ) ! tcx: base(3)
   allocate (child(3,4):: c1 ) ! tcx: child(3,4)

   read ( 1, iostat = stat, iomsg = msg ) b1, c1

   if ( ( b1%getc() /= 'abc' ) .or. ( c1%getc() /= 'def' ) .or. ( c1%geti() /= 1001 ) ) error stop 101_4

   deallocate ( b1 )
   allocate ( child(3,4) :: b1 ) ! tcx: (3,4)

   read ( 1, iostat = stat, iomsg = msg )  b1

   select type ( b1 )
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b1%getc() /= 'ghi' ) .or. ( b1%geti() /= 1002 ) ) error stop 2_4
   end select

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 5 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 9 changes
