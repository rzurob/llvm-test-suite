!*  ===================================================================
!*
!*  DATE                       : 2007-08-13 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - array derived type containing private components
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

program array008akl
   use m

   class(base(:)) , allocatable :: b1(:) ! tcx: (:)
   class(child(:,4)), pointer     :: c1(:,:) ! tcx: (:,4)

   integer :: stat
   character(200) :: msg

   open ( 1, file = 'array008akl.1', form='unformatted', access='sequential' )

   allocate (base(3):: b1(3) ) ! tcx: base(3)
   allocate (child(3,4):: c1(2,2) ) ! tcx: child(3,4)

   call b1(1)%setc('abc')
   call b1(2)%setc('def')
   call b1(3)%setc('ghi')

   call c1(1,1)%setc('ABC')
   call c1(1,1)%seti(101)
   call c1(2,1)%setc('DEF')
   call c1(2,1)%seti(102)
   call c1(1,2)%setc('GHI')
   call c1(1,2)%seti(103)
   call c1(2,2)%setc('JKL')
   call c1(2,2)%seti(104)

   write ( 1, iostat = stat, iomsg = msg ) b1, c1

   deallocate ( b1 )
   allocate ( child(3,4) :: b1(4) ) ! tcx: (3,4)

   select type ( b1 )
      type is ( child(*,4) ) ! tcx: (*,4)
         call b1(1)%setc('ABC')
         call b1(1)%seti(201)
         call b1(2)%setc('DEF')
         call b1(2)%seti(202)
         call b1(3)%setc('GHI')
         call b1(3)%seti(203)
         call b1(4)%setc('JKL')
         call b1(4)%seti(204)
   end select

   write ( 1, iostat = stat, iomsg = msg ) b1

   rewind 1

   deallocate ( b1, c1 )
   allocate (base(3):: b1(3) ) ! tcx: base(3)
   allocate (child(3,4):: c1(2,2) ) ! tcx: child(3,4)

   read ( 1, iostat = stat, iomsg = msg ) b1, c1

   if ( ( b1(1)%getc() /= 'abc' ) .or. ( b1(2)%getc() /= 'def' ) .or. ( b1(3)%getc() /= 'ghi' ) .or. &
        ( c1(1,1)%getc() /= 'ABC' ) .or. ( c1(1,1)%geti() /= 101 ) .or. &
        ( c1(2,1)%getc() /= 'DEF' ) .or. ( c1(2,1)%geti() /= 102 ) .or. &
        ( c1(1,2)%getc() /= 'GHI' ) .or. ( c1(1,2)%geti() /= 103 ) .or. &
        ( c1(2,2)%getc() /= 'JKL' ) .or. ( c1(2,2)%geti() /= 104 ) &
        ) error stop 101_4

   deallocate ( b1 )
   allocate ( child(3,4) :: b1(4) ) ! tcx: (3,4)

   read ( 1, iostat = stat, iomsg = msg )  b1

   select type ( b1 )
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b1(1)%getc() /= 'ABC' ) .or. ( b1(1)%geti() /= 201 ) .or. &
              ( b1(2)%getc() /= 'DEF' ) .or. ( b1(2)%geti() /= 202 ) .or. &
              ( b1(3)%getc() /= 'GHI' ) .or. ( b1(3)%geti() /= 203 ) .or. &
              ( b1(4)%getc() /= 'JKL' ) .or. ( b1(4)%geti() /= 204 ) ) error stop 2_4
   end select

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 5 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 9 changes
