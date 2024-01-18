!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : array009alkl
!*
!*  PROGRAMMER                 : David Forster (derived from array009a by Robert Ma)
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
!*                                  - array derived type with multiple level of heirarchy, and use parent component in child dtio
!*                                    with unformatted I/O
!*                               adaptation: exposed kind, lengths
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

   type, extends(child) :: gen3 (lgen3_1) ! lgen3_1=3
      integer, len :: lgen3_1
      character(lgen3_1) :: s = 'xxx'
      contains
         procedure, pass :: write => writeg
         procedure, pass :: read => readg
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

         write (unit, iostat=iostat, iomsg=iomsg) dtv%base, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iostat, iomsg)
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%base, dtv%i
         iomsg = 'dtioreadc'

      end subroutine

      subroutine writeg (dtv, unit, iostat, iomsg)
         class(gen3(*,4,*)), intent(in) :: dtv ! tcx: (*,4,*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%child, dtv%s

         iomsg = 'dtiowriteg'

      end subroutine

      subroutine readg (dtv, unit, iostat, iomsg)
         class(gen3(*,4,*)), intent(inout) :: dtv ! tcx: (*,4,*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%child, dtv%s
         iomsg = 'dtioreadg'

      end subroutine

end module

program array009alkl
   use m

   integer :: stat
   character(200) :: msg

   class(base(:)), allocatable :: b1(:) ! tcx: (:)
   class(child(:,4)), pointer    :: c1(:,:) ! tcx: (:,4)

   open ( 1, file = 'array009alkl.1', form='unformatted', access='sequential' )

   allocate ( b1(3), source = (/ child(3,4) ( 'abc', 101 ), child(3,4) ( 'def', 102 ), child(3,4) ( 'ghi', 103 ) /) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)
   allocate ( c1(2,2), source = reshape ( source = (/ gen3(3,4,3)( 'abc', 201, 'ABC' ), gen3(3,4,3)( 'def', 202, 'DEF' ), & ! tcx: (3,4,3) ! tcx: (3,4,3)
                                                      gen3(3,4,3)( 'ghi', 203, 'GHI' ), gen3(3,4,3)( 'jkl', 204, 'JKL' ) /), shape = (/2,2/))) ! tcx: (3,4,3) ! tcx: (3,4,3)

   write ( 1,  iostat = stat, iomsg = msg ) b1, c1

   deallocate ( b1 )
   allocate ( b1(2), source = (/ gen3(3,4,3)( 'abc', 301, 'ABC' ), gen3(3,4,3)( 'def', 302, 'DEF' ) /) ) ! tcx: (3,4,3) ! tcx: (3,4,3)

   write ( 1, iostat = stat, iomsg = msg ) b1

   rewind 1

   deallocate ( b1, c1 )
   allocate ( child(3,4) :: b1(3) ) ! tcx: (3,4)
   allocate ( gen3(3,4,3) :: c1(2,2) ) ! tcx: (3,4,3)

   read ( 1, iostat = stat, iomsg = msg )    b1, c1

   select type ( b1 )
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b1(1)%c /= 'abc' ) .or. ( b1(1)%i /= 101 ) .or. &
              ( b1(2)%c /= 'def' ) .or. ( b1(2)%i /= 102 ) .or. &
              ( b1(3)%c /= 'ghi' ) .or. ( b1(3)%i /= 103 ) ) error stop 101_4
   end select

   select type ( c1 )
      type is ( gen3(*,4,*) ) ! tcx: (*,4,*)
        if ( ( c1(1,1)%c /= 'abc' ) .or. ( c1(1,1)%i /= 201 ) .or. ( c1(1,1)%s /= 'ABC' ) .or. &
             ( c1(2,1)%c /= 'def' ) .or. ( c1(2,1)%i /= 202 ) .or. ( c1(2,1)%s /= 'DEF' ) .or. &
             ( c1(1,2)%c /= 'ghi' ) .or. ( c1(1,2)%i /= 203 ) .or. ( c1(1,2)%s /= 'GHI' ) .or. &
             ( c1(2,2)%c /= 'jkl' ) .or. ( c1(2,2)%i /= 204 ) .or. ( c1(2,2)%s /= 'JKL' )  ) error stop 2_4
   end select

   deallocate ( b1 )
   allocate ( gen3(3,4,3) :: b1(2) ) ! tcx: (3,4,3)

   read ( 1, iostat = stat, iomsg = msg ) b1

   select type ( b1 )
      type is ( gen3(*,4,*) ) ! tcx: (*,4,*)
        if ( ( b1(1)%c /= 'abc' ) .or. ( b1(1)%i /= 301 ) .or. ( b1(1)%s /= 'ABC' ) .or. &
             ( b1(2)%c /= 'def' ) .or. ( b1(2)%i /= 302 ) .or. ( b1(2)%s /= 'DEF' ) ) error stop 3_4
   end select

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 3 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 8 changes
! type: gen3 - added parameters (lgen3_1) to invoke with (3,4,3) / declare with (*,4,*) - 12 changes
