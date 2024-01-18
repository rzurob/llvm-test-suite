!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : array006bkl
!*
!*  PROGRAMMER                 : David Forster (derived from array006b by Robert Ma)
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
!*                                  - array variables of zero-sized
!*                                    with unformatted I/O
!*                               adaptation: superfluous kind, exposed length
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

   type base (kbase_1) ! kbase_1=4
      integer, kind :: kbase_1
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   type, extends(base) :: child (lchild_1) ! lchild_1=0
      integer, len :: lchild_1
      character(lchild_1) :: c = ''
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   integer :: idx

   contains

      subroutine writeb (dtv, unit, iostat, iomsg)
         class(base(4)), intent(in) :: dtv ! tcx: (4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtiowriteb'

         idx = idx + 1

      end subroutine

      subroutine readb (dtv, unit, iostat, iomsg)
         class(base(4)), intent(inout) :: dtv ! tcx: (4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtioreadb'

         idx = idx + 1

      end subroutine

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child(4,*)), intent(in) :: dtv ! tcx: (4,*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtiowritec'

         idx = idx + 1

      end subroutine

      subroutine readc (dtv, unit, iostat, iomsg)
         class(child(4,*)), intent(inout) :: dtv ! tcx: (4,*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         iomsg = 'dtioreadc'

         idx = idx + 1

      end subroutine

end module

program array006a
   use m

   integer :: stat
   character(200) :: msg = ''

   class(base(4)), allocatable :: b1(:), b2(:,:) ! tcx: (4)

   open ( 1, file = 'array006a.1', form='unformatted', access='sequential' )

   allocate ( b1(3) )
   allocate ( child(4,0) :: b2(2,2) ) ! tcx: (4,0)

   idx = 0

   write ( 1, iostat = stat, iomsg = msg )        b1(3:2)
   if ( ( stat /= 0 ) .or. ( msg /= '' ) .or. ( idx /= 0 ) ) error stop 101_4

   idx = 0

   write ( 1, iostat = stat, iomsg = msg )        b2(1:2:-1,2)
   if ( ( stat /= 0 ) .or. ( msg /= '' ) .or. ( idx /= 0 ) ) error stop 2_4

   rewind 1

   idx = 0

   read ( 1, iostat = stat, iomsg = msg )         b1(1:3:-1)
   if ( ( stat /= 0 ) .or. ( msg /= '' ) .or. ( idx /= 0 ) )  error stop 3_4

   idx = 0

   read ( 1, iostat = stat, iomsg = msg )         b2(2:1,1)
   if ( ( stat /= 0 ) .or. ( msg /= '' ) .or. ( idx /= 0 ) )  error stop 4_4

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
! type: child - added parameters (lchild_1) to invoke with (4,0) / declare with (4,*) - 3 changes
