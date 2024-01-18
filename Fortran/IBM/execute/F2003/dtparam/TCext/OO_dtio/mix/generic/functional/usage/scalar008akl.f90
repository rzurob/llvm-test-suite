!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar008akl
!*
!*  PROGRAMMER                 : David Forster (derived from scalar008a by Robert Ma)
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
!*                                  - scalar derived type parameter
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

program scalar008akl
   use m

   type(base(3)), parameter  :: b1 = base(3)('ibm') ! tcx: (3) ! tcx: (3)
   type(base(3)), parameter  :: b2 = base(3)('IBM') ! tcx: (3) ! tcx: (3)
   type(child(3,4)), parameter :: c1 = child(3,4)('ftn',2003) ! tcx: (3,4) ! tcx: (3,4)
   type(child(3,4)), parameter :: c2 = child(3,4)('FTN',2004) ! tcx: (3,4) ! tcx: (3,4)
   
   type(base(3))  :: b11, b12 ! tcx: (3)
   type(child(3,4)) :: c11, c12 ! tcx: (3,4)

   integer :: stat
   character(200) :: msg

   open ( 1, file = 'scalar008akl.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg )     b1, c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )   error stop 101_4

   write ( 1, iostat = stat, iomsg = msg )          c2, b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )   error stop 2_4

   rewind 1

   read ( 1, iostat = stat, iomsg = msg )            b11, c11
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )          error stop 3_4
   read ( 1, iostat = stat, iomsg = msg )   c12, b12
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )          error stop 4_4

   if ( ( b11%c /= 'ibm' ) .or. ( c11%c /= 'ftn' ) .or. ( c11%i /= 2003 ) .or. &
        ( b12%c /= 'IBM' ) .or. ( c12%c /= 'FTN' ) .or. ( c12%i /= 2004 ) ) error stop 5_4

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 7 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 7 changes
