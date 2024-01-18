!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : resolve006akl
!*
!*  PROGRAMMER                 : David Forster (derived from resolve006a by Robert Ma)
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
!*  DESCRIPTION                : Section 9.5.3.7.3 Resolving derived-type input/output procedure references (generic binding)
!*                                    - Make both generic type bound and interface available
!*                                        - for unformatted I/O with single non-extended type, recursive I/O
!*                                          which contains dtio interface (which shall never be called)
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
      type(base(:)), pointer :: next => null() ! tcx: (:)
      contains
         procedure, pass :: write => writebase
         procedure, pass :: read => readbase
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   contains

      recursive subroutine writebase (dtv, unit, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         interface write(unformatted)
            subroutine writebaseext (dtv, unit, iostat, iomsg)
               import base
               class(base(*)), intent(in) :: dtv ! tcx: (*)
               integer, intent(in) :: unit
               integer, intent(out) :: iostat
               character(*), intent(inout) :: iomsg
            end subroutine
         end interface

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c

         if ( associated ( dtv%next ) ) then
            write (unit, iostat = iostat, iomsg = iomsg ) dtv%next
         end if

         iomsg = 'dtiowrite'

      end subroutine

      recursive subroutine readbase (dtv, unit, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         interface read(unformatted)
            subroutine readbaseext (dtv, unit, iostat, iomsg)
               import base
               class(base(*)), intent(inout) :: dtv ! tcx: (*)
               integer, intent(in) :: unit
               integer, intent(out) :: iostat
               character(*), intent(inout) :: iomsg
            end subroutine
         end interface

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c

         if ( associated ( dtv%next ) ) then
            read (unit, iostat = iostat, iomsg = iomsg ) dtv%next
         end if

         iomsg = 'dtioread'

      end subroutine

end module

program resolve006akl
   use m

   class(base(:)), allocatable :: b1 ! tcx: (:)
   type(base(:)) , pointer     :: b2, dummy ! tcx: (:)

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = base(3) ( 'abc' ) ) ! tcx: (3)
   allocate ( b1%next, source = base(3) ('def') ) ! tcx: (3)
   allocate ( b1%next%next, source = base(3) ('ghi') ) ! tcx: (3)
   allocate ( b1%next%next%next, source = base(3) ('jkl') ) ! tcx: (3)
   allocate ( b1%next%next%next%next, source = base(3) ('mno') ) ! tcx: (3)
   allocate ( b1%next%next%next%next%next, source = base(3) ('pqr') ) ! tcx: (3)

   allocate ( b2, source = base(3) () ) ! tcx: (3)
   allocate ( b2%next, source = base(3) () ) ! tcx: (3)
   allocate ( b2%next%next, source = base(3) () ) ! tcx: (3)
   allocate ( b2%next%next%next, source = base(3) () ) ! tcx: (3)
   allocate ( b2%next%next%next%next, source = base(3) () ) ! tcx: (3)
   allocate ( b2%next%next%next%next%next, source = base(3) () ) ! tcx: (3)

   open ( 1, file = 'resolve006akl.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg ) b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   
   rewind 1
   
   read ( 1, iostat = stat, iomsg = msg )  b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )  error stop 2_4
   
   dummy => b2
   
   do while ( associated(dummy ) )
      print *, dummy%c
      dummy => dummy%next   
   end do
   
   close ( 1, status ='delete')

end program

subroutine writebaseext (dtv, unit, iostat, iomsg)
   use m
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   error stop 10_4
   iomsg = 'ERROR'

end subroutine

subroutine readbaseext (dtv, unit, iostat, iomsg)
   use m
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   error stop 11_4
   iomsg = 'ERROR'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 21 changes
