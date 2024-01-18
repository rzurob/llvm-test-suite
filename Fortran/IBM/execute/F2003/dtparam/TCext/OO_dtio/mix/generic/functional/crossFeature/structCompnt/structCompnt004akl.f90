!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : structCompnt004akl
!*
!*  PROGRAMMER                 : David Forster (derived from structCompnt004a by Robert Ma)
!*  DATE                       : 2007-08-08 (original: 04/26/2005)
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
!*                                  Cross Feature: Structure Component
!*                                    -  non-polymorphic structure array component with unformatted i/o
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
         generic :: write(unformatted) => write
         generic :: read(unformatted) => read
   end type

   type container (lcontainer1) ! lcontainer1=3
      integer, len :: lcontainer1
      type(base(lcontainer1)) :: b1(lcontainer1) ! tcx: (lcontainer1)
      type(base(:)), allocatable :: b2(:) ! tcx: (:)
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

end module

program structCompnt004akl
   use m

   integer :: stat
   character(200) :: msg

   type(container(3)) :: cc1 ! tcx: (3)
   class(container(:)), allocatable :: cc2 ! tcx: (:)

   open ( 1, file = 'structCompnt004akl.1', form='unformatted', access='sequential' )

   cc1 = container(3)( (/ base(3)('abc'), base(3)('def'), base(3)('ghi') /), (/ base(3)('ABC'), base(3)('DEF') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( cc2, source = container(3)( (/ base(3)('101'), base(3)('102'), base(3)('103') /), (/ base(3)('201'), base(3)('202'), base(3)('203') /) ) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   write ( 1, iostat = stat, iomsg = msg )     cc1%b1, cc1%b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )         error stop 1_4

   write ( 1, iostat = stat, iomsg = msg )     cc2%b1, cc2%b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )         error stop 2_4

   rewind 1

   deallocate ( cc2 )
   cc1 = container(3)( (/ ( base(3)('xxx') , i=1,3 )  /), (/ ( base(3)('xxx') , i=1,2)  /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( cc2, source = container(3)( (/ ( base(3)('xxx') , i=1,3 )  /), (/ ( base(3)('xxx') , i=1,3)  /) ) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   read ( 1, iostat = stat, iomsg = msg )      cc1%b1, cc1%b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )          error stop 3_4

   if ( ( cc1%b1(1)%c /= 'abc' ) .or. ( cc1%b1(2)%c /= 'def' ) .or. ( cc1%b1(3)%c /= 'ghi' ) .or. &
        ( cc1%b2(1)%c /= 'ABC' ) .or. ( cc1%b2(2)%c /= 'DEF' ) )                                  error stop 4_4

   read ( 1, iostat = stat, iomsg = msg )      cc2%b1, cc2%b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )          error stop 5_4

   if ( ( cc2%b1(1)%c /= '101' ) .or. ( cc2%b1(2)%c /= '102' ) .or. ( cc2%b1(3)%c /= '103' ) .or. &
        ( cc2%b2(1)%c /= '201' ) .or. ( cc2%b2(2)%c /= '202' ) .or. ( cc2%b2(3)%c /= '203' ) )    error stop 5_4

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase1) to invoke with (3) / declare with (*) - 19 changes
! type: container - added parameters (lcontainer1) to invoke with (3) / declare with (*) - 6 changes
