!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : bindingOrder001kl
!*
!*  PROGRAMMER                 : David Forster (derived from bindingOrder001 by Robert Ma)
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
!*  DESCRIPTION                : Miscellaneous:
!*                                 - reverse the order of declaration between generic and specific binding
!*                               adaptation: exposed length
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
         generic :: write(formatted) => write
         generic :: read(formatted)  => read   
         procedure, pass :: write => writebase
         procedure, pass :: read => readbase
   end type

   contains

      subroutine writebase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c

      end subroutine

      subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, *, iostat=iostat, iomsg=iomsg) dtv%c
      end subroutine

end module

program bindingOrder001kl
   use m

   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer     :: b2 ! tcx: (:)

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = base(3) ( 'ibm' ) ) ! tcx: (3)
   allocate ( b2, source = base(3) ( 'ftn' ) ) ! tcx: (3)

   open ( 1, file = 'bindingOrder001kl.1', form='formatted', access='direct', recl = 4 )

   write ( 1, "(DT)", iostat = stat, iomsg = msg, rec = 10 ) b1
   write ( 1, "(DT)", iostat = stat, iomsg = msg, rec = 20 ) b2

   read ( 1, "(DT)", iostat = stat, iomsg = msg, rec = 20 )  b1
   read ( 1, "(DT)", iostat = stat, iomsg = msg, rec = 10 )  b2

   if ( ( b2%c /= 'ibm' ) .or. ( b1%c /= 'ftn' ) ) error stop 101_4

   close (1, status = 'delete' )

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 6 changes
