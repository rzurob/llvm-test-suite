!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : access003kl
!*
!*  PROGRAMMER                 : David Forster (derived from access003 by Robert Ma)
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
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic-binding)
!*                               Stmt by Stmt: (pg.58 ln3-6) The default accessibility for the procedure bindings
!*                                             of a type is private if the type definition contains
!*                                             a binding-private-stmt, and public otherwise. The
!*                                             accessibility of a procedure binding may be explicitly
!*                                             declared by an access-spec; otherwise its accessibility
!*                                             is the default for the type definition in which it is
!*                                             declared.
!*
!*                                             - private binding cannot be accessed outside module
!*                                                - Ensure private generic binding can still be invoked inside the module
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

   type :: base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = 'xxx'
      contains
         private
         procedure, pass :: write => writebase
         procedure, pass :: read => readbase
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

         write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowritebase'

      end subroutine

      subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, *, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtioreadbase'

      end subroutine

      subroutine myWrite(dtv)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         write ( 101, * ) dtv
      end subroutine

      subroutine myRead(dtv)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         read ( 101, * ) dtv
      end subroutine

end module

program access003kl
   use m

   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer     :: c1 ! tcx: (:)
   type(base(3))               :: c2 = base(3) ( 'abc' ) ! tcx: (3) ! tcx: (3)

   allocate ( b1, source = base(3) ( 'ibm' ) ) ! tcx: (3)
   allocate ( c1, source = base(3) ( 'ftn' ) ) ! tcx: (3)

   open ( 101, file = 'access003kl.1', form='formatted', access='sequential' )

   call myWrite( b1 )
   call myWrite( c1 )
   call myWrite( c2 )

   rewind 101

   call myRead ( c2 )
   call myRead ( b1 )
   call myRead ( c1 )

   close (101, status = 'delete' )


end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 10 changes
