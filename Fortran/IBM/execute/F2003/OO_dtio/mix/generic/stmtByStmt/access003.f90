!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 04/26/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
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

   type :: base
      character(3) :: c = 'xxx'
      contains
         private
         procedure, pass :: write => writebase
         procedure, pass :: read => readbase
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   contains

      subroutine writebase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowritebase'

      end subroutine

      subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, *, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtioreadbase'

      end subroutine

      subroutine myWrite(dtv)
         class(base), intent(in) :: dtv
         write ( 101, * ) dtv
      end subroutine

      subroutine myRead(dtv)
         class(base), intent(inout) :: dtv
         read ( 101, * ) dtv
      end subroutine

end module

program access003
   use m

   class(base), allocatable :: b1
   class(base), pointer     :: c1
   type(base)               :: c2 = base ( 'abc' )

   allocate ( b1, source = base ( 'ibm' ) )
   allocate ( c1, source = base ( 'ftn' ) )

   open ( 101, file = 'access003.1', form='formatted', access='sequential' )

   call myWrite( b1 )
   call myWrite( c1 )
   call myWrite( c2 )

   rewind 101

   call myRead ( c2 )
   call myRead ( b1 )
   call myRead ( c1 )

   close (101, status = 'delete' )


end program
