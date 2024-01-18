!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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
!*                                             - Even specific binding has private attr, generic binding should be able to be called
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
         procedure, private, pass :: read => readbase
         generic, public :: write(formatted) => write
         generic, public :: read(formatted)  => read
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

end module

program access004
   use m

   class(base), allocatable :: b1
   class(base), pointer     :: c1
   type(base)               :: c2 = base ( 'abc' )

   integer :: stat
   character(200) :: msg

   namelist /n1/ b1, c1, c2


   allocate ( b1, source = base ( 'ibm' ) )
   allocate ( c1, source = base ( 'ftn' ) )

   open ( 101, file = 'access004.1', form='formatted', access='sequential' )

   write ( 101, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritebase' ) ) error stop 1_4

   rewind 101

   b1%c = 'xxx'
   c1%c = 'xxx'
   c2%c = 'xxx'

   read ( 101, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadbase' ) ) error stop 2_4
   if ( ( c2%c /= 'abc' ) .or. ( b1%c /= 'ibm' ) .or. ( c1%c /= 'ftn' ) ) error stop 3_4

   close (101, status = 'delete' )


end program
