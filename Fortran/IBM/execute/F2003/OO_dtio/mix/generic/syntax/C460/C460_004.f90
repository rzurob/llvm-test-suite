!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic-binding)
!*                               Syntax Check: C460 Each binding-name in binding-name-list
!*                                                  shall be the name of a specific binding of the type.
!*
!*                                             - binding is inherited from base type but is overridden by child's binding
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

   type base
      character(3) :: c
      contains
         procedure, pass :: write => writebase
   end type

   type, extends(base) :: child
      integer(4) :: i
      contains
         procedure, pass :: write => writechild
         generic :: write(formatted) => write
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


      subroutine writechild (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,I5)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritechild'

      end subroutine

end module

program C460_004
   use m

   class(child), allocatable :: b1
   class(child), pointer     :: c1
   type(child)               :: c2 = child ( 'abc', 1234 )
   integer :: stat
   character(200) :: msg

   namelist /nml/ b1

   allocate ( b1, source = child ( 'ibm', 2005 ) )
   allocate ( c1, source = child ( 'ftn', 2003 ) )

   open ( 101, file = 'C460_004.1', form='formatted', access='sequential' )

   write ( 101, nml, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritechild' ) ) error stop 1_4
   msg = ''
   write ( 101, *, iostat = stat, iomsg = msg )   c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritechild' ) ) error stop 2_4
   msg = ''
   write ( 101, "(DT)", iostat = stat, iomsg = msg )   c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritechild' ) ) error stop 3_4

end program
