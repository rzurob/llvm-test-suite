!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Miscellaneous:
!*                                 - Invoke specific type bound that is associated with generic binding
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
      character(3) :: c = 'xxx'
      contains
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

end module

program specificBindingCall001
   use m

   class(base), allocatable :: b1
   class(base), pointer     :: c1
   type(base)               :: c2 = base ( 'abc' )
   integer :: stat
   character(200) :: msg

   allocate ( b1, source = base ( 'ibm' ) )
   allocate ( c1, source = base ( 'ftn' ) )

   open ( 101, file = 'specificBindingCall001.1', form='formatted', access='sequential' )

   call b1%write( 101, '', (/1/), stat, msg )
   call c1%write( 101, '', (/4/), stat, msg )
   call c2%write( 101, '', (/7/), stat, msg )

   rewind 101

   call c2%read( 101, '', (/0/), stat, msg )
   call b1%read( 101, '', (/0/), stat, msg )
   call c1%read( 101, '', (/0/), stat, msg )

   if ( ( c2%c /= 'ibm' ) .or. ( b1%c /= 'ftn' ) .or. ( c1%c /= 'abc' ) ) error stop 1_4
   close (101, status = 'delete' )
end program
