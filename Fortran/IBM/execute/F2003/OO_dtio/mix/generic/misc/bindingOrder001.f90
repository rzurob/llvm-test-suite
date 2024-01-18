!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Miscellaneous:
!*                                 - reverse the order of declaration between generic and specific binding
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
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
         procedure, pass :: write => writebase
         procedure, pass :: read => readbase
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

      end subroutine

      subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, *, iostat=iostat, iomsg=iomsg) dtv%c
      end subroutine

end module

program bindingOrder001
   use m

   class(base), allocatable :: b1
   class(base), pointer     :: b2

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = base ( 'ibm' ) )
   allocate ( b2, source = base ( 'ftn' ) )

   open ( 1, file = 'bindingOrder001.1', form='formatted', access='direct', recl = 4 )

   write ( 1, "(DT)", iostat = stat, iomsg = msg, rec = 10 ) b1
   write ( 1, "(DT)", iostat = stat, iomsg = msg, rec = 20 ) b2

   read ( 1, "(DT)", iostat = stat, iomsg = msg, rec = 20 )  b1
   read ( 1, "(DT)", iostat = stat, iomsg = msg, rec = 10 )  b2

   if ( ( b2%c /= 'ibm' ) .or. ( b1%c /= 'ftn' ) ) error stop 1_4

   close (1, status = 'delete' )

end program
