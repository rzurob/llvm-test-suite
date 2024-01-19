!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Final Subroutine
!*                                    -  Ensure DTIO can be invoked during final subroutine
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
         procedure, pass :: write => writeb
         generic :: write(formatted) => write
         final :: finalbase
   end type

   type, extends(base) :: child
      integer(4) :: i = -999
      contains
         procedure, pass :: write => writec
         final :: finalchild, finalchildrank1
   end type

   contains

      subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt

         write (fmt, *) "(A", v_list(1), ")"
         write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt

         write (fmt, *) "(A", v_list(1), ",1X, I",v_list(2),")"

         write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine finalbase(dtv)
         type(base), intent(inout) :: dtv
         integer :: stat
         character(200) :: msg

         write ( 1, "(DT(3))", iostat = stat, iomsg = msg )   dtv
         if ( ( stat /= 0 ) .or. (  msg /= 'dtiowriteb' ) )   error stop 1_4

      end subroutine

      subroutine finalchild(dtv)
         type(child), intent(inout) :: dtv
         integer :: stat
         character(200) :: msg

         write ( 1, "(DT(3,5))", iostat = stat, iomsg = msg )   dtv
         if ( ( stat /= 0 ) .or. (  msg /= 'dtiowritec' ) )     error stop 2_4

      end subroutine

      subroutine finalchildrank1(dtv)
         type(child), intent(inout) :: dtv(:)
         integer :: stat
         character(200) :: msg

         write ( 1, "(DT(3,5))", iostat = stat, iomsg = msg )   dtv
         if ( ( stat /= 0 ) .or. (  msg /= 'dtiowritec' ) )     error stop 3_4

      end subroutine

end module

program final001
   use m

   integer :: stat
   character(200) :: msg

   type(base), allocatable :: b1
   type(child) :: c1
   class(child), allocatable :: c2(:)

   open ( 1, file = 'final001.1', form='formatted', access='sequential' )

   allocate ( b1 )
   deallocate ( b1 )       !<- finalize b1 (base('xxx'))
   c1 = child('abc',1001)  !<- finalize c1 (child('xxx',-999)) and child('abc',1001) and base('abc')

   allocate ( c2(3) )
   deallocate ( c2 )       !<- finalize c2 ((/child('xxx',-999), child('xxx',-999), child('xxx',-999)/)) and 3 base('xxx')

end program
