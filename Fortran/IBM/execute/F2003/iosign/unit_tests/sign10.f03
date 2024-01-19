!**********************************************************************
!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : SIGN= specifier
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : SIGN
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  DESCRIPTION                : Testing sign= specifier in formatted
!*                               write statement in dtio
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
module signdtio
   type base
      real :: r4
      real(8) :: r8
      real(16) :: r16
      integer(1):: i1
      integer:: i4
      integer(8)  :: i8
      complex :: c4
      complex(8) :: c8
      complex(16) :: c16
   end type

   interface write(formatted)
      subroutine dtio_write(dtv, unit, iotype, v_list, iostat, iomsg)
      import base
         class(base), intent(in) ::  dtv
         integer, intent(in) ::  unit
         character(*), intent(in) ::  iotype
         character(*), intent(inout) ::  iomsg
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
      end subroutine
   end interface
end module

use signdtio

write (*,*) base(1.23, 2.34, 3.45, 100, 200, 300, (1.11,2.22),(3.33, 4.44),(5.55, 6.66))

write (*,*,sign='plus') base(1.23, 2.34, 3.45, 100, 200, 300, (1.11,2.22),(3.33, 4.44),(5.55, 6.66))

write (*,*,sign='suppress') base(1.23, 2.34, 3.45, 100, 200, 300, (1.11,2.22),(3.33, 4.44),(5.55, 6.66))

write (*,*,sign='processor_defined') base(1.23, 2.34, 3.45, 100, 200, 300, (1.11,2.22),(3.33, 4.44),(5.55, 6.66))

end

subroutine dtio_write(dtv, unit, iotype, v_list, iostat, iomsg)
use signdtio, only: base
   class(base), intent(in) ::  dtv
   integer, intent(in) ::  unit
   character(*), intent(in) ::  iotype
   character(*), intent(inout) ::  iomsg
   integer, intent(in) :: v_list(:)
   integer, intent(out) :: iostat

   write ( unit, '(f7.2)' )  dtv%r4, dtv%r8, dtv%r16,dtv%c4, dtv%c8, dtv%c16
   write ( unit, '(I5)' )  dtv%i1, dtv%i4, dtv%i8

   write ( unit, '(f7.2)', sign='plus' )  dtv%r4, dtv%r8, dtv%r16,dtv%c4, dtv%c8, dtv%c16
   write ( unit, '(I5)', sign='plus' )  dtv%i1, dtv%i4, dtv%i8

   write ( unit, '(f7.2)', sign='suppress' )  dtv%r4, dtv%r8, dtv%r16,dtv%c4, dtv%c8, dtv%c16
   write ( unit, '(I5)', sign='suppress' )  dtv%i1, dtv%i4, dtv%i8

   write ( unit, '(f7.2)', sign='processor_defined' )  dtv%r4, dtv%r8, dtv%r16,dtv%c4, dtv%c8, dtv%c16
   write ( unit, '(I5)', sign='processor_defined' )  dtv%i1, dtv%i4, dtv%i8

end subroutine

