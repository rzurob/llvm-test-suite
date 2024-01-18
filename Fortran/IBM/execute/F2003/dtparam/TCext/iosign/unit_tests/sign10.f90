! GB DTP extension using:
! ftcx_dtp -ql -qreuse=self /tstdev/F2003/iosign/unit_tests/sign10.f
! opt variations: -qnol -qreuse=none

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
   type base(n1,d1,d2,d3,d4)    ! (20,4,8,16,1)
      integer, kind :: d1,d2,d3,d4
      integer, len  :: n1
      real(d1)      :: r4
      real(d2)      :: r8
      real(d3)      :: r16
      integer(d4)   :: i1
      integer(d1)   :: i4
      integer(d2)   :: i8
      complex(d1)   :: c4
      complex(d2)   :: c8
      complex(d3)   :: c16
   end type

   interface write(formatted)
      subroutine dtio_write(dtv, unit, iotype, v_list, iostat, iomsg)
      import base
         class(base(*,4,8,16,1)), intent(in) ::  dtv
         integer, intent(in) ::  unit
         character(*), intent(in) ::  iotype
         character(*), intent(inout) ::  iomsg
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
      end subroutine
   end interface
end module

use signdtio

write (*,*) base(20,4,8,16,1)(1.23, 2.34, 3.45, 100, 200, 300, (1.11,2.22),(3.33, 4.44),(5.55, 6.66))

write (*,*,sign='plus') base(20,4,8,16,1)(1.23, 2.34, 3.45, 100, 200, 300, (1.11,2.22),(3.33, 4.44),(5.55, 6.66))

write (*,*,sign='suppress') base(20,4,8,16,1)(1.23, 2.34, 3.45, 100, 200, 300, (1.11,2.22),(3.33, 4.44),(5.55, 6.66))

write (*,*,sign='processor_defined') base(20,4,8,16,1)(1.23, 2.34, 3.45, 100, 200, 300, (1.11,2.22),(3.33, 4.44),(5.55, 6.66))

end

subroutine dtio_write(dtv, unit, iotype, v_list, iostat, iomsg)
use signdtio, only: base
   class(base(*,4,8,16,1)), intent(in) ::  dtv
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

