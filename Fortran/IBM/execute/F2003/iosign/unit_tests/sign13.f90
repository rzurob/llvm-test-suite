!**********************************************************************
!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
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
   type base
      integer(1):: i1
      integer:: i4
      integer(8)  :: i8
      real :: r4
      real(8) :: r8
      real(16) :: r16
      complex :: c4
      complex(8) :: c8
      complex(16) :: c16
   end type

   type (base) b
   b = base(10, 20, 30, 1.2, 2.3, 3.4, (1.1,2.2),(3.3, 4.4),(5.5, 6.6))

   write (*,10,sign='plus') b

   write (*,10, sign='suppress') base(10, 20, 30, 1.2, 2.3, 3.4, (1.1,2.2),(3.3, 4.4),(5.5, 6.6))

   write (*,10 ,sign='processor_defined') base(10, 20, 30, 1.2, 2.3, 3.4, (1.1,2.2),(3.3, 4.4),(5.5, 6.6))

   10 format (3I5, " ",9F5.1)
      end
