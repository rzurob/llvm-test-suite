! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=self /tstdev/F2003/iosign/unit_tests/sign13.f
! opt variations: -ql -qreuse=none

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
   type base(d1,d2,d3,d4)    ! (1,4,8,16)
      integer, kind :: d1,d2,d3,d4
      integer(d1)   :: i1
      integer(d2)   :: i4
      integer(d3)   :: i8
      real(d2)      :: r4
      real(d3)      :: r8
      real(d4)      :: r16
      complex(d2)   :: c4
      complex(d3)   :: c8
      complex(d4)   :: c16
   end type

   type (base(1,4,8,16)) b
   b = base(1,4,8,16)(10, 20, 30, 1.2, 2.3, 3.4, (1.1,2.2),(3.3, 4.4),(5.5, 6.6))

   write (*,10,sign='plus') b

   write (*,10, sign='suppress') base(1,4,8,16)(10, 20, 30, 1.2, 2.3, 3.4, (1.1,2.2),(3.3, 4.4),(5.5, 6.6))

   write (*,10 ,sign='processor_defined') base(1,4,8,16)(10, 20, 30, 1.2, 2.3, 3.4, (1.1,2.2),(3.3, 4.4),(5.5, 6.6))

   10 format (3I5, " ",9F5.1)
      end
