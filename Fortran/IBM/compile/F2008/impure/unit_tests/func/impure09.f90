!*  ============================================================================
!*
!*  DATE                       : 2012-03-08
!*
!*  PRIMARY FUNCTIONS TESTED   : impure procedures
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 917300
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Fortran 2008 support for the IMPURE attribute
!*                               for procedures,which allows for ELEMENTAL procedures
!*                               without the restrictions of PURE.
!*                               C1283 is not apply to IMPURE procedures
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
      module md
        type derived
          integer :: i
          integer,pointer :: pI
        end type

        type,bind(c):: derived2
          integer :: i
        end type

        type derived3
          integer :: i
          integer,pointer :: pI
        end type

        type(derived) dt
      end module

      impure elemental subroutine sub(para)
        use md
        implicit none
        integer,target,intent(in):: para
        integer,pointer ::ppara
        integer,target :: ia
        type(derived2) dt2
        type(derived3) dt3
        common /com_datas/ dt2
        interface iface
          impure subroutine sub2(para)
            integer,intent(inout) :: para
          end
        end interface

        ! below stmts are allowed in impure procedures
        dt2%i = para
        ppara => para
        dt%pI => ia
        dt%i = para
        ppara => dt%pI
        dt3 = derived3(dt%i,dt%pI)
        call sub2(dt%i)
      end

      impure subroutine sub2(para)
        implicit none
        integer,intent(inout) :: para
      end
