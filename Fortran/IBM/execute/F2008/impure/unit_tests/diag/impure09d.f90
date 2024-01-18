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
!*                               for procedures, C737
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
      impure elemental logical function logical_func(para)
        implicit none
        logical, intent(in) :: para
        logical_func = para
      end

      program t_main
        implicit none
        integer a(5),i
        logical logical_arr(5)
        interface iface
        impure elemental logical function logical_func(para)
          implicit none
          logical, intent(in):: para
        end
        end interface

        forall(i=1:5,logical_func(logical_arr))
          a(i) = i
        end forall
      end
