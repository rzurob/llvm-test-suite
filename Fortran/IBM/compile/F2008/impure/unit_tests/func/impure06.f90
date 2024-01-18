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
!*                               C1284 is not apply to IMPURE procedures
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789
      impure elemental subroutine sub(para)
        implicit none
        logical,intent(in):: para
        interface operator (.op.)
          impure elemental integer function ideterminant (left,right)
            integer,intent(in)::left,right
          end function
        end interface

        integer arra(5),arrb(5)
        arra = arra .op. arrb          ! impure procedure reference(via defined operation) is allowed
      end subroutine

      impure elemental logical function logical_func(para)
        implicit none
        logical,intent(in):: para
        logical logical_arr(5)

        interface assignment(=)
          subroutine bit_to_numeric (n,b)
           logical, intent(out) :: n
           logical, intent(in), dimension(:) :: b
          end subroutine
        end interface

        logical_func = logical_arr     ! impure procedure reference(via defined assignment) is allowed
      end function
