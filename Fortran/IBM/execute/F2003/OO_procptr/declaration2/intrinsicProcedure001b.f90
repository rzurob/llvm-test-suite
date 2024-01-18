!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Declare a procedure pointer using
!                              procedure declaration statement.
!
!                              Specify procedure interface using
!                              interface-name. Non-poly. Basic test.
!                              Same procedure pointer points to
!                              different intrinsics as long as they
!                              have equal interface.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program intrinsicProcedure001b
!  TC changed due to C1215

!   procedure(sin), pointer :: pp1

    INTERFACE
      FUNCTION IFunc(Arg)
        REAL :: IFunc
        REAL, INTENT(IN) :: Arg
      END FUNCTION
    END INTERFACE

    PROCEDURE(IFunc), POINTER :: pp1


    pp1 => sin
    print *, nint(pp1(3.1415926 / 2))

    pp1 => cos
    print *, nint(pp1(3.1415926 / 2))
end
