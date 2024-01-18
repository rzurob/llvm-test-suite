!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Declare a procedure pointer using
!                              procedure declaration statement.
!
!                              Specify procedure interface using
!                              interface-name. Non-poly. Basic test.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program intrinsicProcedure001a
!  TC changed due to C1215

!   procedure(sin), pointer :: pp1
!   procedure(cos), pointer :: pp2

    INTERFACE
      FUNCTION IFunc(Arg)
        REAL :: IFunc
        REAL, INTENT(IN) :: Arg
      END FUNCTION
    END INTERFACE
  
    PROCEDURE(IFunc), POINTER :: pp1
    PROCEDURE(IFunc), POINTER :: pp2
 
    pp1 => sin
    pp2 => cos

    print *, nint(pp1(3.1415926 / 2))
    print *, nint(pp2(3.1415926 / 2))
end
