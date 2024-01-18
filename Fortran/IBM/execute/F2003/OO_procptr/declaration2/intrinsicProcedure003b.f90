!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Declare a procedure pointer using
!                              procedure declaration statement.
!
!                              Do not specify procedure interface.
!                              Non-poly. The same procedure pointer
!                              points to different intrinsics.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program intrinsicProcedure003b
    procedure(), pointer :: pp1

    pp1 => sin
    print *, nint(pp1(3.1415926 / 2))

    pp1 => cos
    print *, nint(pp1(3.1415926 / 2))
end
