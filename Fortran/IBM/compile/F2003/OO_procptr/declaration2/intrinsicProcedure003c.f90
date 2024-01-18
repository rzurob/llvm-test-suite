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

program intrinsicProcedure003c
    procedure(), pointer :: ipp1

    ipp1 => mod
    print *, ipp1(8, -5)

    ipp1 => modulo
    print *, ipp1(-8, -5)
end
