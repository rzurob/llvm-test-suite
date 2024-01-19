!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Declare a procedure pointer using
!                              procedure declaration statement.
!
!                              Specify procedure interface using
!                              type declaration specification. Non-poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program intrinsicProcedure002a
    procedure(real), pointer :: pp1
   !procedure(real), pointer :: pp2

    pp1 => sin
   !pp2 => log10

    print *, nint(pp1(3.1415926 / 2))
   !print *, nint(pp2(10.0))
end
