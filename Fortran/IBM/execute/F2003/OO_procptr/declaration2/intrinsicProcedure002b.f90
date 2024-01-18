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
!                              type declaration specification. Non-poly.
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

program intrinsicProcedure002b
    procedure(real), pointer :: pp1

    pp1 => sin
    print *, nint(pp1(3.1415926 / 2))

    pp1 => cos
    print *, nint(pp1(3.1415926 / 2))
end
