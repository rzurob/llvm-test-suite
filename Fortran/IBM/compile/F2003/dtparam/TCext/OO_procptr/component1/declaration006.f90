! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/component1/declaration006.f
! opt variations: -qnok -qnol

!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Duplicate the POINTER attribute.
!
!                              This test case is diagnostic.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program declaration006
    type Base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        procedure(integer), pointer, nopass, pointer :: pp1
        procedure(), pointer, nopass, pointer :: pp2
    end type
end
