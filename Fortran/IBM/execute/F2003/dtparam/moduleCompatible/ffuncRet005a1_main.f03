! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet005a1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : poly-function-return (function returns
!                               allocatable of a class of abstract type SHAPE)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program ffuncRet005a1
use m1
    type (triangle(4,20)), save :: t1
    class (shape(4,:)), allocatable :: s1

    t1 = triangle(4,20) ((/point(4)(1,1),point(4)(2,2), point(4)(3.0,1)/))

    call printArea(t1%genShape())

    allocate (s1, source=t1)

    call printArea (s1)

    deallocate(s1)
end
