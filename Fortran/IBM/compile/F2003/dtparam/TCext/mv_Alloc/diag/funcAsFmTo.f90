! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/mv_Alloc/diag/funcAsFmTo.f
! opt variations: -qnok -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*                               If a nonpointer dummy arg of procedure has
!*                               intent(out) or intent(inout) attribute, the
!*                               actual argument shall be definable.
!*                               FROM is intent(inout) argument
!*                               TO is intent(out) argument.
!*                               function return is not definable
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type T(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    class(T(4,:)), allocatable :: TO, FROM

    call move_alloc(func(), TO)

    call move_alloc(FROM, func())

    contains
        function func()
          class(T(4,:)), allocatable :: func
          allocate(T(4,20)::func)
        end function
    end
