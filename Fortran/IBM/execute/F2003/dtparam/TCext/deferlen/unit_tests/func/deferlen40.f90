! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp /tstdev/F2003/deferlen/unit_tests/func/deferlen40.f
! opt variations: -qnock -qnol -qnodeferredlp

!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Testing the ASSOCIATE related
!*                               with characters with deferred length
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      id
    end type

    type, extends(base) :: child    ! (20,4)
        character(:), allocatable :: name
    end type

    type (child(:,4)), pointer:: b1
    type (child(20,4)), target :: c1

    c1 = child(20,4) (10, name = 'Thomas')

    b1 => c1
    associate (iitem => b1%name)
       if (iitem /= 'Thomas') error stop 1
    end associate

    end
