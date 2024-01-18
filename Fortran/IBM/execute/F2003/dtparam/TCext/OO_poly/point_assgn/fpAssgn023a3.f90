! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn023a3.f
! opt variations: -qnol -qdeferredlp -qreuse=none

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn023a3.f
! %VERIFY: fpAssgn023a3.out:fpAssgn023a3.vf
! %STDIN:
! %STDOUT: fpAssgn023a3.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (unlimited poly-pointer
!                               array' finalization when deallocating the data
!                               pointer; use pointer assignment for the
!                               association; rank-one array)
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

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id
    end type

     type, extends (base) :: child    ! (20,4)
        integer(k1), pointer :: value => null()

        contains

        final :: finalizeChild
     end type

     class (base(20,4)), pointer :: b_ptr (:)
     type (child(20,4)), pointer :: c_ptr (:)

     contains

     subroutine finalizeChild (c)
        type (child(*,4)), intent(inout) :: c(:)

        do i = 1, size (c)
            if (associated (c(i)%value)) then
                print *, 'deallocating c:', i

                deallocate (c(i)%value)
            end if
        end do
     end subroutine
end module

program fpAssgn023a3
use m
    class (*), pointer :: x(:)
    integer*4, pointer :: v1, v2

    type (child(20,4)) :: c1(2)

    allocate (v1, v2)

    c1 = (/child(20,4) (1, v1), child(20,4)(2, v2)/)

    allocate (b_ptr (2:3), source=c1)
    allocate (c_ptr (3))

    allocate (c_ptr(3)%value)

    x => b_ptr

    deallocate (x)

    x => c_ptr

    deallocate (x)
end
