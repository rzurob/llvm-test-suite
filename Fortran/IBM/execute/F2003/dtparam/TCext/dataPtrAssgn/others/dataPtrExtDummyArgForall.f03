! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/others/dataPtrExtDummyArgForall.f
! opt variations: -qnol -qnodeferredlp

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!*  pointer is dummy argument of external subroutine
!*  pointer is used in forall statement
!*  pointer is redefined by assigning values in the same row to element in same column
!*	   in forall statement
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        module m
                type parent(n1,k1)    ! (20,4)
                    integer, kind :: k1
                    integer, len  :: n1
                    integer(k1)   :: id = 8
                end type
                type, extends(parent) :: child    ! (20,4)
                end type

                interface
                    subroutine sub(ptr)
                        import parent
                        type(parent(:,4)), pointer :: ptr(:,:)
                    end subroutine
                end interface
        end module

        program main
                use m
                class(parent(:,4)), allocatable, target :: tar(:)
                type(parent(:,4)), pointer :: ptr(:,:)

                allocate(child(20,4) :: tar(20))

                if ( .not. allocated(tar)) error stop 17

                select type(tar)
                    type is (child(*,4))
                        tar(20:1:-1) = (/(child(20,4)(i),i=1,20) /)
                    class default
                        stop 21
                end select

                ptr(2:4,-2:0) => tar(:19)

                call sub(ptr)

                if ( any(lbound(ptr) .ne. (/1,1/)) ) error stop 31
                if ( any(ubound(ptr) .ne. (/3,3/)) ) error stop 33

                print *, ptr%id
        End program

        subroutine sub(ptr)
                use m , only : parent
                type(parent(:,4)), pointer :: ptr(:,:)
                type(parent(20,4)) :: temp(3,3)

                if ( .not. associated(ptr)) error stop 37
                if ( any(lbound(ptr) .ne. (/2,-2/)) ) error stop 31
                if ( any(ubound(ptr) .ne. (/4,0/)) ) error stop 33
                if (ptr%n1 /= 20) error stop 35

                ptr(1:,1:) => ptr

                temp = ptr
                forall (i=1:3)
!                    ptr(:,i) = ptr(i,:)
                    ptr(:,i) = temp(i,:)
                end forall

        end subroutine
